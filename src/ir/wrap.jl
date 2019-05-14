module Wrap

using MacroTools: isexpr, prewalk
import Core: SSAValue, GotoNode, Compiler
import Core: Typeof
import Core.Compiler: CodeInfo, IRCode, CFG, BasicBlock, ReturnNode,
  just_construct_ssa, compact!, NewNode, InferenceState, OptimizationState,
  GotoIfNot, PhiNode, PiNode, StmtRange, IncrementalCompact, insert_node!, insert_node_here!,
  compact!, finish, DomTree, construct_domtree, dominates, userefs, widenconst, types, verify_ir,
  ssamap

for T in :[IRCode, IncrementalCompact, UseRef, UseRefIterator, TypesView].args
  @eval begin
    Base.getindex(ir::Compiler.$T, a...) = Compiler.getindex(ir, a...)
    Base.setindex!(ir::Compiler.$T, a...) = Compiler.setindex!(ir, a...)
  end
end

for T in :[UseRefIterator, IncrementalCompact, Pair].args
  @eval Base.iterate(x::Compiler.$T, a...) = Compiler.iterate(x, a...)
end

if VERSION > v"1.1.0-DEV.560"
  Base.getindex(r::StmtRange, i) = (r.start:r.stop)[i]
else
  Base.first(r::StmtRange) = r.first
  Base.getindex(r::StmtRange, i) = (r.first:r.last)[i]
end

PhiNode(x, y) = PhiNode(Any[x...], Any[y...])

if VERSION > v"1.1.0-DEV.560"
  CFG(bs) = CFG(bs, map(b -> b.stmts.start, bs[2:end]))
else
  CFG(bs) = CFG(bs, map(b -> b.stmts.first, bs[2:end]))
end

StmtRange(r::UnitRange) = StmtRange(first(r), last(r))

Base.length(phi::PhiNode) = length(phi.edges)

function Base.iterate(phi::PhiNode, i = 1)
  i > length(phi) && return
  phi.edges[i]=>phi.values[i], i+1
end

function Base.getindex(phi::PhiNode, b)
  j = findfirst(c -> c == b, phi.edges)
  return phi.values[j]
end

# SSA contruction (forked from Base for untyped code)

import Core.Compiler: normalize, strip_trailing_junk!, compute_basic_blocks,
  scan_slot_def_use, LineInfoNode, construct_ssa!, IR_FLAG_INBOUNDS

function just_construct_ssa(ci::CodeInfo, code::Vector{Any}, nargs::Int, sp)
  ci.ssavaluetypes = Any[Any for _ = 1:length(code)]
  slottypes = Any[Any for _ = 1:length(ci.slotnames)]
  inbounds_depth = 0 # Number of stacked inbounds
  meta = Any[]
  flags = fill(0x00, length(code))
  for i = 1:length(code)
    stmt = code[i]
    if isexpr(stmt, :inbounds)
      arg1 = stmt.args[1]
      if arg1 === true # push
        inbounds_depth += 1
      elseif arg1 === false # clear
        inbounds_depth = 0
      elseif inbounds_depth > 0 # pop
        inbounds_depth -= 1
      end
      stmt = nothing
    else
      stmt = normalize(stmt, meta)
    end
    code[i] = stmt
    if !(stmt === nothing)
      if inbounds_depth > 0
        flags[i] |= IR_FLAG_INBOUNDS
      end
    end
  end
  strip_trailing_junk!(ci, code, flags)
  cfg = compute_basic_blocks(code)
  defuse_insts = scan_slot_def_use(nargs, ci, code)
  domtree = construct_domtree(cfg)
  ir = let code = Any[nothing for _ = 1:length(code)]
    argtypes = slottypes[1:(nargs+1)]
    IRCode(code, Any[], ci.codelocs, flags, cfg, collect(LineInfoNode, ci.linetable), argtypes, meta, sp)
  end
  ir = construct_ssa!(ci, code, ir, domtree, defuse_insts, nargs, sp, slottypes)
  return ir
end

sparams(opt::OptimizationState) = VERSION > v"1.2-" ? Any[t.val for t in opt.sptypes] : Any[opt.sp...]

using ..IRTools
import ..IRTools: IR, Variable, Statement, Branch, TypedMeta, Meta, block!,
  unreachable, varmap, argument!, branch!, return!

vars(ex) = prewalk(x -> x isa SSAValue ? Variable(x.id) : x, ex)

Branch(x::GotoNode) = Branch(nothing, x.label, [])
Branch(x::GotoIfNot) = Branch(vars(x.cond), x.dest, [])
Branch(x::ReturnNode) = isdefined(x, :val) ? Branch(nothing, 0, [vars(x.val)]) : unreachable

function IRCode(meta::TypedMeta)
  opt = OptimizationState(meta.frame)
  Base.Meta.partially_inline!(meta.code.code, [], meta.method.sig, sparams(opt), 0, 0, :propagate)
  ir = just_construct_ssa(meta.code, copy(meta.code.code),
                          Int(meta.method.nargs)-1, opt)
  resize!(ir.argtypes, meta.method.nargs)
  return compact!(ir)
end

function IRCode(meta::Meta)
  sps = VERSION > v"1.2-" ? Any[meta.sparams...] : meta.sparams
  ir = just_construct_ssa(meta.code, copy(meta.code.code),
                          Int(meta.nargs)-1, sps)
  return compact!(ir)
end

function branches_for!(ir, (from, to))
  brs = []
  for br in ir.blocks[from].branches
    br.block == to && push!(brs, br)
  end
  if isempty(brs)
    br = Branch(nothing, to, [])
    push!(ir.blocks[from].branches, br)
    push!(brs, br)
  end
  return brs
end

function rewrite_phis!(ir::IR, offset)
  for (v, st) in ir
    ex = st.expr
    ex isa PhiNode || continue
    to, = IRTools.blockidx(ir, v)
    bb = IRTools.basicblock(to)
    push!(bb.args, v)
    push!(bb.argtypes, st.type)
    for (from, arg) in zip(ex.edges, ex.values), br in branches_for!(ir, from+offset=>to.id)
      push!(br.args, ex[from])
    end
    delete!(ir, v)
    ir.defs[v.id] = (to.id, -length(bb.args))
  end
  return ir
end

function IR(ir::IRCode)
  isempty(ir.new_nodes) || error("IRCode must be compacted")
  ir2 = IR(ir.linetable)
  defs = Dict()
  args = map(T -> argument!(ir2, T), ir.argtypes)
  offset = !isempty(ir.cfg.blocks[1].preds)
  offset && block!(ir2)
  for i = 1:length(ir.stmts)
    findfirst(==(i), ir.cfg.index) == nothing || block!(ir2)
    if ir.stmts[i] isa Union{GotoIfNot,GotoNode,ReturnNode}
      push!(ir2.blocks[end].branches, Branch(ir.stmts[i]))
    else
      x = push!(ir2, Statement(vars(ir.stmts[i]), ir.types[i], ir.lines[i]))
      defs[Variable(i)] = x
    end
  end
  ir2 = prewalk(x -> x isa Variable ? defs[x] :
                     x isa Core.Compiler.Argument ? Variable(x.n) :
                     x, ir2)
  rewrite_phis!(ir2, offset)
  return ir2 |> IRTools.renumber
end

unvars(ex) = prewalk(x -> x isa Variable ? SSAValue(x.id) : x, ex)

function IRCode(ir::IR)
  defs = Dict()
  stmts, types, lines = [], [], Int32[]
  index = Int[]
  for b in IRTools.blocks(ir)
    if b.id == 1
      for (i, arg) in enumerate(IRTools.arguments(b))
        defs[arg] = Core.Compiler.Argument(i)
      end
    else
      @assert isempty(IRTools.basicblock(b).args)
    end
    for (v, st) in b
      defs[v] = Variable(length(stmts)+1)
      ex = varmap(x -> get(defs, x, x), st.expr) |> unvars
      push!(stmts, ex)
      push!(types, st.type)
      push!(lines, st.line)
    end
    for br in IRTools.basicblock(b).branches
      if IRTools.isreturn(br)
        x = get(defs, br.args[1], br.args[1]) |> unvars
        push!(stmts, ReturnNode(x))
      elseif br == unreachable
        push!(stmts, ReturnNode())
      elseif br.condition == nothing
        push!(stmts, GotoNode(br.block))
      else
        cond = get(defs, br.condition, br.condition) |> unvars
        push!(stmts, GotoIfNot(cond, br.block))
      end
      push!(types, Any); push!(lines, 0)
    end
    push!(index, length(stmts)+1)
  end
  ranges = StmtRange.([1, index[1:end-1]...], index.-1)
  succs = map.(x -> x.id, IRTools.successors.(IRTools.blocks(ir)))
  preds = map.(x -> x.id, IRTools.predecessors.(IRTools.blocks(ir)))
  bs = BasicBlock.(ranges, preds, succs)
  cfg = CFG(bs, index)
  flags = [0x00 for _ in stmts]
  sps = VERSION > v"1.2-" ? [] : Core.svec()
  IRCode(stmts, types, lines, flags, cfg, ir.lines, ir.blocks[1].argtypes, [], sps)
end

function blockstarts(ci::CodeInfo)
  bs = Int[]
  terminator = false
  for i = 1:length(ci.code)
    ex = ci.code[i]
    if isexpr(ex, :gotoifnot)
      push!(bs, ex.args[2])
      terminator = true
    elseif isexpr(ex, GotoNode, :return)
      ex isa GotoNode && push!(bs, ex.label)
      i < length(ci.code) && push!(bs, i+1)
      terminator = false
    elseif terminator
      push!(bs, i)
      terminator = false
    end
  end
  return sort(unique(bs))
end

function IR(ci::CodeInfo, nargs::Integer)
  bs = blockstarts(ci)
  ir = IR([ci.linetable...])
  _rename = Dict()
  rename(ex) = prewalk(ex) do x
    haskey(_rename, x) && return _rename[x]
    x isa Core.SlotNumber && return IRTools.Slot(ci.slotnames[x.id])
    return x
  end
  for i = 1:nargs
    _rename[Core.SlotNumber(i)] = argument!(ir)
  end
  for i = 1:length(ci.code)
    i in bs && block!(ir)
    ex = ci.code[i]
    if ex isa Core.NewvarNode
      continue
    elseif isexpr(ex, GotoNode)
      branch!(ir, findfirst(==(ex.label), bs)+1)
    elseif isexpr(ex, :gotoifnot)
      branch!(ir, findfirst(==(ex.args[2]), bs)+1,
              unless = rename(ex.args[1]))
    elseif isexpr(ex, :return)
      return!(ir, rename(ex.args[1]))
    else
      _rename[Core.SSAValue(i)] = push!(ir, rename(ex))
    end
  end
  return ir
end

function IR(meta::Union{Meta,TypedMeta}; slots = false)
  if slots
    IR(meta.code, meta.method.nargs)
  else
    IR(IRCode(meta))
  end
end

end
