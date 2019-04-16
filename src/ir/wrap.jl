module Wrap

using MacroTools: isexpr, prewalk
import Core: SSAValue, GotoNode, Compiler
import Core: Typeof
import Core.Compiler: CodeInfo, IRCode, CFG, BasicBlock, Argument, ReturnNode,
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
  unreachable, varmap

function vars(ex)
  _vars(x) = x
  _vars(x::SSAValue) = Variable(x.id)
  _vars(x::Argument) = IRTools.Argument(x.n)
  prewalk(_vars, ex)
end

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

function rewrite_phis!(ir::IR)
  for (v, st) in ir
    ex = st.expr
    ex isa PhiNode || continue
    to, = IRTools.blockidx(ir, v)
    push!(IRTools.basicblock(to).args, v)
    for (from, arg) in zip(ex.edges, ex.values), br in branches_for!(ir, from=>to.id)
      push!(br.args, ex[from])
    end
    delete!(ir, v)
  end
  return ir
end

function IR(ir::IRCode)
  ir2 = IR(ir.linetable, ir.argtypes)
  defs = Dict()
  isempty(ir.new_nodes) || error("IRCode must be compacted")
  for i = 1:length(ir.stmts)
    findfirst(==(i), ir.cfg.index) == nothing || block!(ir2)
    if ir.stmts[i] isa Union{GotoIfNot,GotoNode,ReturnNode}
      push!(ir2.blocks[end].branches, Branch(ir.stmts[i]))
    else
      x = push!(ir2, Statement(vars(ir.stmts[i]), ir.types[i], ir.lines[i]))
      defs[Variable(i)] = x
    end
  end
  ir2 = varmap(x -> defs[x], ir2)
  return rewrite_phis!(ir2)
end

IR(meta::Union{Meta,TypedMeta}) = IR(IRCode(meta))

function unvars(ex)
  _unvars(x) = x
  _unvars(x::Variable) = SSAValue(x.id)
  _unvars(x::IRTools.Argument) = Argument(x.id)
  prewalk(_unvars, ex)
end

function IRCode(ir::IR)
  defs = Dict()
  stmts, types, lines = [], [], Int32[]
  index = Int[]
  for b in IRTools.blocks(ir)
    @assert isempty(IRTools.basicblock(b).args)
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
  succs = IRTools.successors.(IRTools.blocks(ir))
  preds = [filter(j -> i in succs[j], 1:length(succs)) for i = 1:length(succs)]
  bs = BasicBlock.(ranges, preds, succs)
  cfg = CFG(bs, index)
  flags = [0x00 for _ in stmts]
  sps = VERSION > v"1.2-" ? [] : Core.svec()
  IRCode(stmts, types, lines, flags, cfg, ir.lines, ir.args, [], sps)
end

end
