module Wrap

using MacroTools: isexpr, prewalk
import Core: SSAValue, GotoNode
import Core.Compiler: CodeInfo, IRCode, CFG, BasicBlock, ReturnNode,
  just_construct_ssa, compact!, OptimizationState, GotoIfNot, PhiNode, StmtRange

PhiNode(x, y) = PhiNode(Any[x...], Any[y...])

function Base.getindex(phi::PhiNode, b)
  j = findfirst(c -> c == b, phi.edges)
  return phi.values[j]
end

# SSA contruction (forked from Base for untyped code)

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

function IR(ci::CodeInfo, nargs::Integer; meta = nothing)
  bs = blockstarts(ci)
  ir = IR([ci.linetable...], meta = meta)
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
      _rename[Core.SSAValue(i)] = push!(ir, IRTools.stmt(rename(ex), line = ci.codelocs[i]))
    end
  end
  return ir
end

function IR(meta::Union{Meta,TypedMeta}; slots = false, prune = true)
  if slots
    return IR(meta.code, meta.method.nargs, meta = meta)
  elseif meta isa Meta # TODO check this works for meta
    ir = IR(meta.code, meta.nargs, meta = meta) |> IRTools.ssa!
    if prune
      ir = ir |> IRTools.prune! |> IRTools.renumber
    end
    return ir
  else
    return IR(IRCode(meta))
  end
end

function IR(Ts::Type...; slots = false, prune = true)
  m = IRTools.meta(Tuple{Ts...})
  m == nothing && return
  IR(m, slots = slots, prune = prune)
end

end
