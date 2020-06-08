module Wrap

using MacroTools: isexpr, prewalk

using ..Inner, ..IRTools
import ..Inner: IR, Variable, Statement, Branch, BasicBlock, Meta, block!,
  unreachable, varmap, argument!, branch!, return!
import Core: CodeInfo, GotoNode, SSAValue
import Core.Compiler: IRCode, CFG, GotoIfNot, ReturnNode, StmtRange

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
      @assert isempty(BasicBlock(b).args)
    end
    for (v, st) in b
      defs[v] = Variable(length(stmts)+1)
      ex = varmap(x -> get(defs, x, x), st.expr) |> unvars
      push!(stmts, ex)
      push!(types, st.type)
      push!(lines, st.line)
    end
    for br in BasicBlock(b).branches
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
  bs = Core.Compiler.BasicBlock.(ranges, preds, succs)
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

# TODO more informative names, while avoiding clashes.
slotname(ci, s) = Symbol(:_, s.id)

function IR(ci::CodeInfo, nargs::Integer; meta = nothing)
  bs = blockstarts(ci)
  ir = IR(Core.LineInfoNode[ci.linetable...], meta = meta)
  _rename = Dict()
  rename(ex) = prewalk(ex) do x
    haskey(_rename, x) && return _rename[x]
    x isa Core.SlotNumber && return Inner.Slot(slotname(ci, x))
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
    elseif isexpr(ex, :enter)
      _rename[Core.SSAValue(i)] = push!(ir, Expr(:enter, findfirst(==(ex.args[1]), bs)+1))
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

function IR(meta::Meta; slots = false, prune = true)
  ir = IR(meta.code, meta.nargs, meta = meta)
  slots && return ir
  ir = IRTools.ssa!(ir)
  if prune
    ir = ir |> IRTools.prune! |> IRTools.renumber
  end
  return ir
end

function IR(Ts::Type...; slots = false, prune = true)
  m = IRTools.meta(Tuple{Ts...})
  m == nothing && return
  IR(m, slots = slots, prune = prune)
end

end
