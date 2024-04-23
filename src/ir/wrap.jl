module Wrap

using MacroTools: isexpr, prewalk

using ..Inner, ..IRTools
import ..Inner: IR, Variable, Statement, Branch, BasicBlock, Meta, block!,
  unreachable, varmap, argument!, branch!, return!, LineInfoNode
import Core: CodeInfo, GotoNode, SSAValue
import Core.Compiler: IRCode, CFG, GotoIfNot, ReturnNode, StmtRange

@static if VERSION > v"1.6-"
  import Core.Compiler: InstructionStream
end

@static if VERSION ≥ v"1.9.0-DEV.502"
  const LineType = Int32
else
  const LineType = Int
end

@static if VERSION ≥ v"1.12.0-DEV.173"
  addline!(lines, li) = push!(lines, li, Int32(0), Int32(0))
else
  addline!(lines, li) = push!(lines, li)
end

unvars(ex) = prewalk(x -> x isa Variable ? SSAValue(x.id) : x, ex)

function IRCode(ir::IR)
  defs = Dict()
  stmts, types, lines = [], [], Int32[]
  index = Int[]
  for b in IRTools.blocks(ir)
    if b.id == 1
      for (i, arg) in enumerate(IRTools.arguments(b))
        defs[arg] = Core.SlotNumber(i)
      end
    else
      @assert isempty(BasicBlock(b).args)
    end
    for (v, st) in b
      defs[v] = Variable(length(stmts)+1)
      ex = varmap(x -> get(defs, x, x), st.expr) |> unvars
      @static if isdefined(Core.IR, :EnterNode)
        ex = isexpr(ex, :enter) ? Core.EnterNode(ex.args...) : ex
      end
      push!(stmts, ex)
      push!(types, st.type)
      addline!(lines, st.line)
    end
    for br in BasicBlock(b).branches
      if IRTools.isreturn(br)
        x = get(defs, br.args[1], br.args[1]) |> unvars
        push!(stmts, ReturnNode(x))
      elseif br == unreachable
        @static if VERSION >= v"1.10"
          push!(stmts, ReturnNode())
        else
          push!(stmts, Expr(:call, GlobalRef(Core, :throw), "unreachable"))
        end
      elseif br.condition === nothing
        push!(stmts, GotoNode(br.block))
      else
        cond = get(defs, br.condition, br.condition) |> unvars
        push!(stmts, GotoIfNot(cond, br.block))
      end
      push!(types, Any); addline!(lines, Int32(0))
    end
    push!(index, length(stmts)+1)
  end
  ranges = StmtRange.([1, index[1:end-1]...], index.-1)
  succs = map.(x -> x.id, IRTools.successors.(IRTools.blocks(ir)))
  preds = map.(x -> x.id, IRTools.predecessors.(IRTools.blocks(ir)))
  bs = Core.Compiler.BasicBlock.(ranges, preds, succs)
  cfg = CFG(bs, index)

  flags = UInt32[0x00 for _ in stmts]
  sps = VERSION > v"1.2-" ? (VERSION >= v"1.10.0-DEV.552" ? Core.Compiler.VarState[] : []) : Core.svec()

  @static if VERSION > v"1.6-"
    @static if isdefined(Core.Compiler, :CallInfo)
      stmtinfo = Core.Compiler.CallInfo[Core.Compiler.NoCallInfo() for _ in 1:length(stmts)]
    else
      stmtinfo = Any[nothing for _ in 1:length(stmts)]
    end
    stmts = InstructionStream(stmts, types, stmtinfo, lines, flags)
    meta = @static VERSION < v"1.9.0-DEV.472" ? [] : Expr[]
    @static if VERSION ≥ v"1.12.0-DEV.173"
      nlocs = length(types)
      codelocs = fill(Int32(0), 3nlocs)

      if !isempty(ir.lines)
        LI = first(ir.lines)
        topfile, topline = LI.file, LI.line

        for i in 1:nlocs
          lineidx = lines[3i - 2]
          if lineidx == 0
            continue
          end
          # TODO: support inlining, see passes/inline.jl
          @assert LI.file === topfile && LI.inlined_at == 0
          LI = ir.lines[lineidx]
          codelocs[3i - 2] = LI.line
        end
      else
        topline = Int32(1)
      end
      codelocs = @ccall jl_compress_codelocs(topline::Int32, codelocs::Any, nlocs::Csize_t)::Any

      debuginfo = Core.Compiler.DebugInfoStream(lines)
      debuginfo.def = ir.meta isa Meta ? ir.meta.instance : :var"n/a"
      debuginfo.linetable = Core.DebugInfo(debuginfo.def, nothing, Core.svec(), codelocs)
      IRCode(stmts, cfg, debuginfo, ir.blocks[1].argtypes, meta, sps)
    else
      mod, meth = ir.meta isa Meta ? (ir.meta.method.module, ir.meta.method) : (Main, nothing)
      linetable = map(li -> Core.LineInfoNode(mod, meth, li.file, LineType(li.line), LineType(li.inlined_at)),
                      ir.lines)
      IRCode(stmts, cfg, linetable, ir.blocks[1].argtypes, meta, sps)
    end
  else
    IRCode(stmts, types, lines, flags, cfg, ir.lines, ir.blocks[1].argtypes, [], sps)
  end
end

if VERSION >= v"1.6.0-DEV.272"
    isgotoifnot(@nospecialize(ex)) = (@assert !isexpr(ex, :gotoifnot); ex isa Core.GotoIfNot)
    destruct_gotoifnot(@nospecialize(ex)) = Any[ex.cond, ex.dest]

    isreturn(@nospecialize(ex)) = (@assert !isexpr(ex, :return); ex isa ReturnNode)
    retval(@nospecialize(ex)) = ex.val
else
    isgotoifnot(@nospecialize(ex)) = isexpr(ex, :gotoifnot)
    destruct_gotoifnot(@nospecialize(ex)) = ex.args

    isreturn(@nospecialize(ex)) = isexpr(ex, :return)
    retval(@nospecialize(ex)) = ex.args[1]
end

function blockstarts(ci::CodeInfo)
  bs = Int[]
  terminator = false
  for i = 1:length(ci.code)
    ex = ci.code[i]
    if isgotoifnot(ex)
      _, dest = destruct_gotoifnot(ex)
      push!(bs, dest)
      terminator = true
    elseif ex isa GotoNode || isreturn(ex)
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
  codelocs, linetable = @static if VERSION ≥ v"1.12.0-DEV.173"
    def = isnothing(meta) ? :var"n/a" : meta.instance
    N = length(ci.code)
    codelocs = fill(0, N)
    linetable = LineInfoNode[]

    # NOTE: we could be faster about decoding here and support inlining?
    for pc in 1:N
      LI = Base.IRShow.buildLineInfoNode(ci.debuginfo, def, pc)
      if !isempty(LI)
        linode = first(LI) # ::Base.IRShow.LineInfoNode
        push!(linetable, LineInfoNode(linode.file, linode.line))
        codelocs[pc] = length(linetable)
      end
    end

    codelocs, linetable
  else
    ci.codelocs, map(li -> LineInfoNode(li.file, li.line), ci.linetable)
  end
  ir = IR(linetable, meta = meta)
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
    elseif @static if isdefined(Core.IR, :EnterNode); ex isa Core.IR.EnterNode else isexpr(ex, :enter) end
      catch_dest = @static if isdefined(Core.IR, :EnterNode)
          ex.catch_dest
      else
          ex.args[1]
      end
    enter_expr = Expr(:enter, findfirst(==(catch_dest), bs)+1)
      isdefined(ex, :scope) && push!(enter_expr.args, ex.scope)
      _rename[Core.SSAValue(i)] = push!(ir, enter_expr)
    elseif ex isa GotoNode
      branch!(ir, findfirst(==(ex.label), bs)+1)
    elseif isgotoifnot(ex)
      cond, dest = destruct_gotoifnot(ex)
      branch!(ir, findfirst(==(dest), bs)+1,
              unless = rename(cond))
    elseif isreturn(ex)
      return!(ir, rename(retval(ex)))
    else
      _rename[Core.SSAValue(i)] = push!(ir, IRTools.stmt(rename(ex), line = codelocs[i]))
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
