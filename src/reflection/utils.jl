struct Slot
  id::Symbol
  type
end

Slot(id) = Slot(id, Any)

function Base.show(io::IO, s::Slot)
  print(io, "@", s.id)
  s.type != Any && print(io, "::", s.type)
end

phislot(b, i) = Slot(Symbol(:phi_, b, :_, i))

sloteq(a, b) = a isa Slot && b isa Slot && a.id === b.id

# Lower a parallel copy `dests .= srcs` (block-argument passing) into a sequence
# of scalar assignments appended to block `b`. Naively emitting `dest_i = src_i`
# in order is wrong when a destination slot is also used as a source (e.g. the
# swap `a, b = b, a` produced by a loop), because an earlier assignment clobbers
# a value a later one still needs. We therefore emit moves in a safe order,
# breaking cycles by spilling a destination to a fresh temporary first.
function copyargs!(b, dests, srcs)
  srcs = collect(Any, srcs)
  todo = [i for i = 1:length(dests) if !sloteq(dests[i], srcs[i])]
  while !isempty(todo)
    k = findfirst(i -> !any(j -> sloteq(srcs[j], dests[i]), todo), todo)
    if k === nothing
      # Only cycles remain; break one by saving a destination to a temporary.
      i = first(todo)
      tmp = Slot(gensym(:copy))
      push!(b, :($tmp = $(dests[i])))
      for j in todo
        sloteq(srcs[j], dests[i]) && (srcs[j] = tmp)
      end
    else
      i = todo[k]
      push!(b, :($(dests[i]) = $(srcs[i])))
      deleteat!(todo, k)
    end
  end
  return b
end

# TODO: handle undef arguments properly.
function slots!(ir::IR)
  slots = Dict()
  for b in blocks(ir)
    # Block arguments
    if b.id != 1
      for (i, var) in enumerate(BasicBlock(b).args)
        slots[var] = phislot(b.id, i)
      end
      empty!(BasicBlock(b).args)
      empty!(BasicBlock(b).argtypes)
    end
    # Catch branches
    for (v, st) in b
      isexpr(st.expr, :catch) || continue
      target = st.expr.args[1]
      args   = st.expr.args[2:end]
      for (i, val) in enumerate(args)
        insert!(b, v, :($(phislot(target, i)) = $val))
      end
      delete!(b, v)
    end
    # Branches
    for br in BasicBlock(b).branches
      isreturn(br) && continue
      dests = [phislot(br.block, i) for i = 1:length(br.args)]
      # Resolve sources to the slots they will become, so that cycles introduced
      # by block-argument permutations (e.g. swaps) can be detected and broken.
      srcs = [get(slots, a, a) for a in br.args]
      copyargs!(b, dests, srcs)
      empty!(br.args)
    end
  end
  return varmap(x -> get(slots, x, x), ir)
end

using Core.Compiler: CodeInfo, SlotNumber

function slots!(ci::CodeInfo)
  ss = Dict{Slot,SlotNumber}()
  for i = 1:length(ci.code)
    function f(x)
      x isa Slot || return x
      haskey(ss, x) && return ss[x]
      push!(ci.slottypes, x.type)
      push!(ci.slotnames, x.id)
      push!(ci.slotflags, 0x00)
      ss[x] = SlotNumber(length(ci.slotnames))
    end
    ci.code[i] = MacroTools.prewalk(ci.code[i]) do x
      x isa Core.ReturnNode ? (isdefined(x,:val) ? Core.ReturnNode(f(x.val)) : x) :
      x isa Core.GotoIfNot ? Core.GotoIfNot(f(x.cond), x.dest) :
      f(x)
    end
  end
  return ci
end

function varargs!(meta, ir::IR, n = 0)
  isva = meta.method.isva
  argTs = ir.blocks[1].argtypes
  Ts = Core.Compiler.widenconst.(argTs[n+1:end])
  typed = !all(T -> T==Any, Ts)
  allTs = !isva ?
    Any[argTs[1:n]..., Tuple{Ts...}] :
    Any[argTs[1:n]..., typed ? Tuple{Ts[1:end-1]...,Ts[end].parameters...} : Any]
  args = copy(ir.blocks[1].args)
  deletearg!(ir, length(allTs)+1:length(args))
  empty!(argTs); append!(argTs, allTs)
  argmap = Dict{Variable,Any}()
  argis = 1:(length(Ts))
  newargs = map(reverse(argis)) do i
    argmap[args[i+n]] = pushfirst!(ir, Statement(nothing, type = Ts[i]))
  end |> reverse
  ir = varmap(x -> get(argmap, x, x), ir)
  for i in argis
    if isva && i == length(argis)
      i = length(Ts)
      xs, T = arguments(ir)[end], argTs[end]
      for _ = 1:i-1
        T = typed ? Tuple{T.parameters[2:end]...} : Any
        st = Statement(xcall(Base, :tail, xs), type = T)
        xs = insertafter!(ir, xs, st)
      end
      ir[newargs[i]] = xcall(:identity, xs)
    else
      ir[newargs[i]] = xcall(Base, :getfield, arguments(ir)[end], i)
    end
  end
  return ir
end

function closureargs!(ir::IR)
  args = arguments(ir)[2:end]
  deletearg!(ir, 2:length(arguments(ir)))
  argtuple = argument!(ir)
  env = Dict()
  for (i, a) in reverse(collect(enumerate(args)))
    env[a] = pushfirst!(ir, xcall(:getindex, argtuple, i))
  end
  prewalk!(x -> get(env, x, x), ir)
  return ir
end

# TODO this is hacky and leaves `ir.defs` incorrect
function splicearg!(ir::IR)
  args = arguments(ir)
  push!(ir.defs, (1, -1))
  arg = var(length(ir.defs))
  pushfirst!(args, arg)
  pushfirst!(ir.blocks[1].argtypes, Any)
  return arg
end

function replace_code_newstyle!(ci, ir, _)
  isnothing(ci.slottypes) && (ci.slottypes = Any[])
  return Core.Compiler.replace_code_newstyle!(ci, ir)
end

function get_staged(mi, world)
  return Core.Compiler.get_staged(mi, world)
end

function update!(ci::CodeInfo, ir::Core.Compiler.IRCode)
  replace_code_newstyle!(ci, ir, length(ir.argtypes))
  @static if VERSION < v"1.12.0-DEV.15"
    ci.inferred = false
  end
  ci.ssavaluetypes = length(ci.code)
  slots!(ci)
  fill!(ci.slotflags, 0)
  return ci
end

function update!(ci::CodeInfo, ir::IR)
  if ir.meta isa Meta
    ci.method_for_inference_limit_heuristics = ir.meta.method
    if isdefined(ci, :edges)
      ci.edges = Core.MethodInstance[ir.meta.instance]
    end
  end
  update!(ci, Core.Compiler.IRCode(slots!(ir)))
end
