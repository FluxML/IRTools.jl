struct Slot
  id::Symbol
end

Base.show(io::IO, s::Slot) = print(io, "@", s.id)

phislot(b, i) = Slot(Symbol(:phi_, b, :_, i))

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
    # Branches
    for br in BasicBlock(b).branches
      isreturn(br) && continue
      for (i, val) in enumerate(br.args)
        push!(b, :($(phislot(br.block, i)) = $val))
      end
      empty!(br.args)
    end
  end
  return varmap(x -> get(slots, x, x), ir)
end

using Core.Compiler: CodeInfo, SlotNumber

function slots!(ci::CodeInfo)
  ss = Dict{Slot,SlotNumber}()
  for i = 1:length(ci.code)
    ci.code[i] = MacroTools.prewalk(ci.code[i]) do x
      x isa Slot || return x
      haskey(ss, x) && return ss[x]
      push!(ci.slotnames, x.id)
      push!(ci.slotflags, 0x00)
      ss[x] = SlotNumber(length(ci.slotnames))
    end
  end
  return ci
end

function varargs!(meta, ir::IR, n = 0)
  isva = meta.method.isva
  argTs = ir.blocks[1].argtypes
  Ts = widenconst.(argTs[n+1:end])
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

# TODO this is hacky and leaves `ir.defs` incorrect
function splicearg!(meta, ir::IR, name)
  args = arguments(ir)
  push!(ir.defs, (1, -1))
  arg = var(length(ir.defs))
  pushfirst!(args, arg)
  pushfirst!(ir.blocks[1].argtypes, Any)
  pushfirst!(meta.code.slotnames, name)
  return arg
end

function update!(ci::CodeInfo, ir::Core.Compiler.IRCode)
  Core.Compiler.replace_code_newstyle!(ci, ir, length(ir.argtypes)-1)
  ci.inferred = false
  ci.ssavaluetypes = length(ci.code)
  slots!(ci)
  fill!(ci.slotflags, 0)
  return ci
end

update!(meta, ir::IR) = update!(meta, Core.Compiler.IRCode(slots!(ir)))
