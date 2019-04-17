struct Slot
  id::Symbol
end

Base.show(io::IO, s::Slot) = print(io, "@", s.id)

spatslot(b, i) = Slot(Symbol(:spat_, b, :_, i))

function slots!(ir::IR)
  slots = Dict()
  for b in blocks(ir)
    # Block arguments
    for (i, var) in enumerate(basicblock(b).args)
      slots[var] = spatslot(b.id, i)
    end
    empty!(basicblock(b).args)
    # Branches
    for br in basicblock(b).branches
      isreturn(br) && continue
      for (i, val) in enumerate(br.args)
        push!(b, :($(spatslot(br.block, i)) = $val))
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

struct NewArg
  id::Int
end

function varargs!(meta, ir::IR, n = 0)
  isva = meta.method.isva
  Ts = widenconst.(ir.args[n+1:end])
  args = !isva ?
    Any[ir.args[1:n]..., Tuple{Ts...}] :
    Any[ir.args[1:n]..., Tuple{Ts[1:end-1]...,Ts[end].parameters...}]
  empty!(ir.args); append!(ir.args, args)
  argmap = Dict{Argument,Any}()
  if isva
    i = length(Ts)
    xs, T = NewArg(n+1), args[end]
    for _ = 1:i-1
      T = Tuple{T.parameters[2:end]...}
      st = Statement(xcall(Base, :tail, xs), type = T)
      xs = xs isa NewArg ? pushfirst!(ir, st) : insertafter!(ir, xs, st)
    end
    argmap[Argument(i+n)] = xs
  end
  for i = (length(Ts)-isva):-1:1
    arg = xcall(Base, :getfield, NewArg(n+1), i)
    argmap[Argument(i+n)] = pushfirst!(ir, Statement(arg, type = Ts[i]))
  end
  unnew(x::NewArg) = Argument(x.id); unnew(x) = x
  map(ir) do x
    prewalk(x -> unnew(x isa Argument ? get(argmap, x, x) : x), x)
  end
end

function spliceargs!(meta, ir::IR, args...)
  ir = argmap(x -> Argument(x.id + length(args)), ir)
  for (name, T) in reverse(args)
    pushfirst!(ir.args, T)
    pushfirst!(meta.code.slotnames, name)
  end
  return ir
end

function update!(meta, ir::Core.Compiler.IRCode)
  Core.Compiler.replace_code_newstyle!(meta.code, ir, length(ir.argtypes)-1)
  meta.code.inferred = false
  meta.code.ssavaluetypes = length(meta.code.code)
  slots!(meta.code)
  return meta.code
end

update!(meta, ir::IR) = update!(meta, Core.Compiler.IRCode(slots!(ir)))
