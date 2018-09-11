struct Slot
  id::Symbol
end

Base.show(io::IO, s::Slot) = print(io, "@", s.id)

function slots!(ir::IR)
  n = 0
  amap = Dict()
  for (x, st) in ir
    st.expr isa PhiNode || continue
    slot = ir[x] = Slot(Symbol(:phi, n += 1))
    for (p, y) in st.expr
      if y isa SSAValue
        insertafter!(ir, y, :($slot = $y))
        amap[y] = slot
      else
        push!(block(ir, p), :($slot = $y))
      end
    end
  end
  map(ir) do x
    isexpr(x, :(=)) && return x
    prewalk(x -> get(amap, x, x), x)
  end
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
  n::Int
end

function varargs!(meta, ir::IR, n = 1)
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
  unnew(x::NewArg) = Argument(x.n); unnew(x) = x
  map(ir) do x
    prewalk(x -> unnew(x isa Argument ? get(argmap, x, x) : x), x)
  end
end

function spliceargs!(meta, ir::IR, args...)
  ir = argmap(x -> Argument(x.n + length(args)), ir)
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

# Test / example function
@generated function roundtrip(f, args...)
  m = meta(Tuple{f,args...})
  ir = IR(m)
  ir = varargs!(m, ir)
  argnames!(m, :f, :args)
  ir = spliceargs!(m, ir, (Symbol("#self#"), typeof(roundtrip)))
  return update!(m, ir)
end

Base.isconst(g::GlobalRef) = isconst(g.mod, g.name)
Base.getindex(g::GlobalRef) = getfield(g.mod, g.name)

_typeof(x) = typeof(x)
_typeof(x::GlobalRef) = isconst(x) ? typeof(x[]) : Any

isprimitive(x) =
  _typeof(x) <: Union{Core.IntrinsicFunction,Core.Builtin}

isprimitive(ir, f) = isprimitive(f)
isprimitive(ir, f::SSAValue) = isprimitive(ir[f].expr)

@generated function passthrough(f, args...)
  m = meta(Tuple{f,args...})
  m == nothing && return :(f(args...))
  ir = IR(m)
  for (x, st) in ir
    (isexpr(st.expr, :call) && !isprimitive(ir, st.expr.args[1])) || continue
    ir[x] = xcall(IRTools, :passthrough, st.expr.args...)
  end
  ir = varargs!(m, ir)
  argnames!(m, :f, :args)
  ir = spliceargs!(m, ir, (Symbol("#self#"), typeof(passthrough)))
  return update!(m, ir)
end
