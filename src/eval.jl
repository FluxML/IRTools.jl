using Base: invokelatest

dummy() = return
const dummy_m = which(dummy, Tuple{})

function build_codeinfo(ir::IR)
  ir = copy(ir)
  ci = Base.uncompressed_ir(dummy_m)
  ci.inlineable = true
  for arg in arguments(ir)
    isnothing(ci.slottypes) && (ci.slottypes = Any[])
    push!(ci.slottypes, Type)
    push!(ci.slotnames, Symbol(""))
    push!(ci.slotflags, 0)
  end
  if isdefined(Base, :__has_internal_change) && Base.__has_internal_change(v"1.12-alpha", :codeinfonargs)
    ci.nargs = length(arguments(ir)) + 1
    ci.isva = false
  end
  argument!(ir, at = 1)
  prewalk!(x -> x === self ? arguments(ir)[1] : x, ir)
  update!(ci, ir)
end

struct Closure{F,T}
  f::F
  data::T
end

closure(f, data...) = Closure(f, data)

@inline Base.getindex(c::Closure, i::Integer) = c.data[i]
(c::Closure)(args...) = c.f(c, args...)

Base.show(io::IO, c::Closure) = print(io, "λ")

# Compile each `:lambda` statement into its own function, wrapped in a `Closure`
# carrying the captured values (and the enclosing `self`).
function compilelambdas(m::Module, ir::IR, _self = self)
  ir = copy(ir)
  for (v, st) in ir
    isexpr(st.expr, :lambda) || continue
    λ, caps = copy(st.expr.args[1]), st.expr.args[2:end]
    push!(caps, _self)
    s = pushfirst!(λ, xcall(:getindex, arguments(λ)[1], length(caps)))
    ir[v] = Expr(:call, closure, func(m, compilelambdas(m, λ, s)), caps...)
  end
  prewalk!(x -> x === self ? _self : x, ir)
  return ir
end

# JuliaLang/julia#48611: world age is exposed to generated functions.
function func(m::Module, ir::IR)
  ir = compilelambdas(m, ir)
  generator = @eval m begin
    function $(gensym())(world::UInt, source, self,
                         $([Symbol(:arg, i) for i = 1:length(arguments(ir))]...))
      return $build_codeinfo($ir)
    end
  end
  @eval m begin
    function $(gensym())($([Symbol(:arg, i) for i = 1:length(arguments(ir))]...))
      $(Expr(:meta, :generated, generator))
      $(Expr(:meta, :generated_only))
    end
  end
end

func(ir::IR) = func(Main, ir)

evalir(m::Module, ir::IR, args...) = invokelatest(func(m, ir), args...)
evalir(ir::IR, args...) = evalir(Main, ir, args...)
