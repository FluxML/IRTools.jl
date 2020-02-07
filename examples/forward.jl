# A simple forward-mode AD. Simplified so that many things don't work (e.g.
# control flow), but shows the basic ideas.

using IRTools.All
using IRTools: Pipe
using Base: tail

ntail(x, n) = n <= 0 ? x : xcall(:tail, ntail(x, n-1))

zerolike(x::Number) = zero(x)
zerolike(x::Tuple) = zerolike.(x)
zerolike(x::T) where T =
  NamedTuple{fieldnames(T)}(map(f -> zerolike(getfield(x, f)), fieldnames(T)))
zerolike(x::Union{Module,Type}) = nothing

function instrument!(pr, v, st)
  ex = st.expr
  if isexpr(ex, :new)
    st = stmt(st, expr = xcall(Main, :__new__, ex.args...))
    pr[v] = st
  end
  return st
end

function dual(ir)
  pr = Pipe(ir)
  Δs = Dict()
  partial(x::Variable) = Δs[x]
  partial(v, x::Variable) = Δs[x]
  partial(v, x) = insert!(pr, v, xcall(Main, :zerolike, x))
  dx = argument!(pr, at = 1)
  for (i, x) in enumerate(arguments(ir))
    if i == length(arguments(ir)) && ir.meta.method.isva
      Δs[x] = push!(pr, ntail(dx, i-1))
    else
      Δs[x] = push!(pr, xcall(:getindex, dx, i))
    end
  end
  for (v, st) in pr
    st = instrument!(pr, v, st)
    if isexpr(st.expr, :call)
      dargs = insert!(pr, v, xcall(:tuple, partial.((v,), st.expr.args)...))
      result = insert!(pr, v, stmt(st, expr = xcall(Main, :diff, dargs, st.expr.args...)))
      pr[v] = xcall(:getindex, result, 1)
      Δs[v] = push!(pr, xcall(:getindex, result, 2))
    elseif !isexpr(st.expr)
      Δs[v] = push!(pr, xcall(Main, :zerolike, v))
    else
      error("Unsupported $(st.expr.head) expression")
    end
  end
  ret = returnvalue(block(ir, 1))
  return!(pr, xcall(:tuple, ret, partial(ret)))
  return finish(pr)
end

@dynamo function diff(_, x...)
  ir = IR(x...)
  ir == nothing && return :(error("non-differentiable function $(args[2])"))
  @assert length(blocks(ir)) == 1 "Control flow not yet supported"
  return dual(ir)
end

@generated function __new__(T, args...)
  quote
    Base.@_inline_meta
    $(Expr(:new, :T, [:(args[$i]) for i = 1:length(args)]...))
  end
end

# Julia internal definitions
diff(_, ::typeof(zerolike), x) = zerolike(x), zerolike(x)
diff(_, ::typeof(one), x) = one(x), zerolike(x)
diff(_, ::typeof(println), x...) = println(x...), nothing
diff(_, ::typeof(typeof), x) = typeof(x), nothing
diff(_, ::typeof(Core.apply_type), args...) = Core.apply_type(args...), nothing
diff(ṫ, ::typeof(tuple), t...) = t, tail(ṫ)
diff((_, ṫ, _), ::typeof(getfield), t, i) = getfield(t, i), getfield(ṫ, i)
diff((_, ṫ, _), ::typeof(getindex), t, i) = getindex(t, i), getindex(ṫ, i)
diff(ṡ, ::typeof(__new__), T, s...) = __new__(T, s...), NamedTuple{fieldnames(T)}(tail(tail(ṡ)))

# Mathematical definitions
diff((_, ȧ, ḃ), ::typeof(+), a, b) = a + b, ȧ + ḃ
diff((_, ȧ, ḃ), ::typeof(-), a, b) = a - b, ȧ - ḃ
diff((_, ȧ, ḃ), ::typeof(*), a, b) = a*b, ȧ*b+ḃ*a
diff((_, ȧ), ::typeof(-), a) = -a, -ȧ

diff((_, ẋ), ::typeof(sin), x) = sin(x), ẋ*cos(x)
diff((_, ẋ), ::typeof(cos), x) = cos(x), -ẋ*sin(x)

# Test cases

D(f, x) = diff((zerolike(f), one(x)), f, x)[2]

D(x -> sin(cos(x)), 0.5) # -cos(cos(x))sin(x)

D(x -> D(cos, x), 0.5) # -cos(x)

D(x -> x*D(y -> x*y, 1), 4) # 8
