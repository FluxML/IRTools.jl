# An implementation of delimited continuations (the shift/reset operators) in
# Julia. Works by transforming all Julia code to continuation passing style.
# The `shift` operator then just has to return the continuation.

# https://en.wikipedia.org/wiki/Delimited_continuation
# https://en.wikipedia.org/wiki/Continuation-passing_style

using IRTools.All

struct Func
  f # Avoid over-specialising on the continuation object.
end

(f::Func)(args...) = f.f(args...)

function captures(ir, vs)
  us = Set()
  for v in vs
    isexpr(ir[v].expr) || continue
    foreach(x -> x isa Variable && push!(us, x), ir[v].expr.args)
  end
  return setdiff(us, vs)
end

rename(env, x) = x
rename(env, x::Variable) = env[x]
rename(env, x::Expr) = Expr(x.head, rename.((env,), x.args)...)
rename(env, x::Statement) = stmt(x, expr = rename(env, x.expr))

excluded = [GlobalRef(Base, :getindex)]

function continuation!(bl, ir, env, vs, ret)
  rename(x) = Main.rename(env, x)
  local v, st
  while true
    isempty(vs) && return return!(bl, rename(Expr(:call, ret, returnvalue(block(ir, 1)))))
    v = popfirst!(vs)
    st = ir[v]
    isexpr(st.expr, :call) && !(st.expr.args[1] ∈ excluded) && break
    isexpr(st.expr, :lambda) &&
      (st = stmt(st, expr = Expr(:lambda, cpslambda(st.expr.args[1]), st.expr.args[2:end]...)))
    env[v] = push!(bl, rename(st))
  end
  cs = [ret, setdiff(captures(ir, vs), [v])...]
  if isempty(vs)
    next = rename(ret)
  else
    next = push!(bl, Expr(:lambda, continuation(ir, vs, cs, v, ret), rename.(cs)...))
    next = xcall(Main, :Func, next)
  end
  ret = push!(bl, stmt(st, expr = xcall(Main, :cps, next, rename(st.expr).args...)))
  return!(bl, ret)
end

function continuation(ir, vs, cs, in, ret)
  bl = empty(ir)
  env = Dict()
  self = argument!(bl)
  env[in] = argument!(bl)
  for (i, c) in enumerate(cs)
    env[c] = pushfirst!(bl, xcall(:getindex, self, i))
  end
  continuation!(bl, ir, env, vs, ret)
end

cpslambda(ir) = cpstransform(ir, true)

function cpstransform(ir, lambda = false)
  lambda || (ir = functional(ir))
  k = argument!(ir, at = lambda ? 2 : 1)
  bl = empty(ir)
  env = Dict()
  for arg in arguments(ir)
    env[arg] = argument!(bl)
  end
  continuation!(bl, ir, env, keys(ir), k)
end

cps(k, f::Core.IntrinsicFunction, args...) = k(f(args...))
cps(k, f::IRTools.Lambda{<:Tuple{typeof(cps),Vararg{Any}}}, args...) = f(k, args...)
cps(k, ::typeof(cond), c, t, f) = c ? cps(k, t) : cps(k, f)
cps(k, ::typeof(cps), args...) = k(cps(args...))

# Speed up compilation
for f in [Broadcast.broadcasted, Broadcast.materialize]
  @eval cps(k, ::typeof($f), args...) = k($f(args...))
end

@dynamo function cps(k, args...)
  ir = IR(args...)
  ir == nothing && return :(args[1](args[2](args[3:end]...)))
  cpstransform(IR(args...))
end

# shift/reset

reset(f) = cps(identity, f)
shift(f) = error("`shift` must be called inside `reset`")
cps(k, ::typeof(shift), f) = f(k)

macro reset(ex)
  :(reset(() -> $(esc(ex))))
end

k = @reset begin
  shift(k -> k)^2
end

k(4) == 16
