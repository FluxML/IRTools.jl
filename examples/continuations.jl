using IRTools.All

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

function continuation(ir, vs, cs, in, ret)
  bl = empty(ir)
  env = Dict()
  rename(x) = Main.rename(env, x)
  self = argument!(bl)
  env[in] = argument!(bl)
  for (i, c) in enumerate(cs)
    env[c] = pushfirst!(bl, xcall(:getindex, self, i))
  end
  while true
    if isempty(vs)
      return!(bl, rename(Expr(:call, ret, returnvalue(block(ir, 1)))))
      return bl
    elseif isexpr(ir[vs[1]].expr, :call)
      break
    else
      v = popfirst!(vs)
      env[v] = push!(bl, rename(ir[v]))
    end
  end
  v = popfirst!(vs)
  st = ir[v]
  cs = [ret, setdiff(captures(ir, vs), [v])...]
  next = push!(bl, Expr(:lambda, continuation(ir, vs, cs, v, ret), rename.(cs)...))
  ret = push!(bl, stmt(st, expr = xcall(Main, :cps, next, rename(st.expr).args...)))
  return!(bl, ret)
end

function cpstransform(ir)
  ir = functional(ir)
  k = argument!(ir, at = 1)
  bl = empty(ir)
  env = Dict()
  for arg in arguments(ir)
    env[arg] = argument!(bl)
  end
  cs = arguments(ir)
  cont = push!(bl, Expr(:lambda, continuation(ir, keys(ir), cs, nothing, k), rename.((env,), cs)...))
  return!(bl, Expr(:call, cont, nothing))
  return bl
end

cps(k, f::Core.IntrinsicFunction, args...) = k(f(args...))
cps(k, ::typeof(cond), c, t, f) = c ? cps(k, t) : cps(k, f)
cps(k, ::typeof(cps), args...) = k(cps(args...))

@dynamo function cps(k, args...)
  ir = IR(args...)
  ir == nothing && return :(args[1](args[2](args[3:end]...)))
  cpstransform(IR(args...))
end

function pow(x, n)
  r = 1
  while n > 0
    n -= 1
    r *= x
  end
  return r
end

cps(identity, pow, 2, 3)

# shift/reset

reset(f) = cps(identity, f)
shift(f) = error("`shift` must be called inside `reset`")
cps(k, ::typeof(shift), f) = f(k)

macro reset(ex)
  :(reset(() -> $(esc(ex))))
end

k = @reset shift(identity)^2
k(4)
