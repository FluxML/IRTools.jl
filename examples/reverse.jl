using IRTools
using IRTools: @dynamo, IR, Pipe, finish, substitute, return!, block, blocks,
  returnvalue, arguments, isexpr, xcall, self, stmt

struct Pullback{S,T}
  data::T
end

Pullback{S}(data) where S = Pullback{S,typeof(data)}(data)

function primal(ir, T = Any)
  pr = Pipe(ir)
  calls = []
  ret = []
  for (v, st) in pr
    ex = st.expr
    if isexpr(ex, :call)
      t = insert!(pr, v, stmt(xcall(Main, :forward, ex.args...), line = st.line))
      pr[v] = xcall(:getindex, t, 1)
      J = push!(pr, xcall(:getindex, t, 2))
      push!(calls, v)
      push!(ret, J)
    end
  end
  pb = Expr(:call, Pullback{T}, xcall(:tuple, ret...))
  return!(pr, xcall(:tuple, returnvalue(block(ir, 1)), pb))
  return finish(pr), calls
end

_sum() = 0
_sum(x) = x
_sum(x...) = xcall(:+, x...)

function adjoint(pr)
  ir = empty(pr)
  grads = Dict()
  grad(x) = _sum(get(grads, x, [])...)
  grad(x, x̄) = push!(get!(grads, x, []), x̄)
  grad(returnvalue(block(pr, 1)), IRTools.argument!(ir))
  data = push!(ir, xcall(:getfield, self, QuoteNode(:data)))
  _, pbs = primal(pr)
  pbs = Dict(pbs[i] => push!(ir, xcall(:getindex, data, i)) for i = 1:length(pbs))
  for v in reverse(keys(pr))
    ex = pr[v].expr
    isexpr(ex, :call) || continue
    Δs = push!(ir, Expr(:call, pbs[v], grad(v)))
    for (i, x) in enumerate(ex.args)
      grad(x, push!(ir, xcall(:getindex, Δs, i)))
    end
  end
  return!(ir, xcall(:tuple, [grad(x) for x in arguments(pr)]...))
end

@dynamo function forward(m...)
  ir = IR(m...)
  ir == nothing && return :(error("Non-differentiable function ", repr(args[1])))
  length(blocks(ir)) == 1 || error("control flow is not supported")
  return primal(ir, Tuple{m...})[1]
end

@dynamo function (pb::Pullback{S})(Δ) where S
  return adjoint(IR(S.parameters...))
end

forward(::typeof(sin), x) = sin(x), ȳ -> (nothing, ȳ*cos(x))
forward(::typeof(cos), x) = cos(x), ȳ -> (nothing, -ȳ*sin(x))

gradient(f, x...) = Base.tail(forward(f, x...)[2](1))

foo(x) = sin(cos(x))

# ir = @code_ir foo(1.0)
# primal(ir)[1]
# adjoint(ir)

gradient(foo, 1.0)
