using IRTools: @dynamo, argument!, IR

# Implementation of `invoke(f, argtypes::Type, args...`, where `args` don't have
# to conform to `argtypes`.  i.e., it allows you to call `f` on `args`, but using the
# method for `argtypes`.
# See https://julialang.slack.com/archives/CJ357CY2Y/p1579598240011500.

@dynamo function sneakyinvoke(f, ::Type{T}, args...) where T<:Tuple
  ir = IR(f, T.parameters...)
  argument!(ir, at = 2)
  return ir
end

f(x::Float64) = x^2

sneakyinvoke(f, Tuple{Float64}, 2)
