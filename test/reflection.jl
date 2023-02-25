using IRTools, Test
using IRTools: Meta, meta

@generated f(x) = :(x+x)

@test meta(Tuple{typeof(gcd),Int,Int}) isa Meta

@test meta(Tuple{typeof(f),Int}) isa Meta

# https://github.com/FluxML/IRTools.jl/issues/106
function g end
@test meta(Tuple{typeof(g),Int,Int}) === nothing
g(a) = 2a
@test meta(Tuple{typeof(g),Int,Int}) === nothing

@test @code_ir(map([1,2], [3,4]) do x, y
  x + y
end) isa IR
