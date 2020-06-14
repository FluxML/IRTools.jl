using IRTools, Test
using IRTools: Meta, meta

@generated f(x) = :(x+x)

@test meta(Tuple{typeof(gcd),Int,Int}) isa Meta

@test meta(Tuple{typeof(f),Int}) isa Meta

@test @code_ir(map([1,2], [3,4]) do x, y
  x + y
end) isa IR
