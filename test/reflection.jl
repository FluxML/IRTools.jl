using IRTools, Test
using IRTools: Meta, meta

@generated f(x) = :(x+x)

@test meta(Tuple{typeof(gcd),Int,Int}) isa Meta

@test meta(Tuple{typeof(f),Int}) isa Meta
