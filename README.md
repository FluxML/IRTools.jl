# IRTools

[![Build Status](https://travis-ci.org/MikeInnes/IRTools.jl.svg?branch=master)](https://travis-ci.org/MikeInnes/IRTools.jl)
[![](https://img.shields.io/badge/docs-dev-blue.svg)](https://mikeinnes.github.io/IRTools.jl/latest/)

IRTools aims to provide a simple and flexible IR format, expressive enough to work with both lowered and typed Julia code, as well as external IRs. It can be used with Julia metaprogramming tools such as [Cassette](https://github.com/jrevels/Cassette.jl).

```julia
julia> using IRTools

julia> function pow(x, n) # A simple Julia function
         r = 1
         while n > 0
           n -= 1
           r *= x
         end
         return r
       end

julia> ir = @code_ir pow(1, 1) # Get its IR
1: (%1, %2, %3)
  br 2 (%3, 1)
2: (%4, %5)
  %6 = %4 > 0
  br 4 unless %6
  br 3
3:
  %7 = %4 - 1
  %8 = %5 * %2
  br 2 (%7, %8)
4:
  return %5

julia> using IRTools: var, xcall

julia> ir[var(8)] = xcall(:+, var(5), var(2)) # Tweak it
:(%5 + %2)

julia> ir
1: (%1, %2, %3)
  br 2 (%3, 1)
2: (%4, %5)
  %6 = %4 > 0
  br 4 unless %6
  br 3
3:
  %7 = %4 - 1
  %8 = %5 + %2
  br 2 (%7, %8)
4:
  return %5

julia> f = IRTools.func(ir); # Turn the new IR into a lambda

julia> f(nothing, 10, 5)
51

julia> @code_llvm f(nothing, 10, 5)
define i64 @"julia_##399_17438"(i64, i64) {
top:
     %2 = icmp slt i64 %1, 1
     %3 = mul i64 %1, %0
     %4 = add i64 %3, 1
     %value_phi1.lcssa = select i1 %2, i64 1, i64 %4
    ret i64 %value_phi1.lcssa
}
```
