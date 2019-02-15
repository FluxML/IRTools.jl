# IRTools

[![Build Status](https://travis-ci.org/MikeInnes/IRTools.jl.svg?branch=master)](https://travis-ci.org/MikeInnes/IRTools.jl)

IRTools aims to provide an easy-to-use and hard-to-get-wrong format for working with lowered Julia code, somewhere between an `Expr` and full-blown IR. It can be used with tools that let you manipulate Julia IR, such as [Cassette](https://github.com/jrevels/Cassette.jl).

Convert `code_lowered` to IR:

```julia
julia> function pow(x, n)
         r = 1
         while n > 0
           n -= 1
           r *= x
         end
         return r
       end

julia> using IRTools

julia> ir = @code_ir pow(2, 3)
1:
  %1 = nothing
2:
  %2 = φ (%1 => 1, %3 => %6)
  %3 = φ (%1 => _3, %3 => %5)
  %4 = %3 > 0
  goto %4 if not %4
3:
  %5 = %3 - 1
  %6 = %2 * _2
  goto %2
4:
  return %2
```

Usual container functions like `push!` and `pushfirst!` try to work as expected. They generally return SSA values that you can use to refer to the result of that statement.

You can also use `IRTools.block(ir, i)` to get the `i`th block and push/insert into it directly.

```julia
julia> ir = IRTools.IR()
1:

julia> x = push!(ir, :(foo()))
:(%1)

julia> y = push!(ir, :(bar($x)))
:(%2)

julia> ir
1:
  %1 = foo()
  %2 = bar(%1)
```

`insert!` and `insertafter!`:

```julia
julia> using IRTools: insertafter!

julia> insert!(ir, x, :(before_x()))
:(%3)

julia> insertafter!(ir, x, :(after_x()))
:(%4)

julia> ir
1:
  %3 = before_x()
  %1 = foo()
  %4 = after_x()
  %2 = bar(%1)
```

Map over IR:

```julia
julia> map(x -> @show(x), ir)
x = :(before_x())
x = :(foo())
x = :(after_x())
x = :(bar(%1))
1:
  %3 = before_x()
  %1 = foo()
  %4 = after_x()
  %2 = bar(%1)
```

Define a no-op Cassette-like pass:

```julia
julia> using IRTools: IR, meta, varargs!, argnames!, spliceargs!, update!

julia> @generated function roundtrip(f, args...)
         m = meta(Tuple{f,args...})
         ir = IR(m)
         ir = varargs!(m, ir)
         argnames!(m, :f, :args)
         ir = spliceargs!(m, ir, (Symbol("#self#"), typeof(roundtrip)))
         return update!(m, ir)
       end
roundtrip (generic function with 1 method)

julia> roundtrip(+, 2, 3)
5
```
