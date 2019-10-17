# IRTools

IRTools provides an IR format with several aims. The idea is to be:

* Expressive enough to represent all parts of Julia's IR pipeline, from lowered code to typed SSA IR;
* Easy to manipulate, like an AST, so that people can do powerful macro-like transformations of code;
* *safe* -- so no segfaults if you misplace an variable somewhere.

Note that before even attempting to understand IRTools, you should have a good handle on Julia's [metaprogramming and macros](https://docs.julialang.org/en/v1/manual/metaprogramming/).

## Reading the IR

### IR Basics

It's easiest to understand the IRTools IR by seeing some examples. We provide the macro `@code_ir` which behaves much like `@code_lowered`.

```jldoctest main
julia> using IRTools

julia> f(x) = x+x
f (generic function with 1 method)

julia> @code_ir f(1)
1: (%1, %2)
  %3 = %2 + %2
  return %3
```

First things first. All variables are numbered (`%1`, `%2`, `%3` ...). IR will usually have a _lot_ of these, which is why numbers make more sense than names. At the start of the IR is a list of arguments that are provided as input to the function, `(%1, %2)`. You'll notice there's an extra argument, `%1`, that's ignored here; this represents the function `f` itself, which is used by callable objects and closures.

The main reason that there are a lot of intermediates is that, in IR, we only allow one function call per line. You can see how a nested Julia expression becomes a sequence of single instructions, kind of like an assembly language.

```jldoctest main
julia> f(x) = 3x*x + 2x + 1
f (generic function with 1 method)

julia> @code_ir f(1)
1: (%1, %2)
  %3 = 3 * %2
  %4 = %3 * %2
  %5 = 2 * %2
  %6 = %4 + %5 + 1
  return %6
```

While this looks noisy and is at first a little hard to read, it's usually a helpful thing to do. IR is largely designed to be read by programs, rather than by humans, where it's usually easier to look at one instruction at a time.

Beyond that, this is essentially just very verbosely-written Julia code.

### Control Flow

The most significant difference between `IR` and `Expr` is how control flow is handled. There are no such thing as nested if statements, while loops and so on in IR, only *branches*.

```jldoctest main
julia> f(x) = x > 0 ? x : 0
f (generic function with 1 method)

julia> @code_ir f(1)
1: (%1, %2)
  %3 = %2 > 0
  br 2 unless %3
  return %2
2:
  return 0
```

The block labels `1:`, `2:` etc and the branch `br 2 unless %3` can be thought of as a version of `@label` and `@goto`. In this case the branch is conditional on the test `%3 = x > 0`; if that's true we'll skip the branch labeled `2` and return `x`.

IR is composed of a series of *basic blocks* that jump between each other like this. A basic block always starts with a label and ends with (optional) branches. No branches can appear in the middle of a basic block; that would just divide the block in two. Any structured control flow, however complex, can be turned into a series of blocks like this.

Here's a more interesting example.

```jldoctest main
julia> function f(x)
         if x < 0
           x = -x
         end
         return x
       end
f (generic function with 1 method)

julia> @code_ir f(1)
1: (%1, %2)
  %3 = %2 < 0
  br 3 (%2) unless %3
  br 2
2:
  %4 = -%2
  br 3 (%4)
3: (%5)
  return %5
```

Basic blocks are actually like mini-functions, and they accept a series of arguments. In this case block `3` takes an argument called `%5` that tells it what to return. If you follow the branches as if they were function calls, you'll see that this IR behaves the same the same as the code we wrote down.

Why not just write this as `%2 = -%2`? It's important to understand that variables in SSA-form IR are *immutable*, in the same sense that variables in functional languages are. For this reason you'll never see a statement like `%2 = %2 + 1`. This again makes analysing IR programmatically a lot easier, because when code uses `%2` you know exactly which definition that refers to.

Loops work this way too: they are visible in the IR by branches that jump backwards, i.e. the `br 2` here. Variables that were modified inside the loop in the original code are explicitly passed between blocks.

```jldoctest main
julia> function pow(x, n)
         r = 1
         while n > 0
           n -= 1
           r *= x
         end
         return r
       end
pow (generic function with 1 method)

julia> @code_ir pow(1, 1)
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
```

## Manipulating IR

### Statements

It's easy to get started by creating an empty fragment of IR.

```jldoctest main
julia> using IRTools: IR, var, argument!, xcall

julia> ir = IR()
1:
```

We can push new statements into the IR. `push!` returns a variable name that we can reuse later on.

```jldoctest main
julia> x = argument!(ir)
%1

julia> x2 = push!(ir, xcall(:*, x, x))
%2

julia> ir
1: (%1)
  %2 = %1 * %1
```

The IR can be viewed as a mapping from variables to statements, and indexing and iteration are consistent with that.

```julia
julia> ir[var(2)]
IRTools.Statement(:(%1 * %1), Any, 0)

julia> collect(ir)
1-element Array{Any,1}:
 %2 => IRTools.Statement(:(%1 * %1), Any, 0)
```

A `Statement` consists of an expression, a type (usually `Any` unless you're explicitly working with typed IR) and a line number. If you work directly with expressions IRTools will automatically wrap them with `Statement(x)`.

There are a few other functions that do obvious things: `pushfirst!`, `insert!`, `insertafter!`, and `delete!`.

In most cases you won't build IR from scratch, but will start from an existing function and modify its IR.

```jldoctest main
julia> ir = @code_ir pow(1, 1)
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
```

### Blocks

You can work with a block at a time with `block(ir, n)` (all of them with `blocks(ir)`). Blocks similarly support functions like `push!`.

```jldoctest main
julia> using IRTools: block

julia> block(ir, 2)
2: (%4, %5)
  %6 = %4 > 0
  br 4 unless %6
  br 3
```

## Evaluating IR

For testing purposes, you can run IR using `IRTools.eval`.

```jldoctest eval
julia> using IRTools

julia> using IRTools: IR, argument!, return!, xcall, func

julia> ir = IR();

julia> x = argument!(ir);

julia> y = push!(ir, xcall(:*, x, x));

julia> return!(ir, y)
1: (%1)
  %2 = %1 * %1
  return %2

julia> IRTools.evalir(ir, 5)
25
```

More generally, you can turn an IR fragment into an anonymous function, useful
not just for evaluation but also to see the compiler's `@code_typed`,
`@code_llvm` output etc.

```julia
julia> f = func(ir)
##422 (generic function with 1 method)

julia> @code_typed f(5)
CodeInfo(
1 ─ %1 = Base.mul_int(@_2, @_2)::Int64
└──      return %1
) => Int64

julia> @code_llvm f(5)
;  @ /Users/mike/projects/flux/IRTools/src/eval.jl:18 within `##422'
define i64 @"julia_##422_17676"(i64) {
top:
; ┌ @ int.jl:54 within `*'
   %1 = mul i64 %0, %0
   ret i64 %1
; └
}
```

The same works for IR taken from existing functions.

```jldoctest
julia> using IRTools: IR, @code_ir, xcall, func, var

julia> function pow(x, n)
         r = 1
         while n > 0
           n -= 1
           r *= x
         end
         return r
       end
pow (generic function with 1 method)

julia> ir = @code_ir pow(2, 3)
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


julia> ir[var(8)] = xcall(:+, var(5), var(2))
:(%5 + %2)

julia> mul = func(ir);

julia> mul(nothing, 10, 3)
31
```
