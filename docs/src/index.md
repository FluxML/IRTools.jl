# IRTools

IRTools provides an IR format with several aims. The idea is to be:

* Expressive enough to represent all parts of Julia's IR pipeline, from lowered code to typed SSA IR;
* Easy to manipulate, like an AST, so that people can do powerful macro-like transformations of code;
* *safe* -- so no segfaults if you misplace an variable somewhere.

Note that before even attempting to understand IRTools, you should have a good handle on Julia's [metaprogramming and macros](https://docs.julialang.org/en/v1/manual/metaprogramming/).

## Reading the IR

### IR Basics

It's easiest to understand the IRTools IR by seeing some examples. We provide the macro `@code_ir` which behaves much like `@code_lowered`.

```julia
julia> using IRTools

julia> f(x) = x+x
f (generic function with 1 method)

julia> @code_ir f(1)
1:
  %1 = _2 + _2
  return %1
```

First things first. Arguments are numbered, and the first argument represents the function `f` itself, so `x` is presented in the IR as `_2`. Intermediate variables (`%1`, `%2`, `%3` ...) are also numbered. IR will usually have a _lot_ of these, which is why numbers make more sense than names.

The main reason that there are a lot of intermediates is that, in IR, we only allow one function call per line. You can see how a nested Julia expression becomes a sequence of single instructions, kind of like an assembly language.

```julia
julia> f(x) = 3x*x + 2x + 1
f (generic function with 1 method)

julia> @code_ir f(1)
1:
  %1 = 3 * _2
  %2 = %1 * _2
  %3 = 2 * _2
  %4 = %2 + %3 + 1
  return %4
```

While this looks noisy and is at first a little hard to read, it's usually a helpful thing to do. IR is largely designed to be read by programs, rather than by humans, where it's usually easier to look at one instruction at a time.

Beyond that, this is essentially just very verbosely-written Julia code.

### Control Flow

The most significant difference between `IR` and `Expr` is how control flow is handled. There are no such thing as nested if statements, while loops and so on in IR, only *branches*.

```julia
julia> f(x) = x > 0 ? x : 0
f (generic function with 1 method)

julia> @code_ir f(1)
1:
  %1 = _2 > 0
  br 3 unless %1
2:
  return _2
3:
  return 0
```

The block labels `1:`, `2:` etc and the branch `br 3 unless %1` can be thought of as a version of `@label` and `@goto`. In this case the branch is conditional on the test `%1 = x > 0`; if that's true we'll skip the branch, move on to the label `2` and return `x`.

IR is composed of a series of *basic blocks* that jump between each other like this. A basic block always starts with a label and ends with (optional) branches. No branches can appear in the middle of a basic block; that would just divide the block in two. Any structured control flow, however complex, can be turned into a series of blocks like this.

Here's a more interesting example.

```julia
julia> function f(x)
         if x < 0
           x = -x
         end
         return x
       end
f (generic function with 1 method)

julia> @code_ir f(1)
1:
  %1 = _2 < 0
  br 3 (_2) unless %1
2:
  %2 = -_2
  br 3 (%2)
3: (%3)
  return %3
```

Basic blocks are actually like mini-functions, and they accept a series of arguments. In this case block `3` takes an argument called `%3` that tells it what to return. If you follow the branches as if they were function calls, you'll see that this IR behaves the same the same as the code we wrote down.

Why not just write this as `_2 = - _2`? It's important to understand that variables in SSA-form IR are *immutable*, in the same sense that variables in functional languages are. For this reason you'll never see a statement like `%2 = %2 + 1`. This again makes analysing IR programmatically a lot easier, because when code uses `%2` you know exactly which definition that refers to.

Loops work this way too: they are visible in the IR by branches that jump backwards, i.e. the `br 2` here.

```julia
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
1:
  %1 = nothing
  br 2 (1, _3)
2: (%2, %3)
  %4 = %3 > 0
  br 4 unless %4
3:
  %5 = %3 - 1
  %6 = %2 * _2
  br 2 (%6, %5)
4:
  return %2
```

## Manipulating IR

### Statements

It's easy to get started by creating an empty fragment of IR.

```julia
julia> using IRTools: IR, Argument, var

julia> ir = IR()
1:
```

We can push new statements into the IR.

```julia
julia> x = arg(2)
_2

julia> x2 = push!(ir, :($x*$x))
%1

julia> ir
1:
  %1 = _2 * _2
```

`push!` returns a variable name that we can reuse later on.

```julia
julia> push!(ir, :(3*$x2 + 2*$x + 1))
%5

julia> ir
1:
  %1 = _2 * _2
  %2 = 3 * %1
  %3 = 2 * _2
  %4 = %2 + %3 + 1
```

The IR can be viewed as a mapping from variables to statements, and indexing and iteration are consistent with that.

```julia
julia> ir[var(2)]
IRTools.Statement(:(3 * %1), Any, 0)

julia> collect(ir)
4-element Array{Any,1}:
 (%1, IRTools.Statement(:(_2 * _2), Any, 0))
 (%2, IRTools.Statement(:(3 * %1), Any, 0))
 (%3, IRTools.Statement(:(2 * _2), Any, 0))
 (%4, IRTools.Statement(:(%2 + %3 + 1), Any, 0))
```

A `Statement` consists of an expression, a type (usually `Any` unless you're explicitly working with typed IR) and a line number. If you work directly with expressions IRTools will automatically wrap them with `Statement(x)`.

There are a few other functions that do obvious things: `pushfirst!`, `insert!`, `insertafter!`, and `delete!`.

### Blocks

In most cases you won't build IR from scratch, but will work from a fragment from an existing function.

```julia
julia> ir = @code_ir pow(1, 1)
1:
  %1 = nothing
  br 2 (1, _3)
2: (%2, %3)
  %4 = %3 > 0
  br 4 unless %4
3:
  %5 = %3 - 1
  %6 = %2 * _2
  br 2 (%6, %5)
4:
  return %2
```

You can work with a block at a time with `block(ir, n)` (all of them with `blocks(ir)`). Blocks similarly support functions like `push!`.

```julia
julia> using IRTools: block

julia> block(ir, 2)
2: (%2, %3)
  %4 = %3 > 0
  br 4 unless %4
```

## IR Internals

Internally the IR data structure is quite simple, and it's worth looking at the source code for more details. Each IR fragment is essentially a list of basic blocks.

```julia
julia> ir = @code_ir pow(1, 1);

julia> ir.blocks[1]
IRTools.BasicBlock(IRTools.Statement[Statement(nothing, Nothing, 0)], IRTools.Variable[], IRTools.Branch[br 2 (1, _3)])
```

Each block is a list of statements, argument names and branches.

Note that no variable names like `%2` are set here. This is defined by a mapping at the IR level:

```julia
julia> ir.defs
6-element Array{Tuple{Int64,Int64},1}:
 (1, 1)
 (-1, -1)
 (-1, -1)
 (2, 3)
 (3, 1)
 (3, 2)
```

SSA values are looked up from this table, in order, so `%4` refers to statement 3 of block 2 and so on. Values listed as `(-1, -1)` have been deleted.
