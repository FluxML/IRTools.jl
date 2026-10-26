using IRTools, MacroTools, InteractiveUtils, Test
using IRTools: @dynamo, IR, meta, isexpr, xcall, self, insertafter!, recurse!,
  argument!, return!, block!, branch!, func, var, functional
using InteractiveUtils: code_typed

@dynamo roundtrip(a...) = IR(a...)

@dynamo function passthrough(a...)
  ir = IR(a...)
  ir == nothing && return
  recurse!(ir)
  return ir
end

add(a, b) = a+b
@test roundtrip(add, 2, 3) == 5
@test passthrough(add, 2, 3) == 5
@test passthrough(sin, 1.0) == sin(1.0)

f_returnnode(::T) where {T} = T
@test passthrough(f_returnnode, 1) == Int
f_gotoifnot(::Val{x}) where {x} = x ? 1 : 0
@test passthrough(f_gotoifnot, Val(true)) == 1

# `invoke` shifts the slots of the invoked method, including those used
# directly by `return` and `goto ... if not` (#126).
f_invoke_id(x) = x
function f_invoke_branch(x, c)
  y = x
  if c
    y = 2x
  end
  return y
end
@test passthrough(x -> invoke(f_invoke_id, Tuple{Any}, x), 1) == 1
@test passthrough((x, c) -> invoke(f_invoke_branch, Tuple{Any,Any}, x, c), 1, true) == 2
@test passthrough((x, c) -> invoke(f_invoke_branch, Tuple{Any,Any}, x, c), 1, false) == 1
# Constructors refer to their arguments as `Core.Argument`s as of Julia 1.14.
struct InvokeT{T}
  x::T
  y::Int
end
@test passthrough((x, y) -> invoke(InvokeT{Int}, Tuple{Any,Any}, x, y), 1, 2) == InvokeT{Int}(1, 2)

@test @code_ir(passthrough, add(2, 3)) isa IR

IRTools.refresh(roundtrip)

add(a, b) = a*b
@test roundtrip(add, 2, 3) == 6

relu(x) = x > 0 ? x : 0
@test roundtrip(relu, 1) == 1
@test passthrough(relu, 1) == 1

foo(x) = y = x > 0 ? x + 1 : x - 1
@test roundtrip(foo, 1) == 2
@test roundtrip(foo, -1) == -2
@test passthrough(foo, 1) == 2

@test passthrough(() -> [1, 2, 3]) == [1, 2, 3]

# Loop-carried variables that permute on a back-edge lower to a parallel copy of
# block arguments; sequentialising it naively drops values (FluxML/Zygote.jl#1198).
function swap_loop(x, y, n)
  for _ in 1:n
    x, y = y, x
  end
  return x
end
for n in 0:5
  @test roundtrip(swap_loop, 10, 20, n) == swap_loop(10, 20, n)
end

function rotate_loop(a, b, c, n)
  for _ in 1:n
    a, b, c = b, c, a
  end
  return (a, b, c)
end
for n in 0:6
  @test roundtrip(rotate_loop, 1, 2, 3, n) == rotate_loop(1, 2, 3, n)
end

function err(f)
  try
    f()
  catch e
    e
  end
end

@test passthrough(err, () -> 2+2) == 4
@test passthrough(err, () -> 0//0) isa ArgumentError

function err2(f)
    x = 1
    y = 1
    try
        y = f()
    catch e
      x, y
    end
    x, y
end

@test passthrough(err2, () -> 2+2) == (1, 4)
@test passthrough(err2, () -> 0//0) == (1, 1)

function err3(f)
  y = 1
  try
    y = f()
  catch
  end
  return y
end

@test passthrough(err3, () -> 2+2) == 4
@test passthrough(err3, () -> 0//0) == 1

# `@with` lowers to a try region whose `enter` references the new scope by SSA value.
@static if isdefined(Base, :ScopedValues)
  const scoped = Base.ScopedValues.ScopedValue(0)
  function with_scope(x)
    y = x + 1
    Base.ScopedValues.@with(scoped => y, 2 * scoped[])
  end
  @test roundtrip(with_scope, 2) == 6
  @test passthrough(with_scope, 2) == 6
end

@dynamo function mullify(a...)
  ir = IR(a...)
  ir == nothing && return
  ir = MacroTools.prewalk(ir) do x
    x isa GlobalRef && x.name == :(*) && return GlobalRef(Base, :+)
    return x
  end
  for (x, st) in ir
    isexpr(st.expr, :call) || continue
    ir[x] = xcall(Main, :mullify, st.expr.args...)
  end
  return ir
end

@test mullify(prod, [5, 10]) == 15

@dynamo err(a...) = 0//0

@test_throws IRTools.Inner.CompileError err(+, 2, 3)

mutable struct Context
  calls::Int
end

@dynamo function (cx::Context)(a...)
  ir = IR(a...)
  ir == nothing && return
  recurse!(ir)
  calls = pushfirst!(ir, xcall(:getfield, self, QuoteNode(:calls)))
  calls = insertafter!(ir, calls, xcall(:+, calls, 1))
  insertafter!(ir, calls, xcall(:setfield!, self, QuoteNode(:calls), calls))
  return ir
end

@code_lowered Context(0)(add, 2, 3)

cx = Context(0)
@test cx(add, 2, 3.0) == 6
@test cx.calls > 5

ir = IR()
x = argument!(ir)
y = push!(ir, xcall(:*, x, x))
return!(ir, y)

@test IRTools.evalir(ir, 5) == 25

ir = IR()
x = argument!(ir)
y = argument!(ir)
z = push!(ir, xcall(:*, x, y))
return!(ir, z)

@test IRTools.evalir(ir, 5, 3) == 15

function pow(x, n)
  r = 1
  while n > 0
    n -= 1
    r *= x
  end
  return r
end

ir = @code_ir pow(2, 3)

ir[var(8)] = xcall(:+, var(5), var(2))

mul = func(ir)

@test mul(nothing, 10, 3) == 31

@dynamo function ir_add(_, _)
  ir = IR()
  args = argument!(ir)
  x = push!(ir, xcall(:getindex, args, 1))
  y = push!(ir, xcall(:getindex, args, 2))
  return!(ir, xcall(:+, x, y))
end

@test ir_add(5, 2) == 7

@dynamo function test_lambda(x)
  λ = IR()
  self = argument!(λ)
  y = argument!(λ)
  x = push!(λ, xcall(:getindex, self, 1))
  return!(λ, xcall(:+, x, y))
  ir = IR()
  args = argument!(ir)
  x = push!(ir, xcall(:getindex, args, 1))
  return!(ir, Expr(:lambda, λ, x))
end

let
  f = test_lambda(3)
  @test f(6) == 9
end

anf(f::Core.IntrinsicFunction, args...) = f(args...)

@dynamo function anf(args...)
  ir = IR(args...)
  ir == nothing && return
  functional(recurse!(ir))
end

@test anf(pow, 2, 3) == 8

# Closures produced by `functional` can be evaluated with `evalir` (#102).
@testset "evalir with lambdas" begin
  using IRTools: functional, evalir
  bar() = if true false else false end
  function powr(x, n); r = 1; while n > 0; r *= x; n -= 1; end; r; end
  function nest(n)
    s = 0
    for i = 1:n
      for j = 1:i
        j % 2 == 0 && continue
        s += j
      end
    end
    s
  end
  @test evalir(functional(IR(typeof(bar))), bar) == bar()
  @test evalir(functional(IR(typeof(powr), Int, Int)), powr, 2, 10) == 1024
  @test evalir(functional(IR(typeof(nest), Int)), nest, 7) == nest(7)
  @dynamo function functional_dyna(a...)
    ir = IR(a...)
    ir === nothing && return
    functional(ir)
  end
  @test functional_dyna(bar) == bar()
  @test evalir(@code_ir(functional_dyna, bar()), bar) == bar()
  @test evalir(@code_ir(functional_dyna, powr(2, 3)), powr, 2, 10) == 1024
end

# `break` leaves unreachable blocks behind (#129).
function f_break()
  while true break end
end

function f_break_loop(n)
  i = 0
  while true
    i += 1
    i >= n && break
  end
  return i
end

@test anf(f_break) === nothing
@test anf(f_break_loop, 3) == 3
@test anf(f_break_loop, -1) == 1

function f94(x)
    i = x
    while i > 0
        i -= 1
    end
    return x
end
@test f94(1) == 1
@test passthrough(f94, 1) == 1

@testset "unreachable" begin
    # 1: (%1)
    #   return nothing
    # 2:
    #   unreachable
    ir = IR()
    argument!(ir)
    return!(ir, nothing)
    block!(ir)
    branch!(ir, 0)

    func_ir = IRTools.func(ir)
    @test (code_typed(func_ir, Tuple{typeof(func_ir)}) |> only
           isa Pair{Core.CodeInfo,DataType})
end

# Blocks with more than two branches (#100).
@testset "multibranch" begin
  f100(x, y) = x && y && error()
  g100(a, b, c, d) = a && b && c && d && error()
  function h100(n, a, b)
    s = 0
    for i = 1:n
      a && b && i > 1 && (s += i)
    end
    return s
  end
  @test_nowarn functional(IR(typeof(f100), Bool, Bool))
  @test_nowarn functional(IR(typeof(g100), Bool, Bool, Bool, Bool))
  for x in (false, true), y in (false, true)
    if x && y
      @test_throws ErrorException anf(f100, x, y)
    else
      @test anf(f100, x, y) == f100(x, y)
    end
  end
  for a in (false, true), b in (false, true), c in (false, true), d in (false, true)
    if a && b && c && d
      @test_throws ErrorException anf(g100, a, b, c, d)
    else
      @test anf(g100, a, b, c, d) == g100(a, b, c, d)
    end
  end
  for a in (false, true), b in (false, true), n in (0, 1, 4)
    @test anf(h100, n, a, b) == h100(n, a, b)
  end
end

function f_try_catch(x)
    y = 0.
    try
        y = sqrt(x)
    catch

    end
    y
end

function f_try_catch2(x, cond)
    local y
    if cond
        y = 2x
    end

    try
        x = 3 * error()
    catch
    end

    y
end

function f_try_catch3()
    local x
    try
        error()
    catch
        x = 42
    end
    x
end

function f_try_catch4(x, cond)
    local y
    try
        throw(x)
    catch err
        if cond
            y = err + x
        end
    end
    y
end

function f_try_catch5(x, cond)
    local y
    cond && (x = 2x)
    try
        y = x
        cond && error()
    catch
        y = x + 1
    end
    y
end

function f_try_catch6(cond, y)
    x = 1

    if cond
        y = 10y
    else
        y = 10y
    end

    try
        cond && error()
    catch
        y = 2x
    end

    y+x
end

function f_try_catch7()
  local x = 1.

  for _ in 1:10

      try
          x = sqrt(x)
          x -= 1.
      catch
          x = -x
      end

      x = x ^ 2
  end

  x
end

function f_try_catch8(x)
    local z = x
    try
        z = z + sqrt(x)
        z = 1 - log(z)
        z = log(z)
    catch
        z = abs(z)
    finally
        return z
    end
end

@testset "try/catch" begin
    ir = @code_ir f_try_catch(1.)
    fir = func(ir)
    @test fir(nothing,1.) === 1.
    @test fir(nothing,-1.) === 0.

    ir = @code_ir f_try_catch2(1., false)
    fir = func(ir)

    # This should be @test_throws UndefVarError fir(nothing,42,false)
    # See TODO in `IRTools.slots!`
    @test_broken try
        fir(nothing,42,false)
        false
    catch e
        e isa UndefVarError
    end
    @test fir(nothing, 42, false) === IRTools.undef
    @test fir(nothing, 42, true) === 84

    ir = @code_ir f_try_catch3()
    @test all(ir) do (_, stmt)
        !IRTools.isexpr(stmt.expr, :catch) ||
          length(stmt.expr.args) == 1
    end
    fir = func(ir)
    @test fir(nothing) == 42

    ir = @code_ir f_try_catch4(42, false)
    fir = func(ir)
    # This should be @test_throws UndefVarError fir(nothing,42,false)
    @test_broken try
        fir(nothing, 42, false)
        false
    catch e
        e isa UndefVarError
    end
    @test fir(nothing, 42, false) === IRTools.undef
    @test fir(nothing, 42, true) === 84

    ir = @code_ir f_try_catch5(1, false)
    fir = func(ir)
    @test fir(nothing, 3, false) === 3
    @test fir(nothing, 3, true) === 7

    ir = @code_ir f_try_catch6(true, 1)
    fir = func(ir)
    @test fir(nothing, true, 1) === 3
    @test fir(nothing, false, 1) === 11

    ir = @code_ir f_try_catch7()
    fir = func(ir)
    @test fir(nothing) === 1.

    ir = @code_ir f_try_catch8(1.)
    fir = func(ir)
    @test fir(nothing, 1.) == log(1. - log(2.))
    @test fir(nothing, -1.) == 1.
end

# `try` regions with several exits, each with its own `leave`.
function f_try_return(cond)
    try
        if cond return end
    finally
    end
end

function f_try_early_return(cond, y)
    x = 1
    try
        cond && return x
        x = 2
        y < 0 && error()
        x = 3
    catch
        return -x
    end
    return x
end

function f_try_break(n)
    s = 0
    for i in 1:n
        try
            i > 3 && break
            s += i
        catch
        end
    end
    return s
end

function f_try_goto(x)
    try
        try
            x > 0 && @goto out
        catch
        end
    catch
    end
    return 0
    @label out
    y = 2x
    return y
end

@testset "try with early exits" begin
    ir = @code_ir f_try_return(true)
    fir = func(ir)
    @test fir(nothing, true) === nothing
    @test fir(nothing, false) === nothing

    ir = @code_ir f_try_early_return(true, 1)
    fir = func(ir)
    @test fir(nothing, true, 1) === 1
    @test fir(nothing, false, 1) === 3
    @test fir(nothing, false, -1) === -2

    ir = @code_ir f_try_break(1)
    fir = func(ir)
    @test fir(nothing, 2) === 3
    @test fir(nothing, 10) === 6

    # The `goto` leaves both handlers, so the code after the label must not
    # branch to either catch block.
    ir = @code_ir f_try_goto(1)
    @test !any(((_, stmt),) -> IRTools.isexpr(stmt.expr, :catch), IRTools.blocks(ir)[end])
    fir = func(ir)
    @test fir(nothing, 2) === 4
    @test fir(nothing, -2) === 0

    @test IR(meta(Tuple{typeof(Base.Partr.multiq_size), Int8})) isa IR
end

# `@isdefined` on a conditionally-defined local. SSA conversion turns the slot into
# a block argument carrying `Undefined()` when unassigned, so `isdefined` must test
# for that rather than substituting the slot's value (which is always "defined").
function f_isdefined(x)
    local y
    x > 0 && (y = 3x)
    @isdefined(y) ? y : zero(x)
end

# `isdefined` of an argument: the arg slot is lowered to a variable before `ssa!`,
# so this arrives as `isdefined(<variable>)` rather than `isdefined(<slot>)`.
f_isdefined_arg(x) = @isdefined(x) ? x * 2 : zero(x)

@testset "isdefined" begin
    ir = @code_ir f_isdefined(1.)
    # The `isdefined` node must not survive with a value argument in its place.
    @test !any(ir) do (_, stmt)
        IRTools.isexpr(stmt.expr, :isdefined)
    end
    fir = func(ir)
    @test fir(nothing, 2.) === 6.    # y defined
    @test fir(nothing, -1.) === 0.   # y undefined -> else branch
    @test passthrough(f_isdefined, 2.) === 6.
    @test passthrough(f_isdefined, -1.) === 0.

    ir = @code_ir f_isdefined_arg(1.)
    @test !any(ir) do (_, stmt)
        IRTools.isexpr(stmt.expr, :isdefined)
    end
    fir = func(ir)
    @test fir(nothing, 2.) === 4.
    @test passthrough(f_isdefined_arg, 2.) === 4.
end

# Bare symbols in calls resolve in the method's module / the module given to `func`
bare_hits = Ref(0)
bare_hit() = (bare_hits[] += 1; nothing)
bare_f(x) = x + 1
@dynamo function bare_dyn(a...)
  ir = IR(a...)
  ir == nothing && return
  pushfirst!(ir, :(bare_hit()))
  return ir
end
bare_hits[] = 0
@test bare_dyn(bare_f, 1) == 2
@test bare_hits[] == 1

module BareMod
  bare_hit() = (hits[] += 1; nothing)
  const hits = Ref(0)
end
ir = IR()
x = argument!(ir)
push!(ir, :(bare_hit()))
return!(ir, x)
@test IRTools.evalir(BareMod, ir, 5) == 5
@test BareMod.hits[] == 1
@test bare_hits[] == 1

# also with a `Meta`: the module passed to `evalir` wins over the method's
ir = IR(typeof(bare_f), Int)
pushfirst!(ir, :(bare_hit()))
@test IRTools.evalir(BareMod, ir, bare_f, 1) == 2
@test BareMod.hits[] == 2
@test bare_hits[] == 1

# a dynamo resolves bare symbols in its own module, not the IR's source module
module BareB
  f(x) = x + 1
  hook() = "B.hook"
end
module BareA
  using IRTools
  hook() = "A.hook"
  IRTools.@dynamo function dyn(a...)
    ir = IRTools.IR(a...)
    ir === nothing && return
    h = push!(ir, Expr(:call, :hook))
    IRTools.return!(ir, h)
    return ir
  end
  struct Counter end
  IRTools.@dynamo function (c::Counter)(a...)
    ir = IRTools.IR(a...)
    ir === nothing && return
    h = push!(ir, Expr(:call, :hook))
    IRTools.return!(ir, h)
    return ir
  end
end
@test BareA.dyn(BareB.f, 1) == "A.hook"
@test BareA.Counter()(BareB.f, 1) == "A.hook"

# `recurse!` keeps splatted calls intact and recurses into the callee (#74, #62).
mutable struct SplatTrace
  calls::Vector{Any}
end

@dynamo function (t::SplatTrace)(a...)
  ir = IR(a...)
  ir == nothing && return
  recurse!(ir)
  return ir
end

(t::SplatTrace)(::typeof(rand), xs...) = (push!(t.calls, xs); 0.0)
f_splat(args) = rand(:sym, args...)
st = SplatTrace([])
@test st(f_splat, (1, 2)) == 0.0
@test st.calls == [(:sym, 1, 2)]
f_splat2(xs, ys) = +(xs..., 1, ys...)
@test passthrough(f_splat2, (1, 2), (3,)) == 7

# The callable's name is bound to its type in the body (#51).
struct SelfCounter end
const self_seen = Ref{Any}(nothing)
@dynamo function (c::SelfCounter)(m...)
  self_seen[] = c
  IR(m...)
end

@test SelfCounter()(sin, 0.5) == sin(0.5)
@test self_seen[] === SelfCounter

# Dynamos can be restricted to a subset of argument types (#41).
@dynamo function dispatch_real(f, x::Real)
  IR(f, x)
end

@testset "dynamo dispatch" begin
  @test dispatch_real(sin, 0.5) == sin(0.5)
  @test_throws MethodError dispatch_real(sin, "a")
end

@dynamo function prepend_stmts(a...)
  ir = IR(a...)
  ir === nothing && return
  for _ in 1:3
    pushfirst!(IRTools.block(ir, 1), xcall(Base, :identity, 1))
  end
  return ir
end

const throw_line = (@__LINE__) + 3
function throws_on_line(x)
  y = x + 1
  error("throws_on_line")
end

@testset "line info survives inserted statements" begin
  bt = try prepend_stmts(throws_on_line, 1) catch; stacktrace(catch_backtrace()) end
  @test any(fr -> endswith(String(fr.file), "compiler.jl") && fr.line == throw_line, bt)
end
