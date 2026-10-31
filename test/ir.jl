using IRTools, Test
using IRTools: IR, @dynamo

function f(x)
    N = 4
    for i1 in 1:3    # single loop works without issue
        for i2 in 1:N   # needs to be a variable `1:4` works fine
        end
    end
    0.0 # same error with `x`
end

@test @code_ir(f(1)) isa IR

function f(a, b)
    u = 1
    while true
        if true
        end
    end
    f(u)
end

@test @code_ir(f(1, 2)) isa IR

# issue 30
@dynamo function foo(a...)
    ir = IR(a...)
    return ir
end

mylog2(x) = ccall((:log2, Base.Math.libm), Float64, (Float64,), x)

@test foo(mylog2, 3.3) === mylog2(3.3)

bar(a, b) = a > b ? a : b

function foo(a, b)
  bar(a, b)
end

let
  ir1 = @code_ir foo(1, 2)
  ir2 = @code_ir bar(1, 2)
  inline_at = findfirst(ir1) do (_, stmt)
    IRTools.isexpr(stmt.expr, :call) &&
      stmt.expr.args[1] == GlobalRef(@__MODULE__, :bar)
  end
  @test inline_at !== nothing
  ir = IRTools.inline(ir1, inline_at, ir2)
  f = IRTools.func(ir)
  # `invokelatest` is needed because `func` defines the method in a world newer
  # than the one this `let` block executes in.
  @test Base.invokelatest(f, nothing, 2, 3) == 3
  @test Base.invokelatest(f, nothing, 3, 2) == 3
end

function foo1(x)
  while x < 10
    x += 1
  end
  return x
end

function foo2(x)
  if x < 100
    x + foo1(x)
  else
    x
  end
end

let
  ir = @code_ir foo2(1)
  ir2 = @code_ir foo1(1)
  inline_at = findfirst(ir) do (_, stmt)
    IRTools.isexpr(stmt.expr, :call) &&
      stmt.expr.args[1] == GlobalRef(@__MODULE__, :foo1)
  end
  @test inline_at !== nothing
  ir3 = IRTools.inline(ir, inline_at, ir2)
  f = IRTools.func(ir3)
  @test Base.invokelatest(f, nothing, 2) == 12
  @test Base.invokelatest(f, nothing, 101) == 101
end


function foo1(x)
  while x < 10
    x += 1
  end
  return x
end

function foo2(x)
  foo1(x)
  x > 0 ? x : 0
end

let
  ir = @code_ir foo2(1)
  ir2 = @code_ir foo1(1)
  inline_at = findfirst(ir) do (_, stmt)
    IRTools.isexpr(stmt.expr, :call) &&
      stmt.expr.args[1] == GlobalRef(@__MODULE__, :foo1)
  end
  @test inline_at !== nothing
  ir3 = IRTools.inline(ir, inline_at, ir2)
  f = IRTools.func(ir3)
  @test Base.invokelatest(f, nothing, 2) == foo2(2)
  @test Base.invokelatest(f, nothing, -2) == foo2(-2)
end

# literals aren't looked up in the variable map, so `get` isn't compiled per literal type
let
  ir = IR()
  IRTools.argument!(ir)
  x = IRTools.argument!(ir)
  IRTools.return!(ir, push!(ir, IRTools.xcall(:tuple, x, Val(:pipe_literal))))
  pr = IRTools.Pipe(ir)
  for _ in pr end
  f = IRTools.func(IRTools.finish(pr))
  @test Base.invokelatest(f, nothing, 1) == (1, Val(:pipe_literal))
  m = which(get, Tuple{Dict{Any,Any},Any,Any})
  @test !any(Base.specializations(m)) do mi
    Val{:pipe_literal} in Base.unwrap_unionall(mi.specTypes).parameters
  end
end

# blocks index their statements' variables, which is rebuilt if `ir.defs` is changed directly
let
  ir = IR()
  x = IRTools.argument!(ir)
  a = push!(ir, IRTools.xcall(:sin, x))
  b = push!(ir, IRTools.xcall(:cos, a))
  c = insert!(ir, b, IRTools.xcall(:tan, a))
  d = IRTools.insertafter!(ir, x, IRTools.xcall(:exp, x))
  bl = IRTools.block(ir, 1)
  @test keys(bl) == [d, a, c, b]
  @test length(bl) == 4
  @test first.(collect(bl)) == [d, a, c, b]
  @test ir[c].expr == IRTools.xcall(:tan, a)
  delete!(ir, c)
  @test keys(bl) == [d, a, b]
  @test sort(IRTools.Inner.definitions(bl), by = v -> v.id) == [x, a, b, d]
  IRTools.block!(ir, 1)
  bl = IRTools.block(ir, 2)
  @test isempty(keys(IRTools.block(ir, 1)))
  @test keys(bl) == [d, a, b]
  t = push!(IRTools.block(ir, 1), ir[b])
  ir.defs[b.id], ir.defs[t.id] = ir.defs[t.id], (-1, -1)
  @test keys(IRTools.block(ir, 1)) == [b]
  @test keys(bl) == [d, a]
  @test insert!(ir, a, :foo) in keys(bl)
  @test length.(IRTools.blocks(ir)) == [1, 3]
end

let
  ir = IR()
  xs = [IRTools.argument!(ir, T) for T in (Int, Float64, String, Symbol)]
  y = IRTools.argument!(ir, Char, at = 4)
  @test IRTools.exprtype.((ir,), [xs; y]) == [Int, Float64, String, Symbol, Char]
  IRTools.deletearg!(ir, [3, 1])
  @test IRTools.arguments(ir) == [xs[2], y, xs[4]]
  @test IRTools.exprtype.((ir,), IRTools.arguments(ir)) == [Float64, Char, Symbol]
end
