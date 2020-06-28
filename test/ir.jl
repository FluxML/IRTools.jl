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
  ir = IRTools.inline(ir1, IRTools.var(4), ir2)
  f = IRTools.func(ir)
  @test f(nothing, 2, 3) == 3
  @test f(nothing, 3, 2) == 3
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
  ir3 = IRTools.inline(ir, IRTools.var(4), ir2)
  @test IRTools.func(ir3)(nothing, 2) == 12
  @test IRTools.func(ir3)(nothing, 101) == 101
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
  ir3 = IRTools.inline(ir, IRTools.var(3), ir2)
  @test IRTools.func(ir3)(nothing, 2) == foo2(2)
  @test IRTools.func(ir3)(nothing, -2) == foo2(-2)
end
