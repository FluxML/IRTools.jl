using IRTools, MacroTools, InteractiveUtils, Test
using IRTools: @dynamo, IR, meta, isexpr, xcall, self, insertafter!, recurse!,
  argument!, return!, func, var

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

@test_throws IRTools.CompileError err(+, 2, 3)

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

@test IRTools.eval(ir, 5) == 25

ir = IR()
x = argument!(ir)
y = argument!(ir)
z = push!(ir, xcall(:*, x, y))
return!(ir, z)

@test IRTools.eval(ir, 5, 3) == 15

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
