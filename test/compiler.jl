using IRTools, Test
using IRTools: @dynamo, IR, isexpr, xcall

@dynamo roundtrip(m) = IR(m)

@dynamo function passthrough(m)
  m == nothing && return
  ir = IR(m)
  for (x, st) in ir
    isexpr(st.expr, :call) || continue
    ir[x] = xcall(Main, :passthrough, st.expr.args...)
  end
  return ir
end

add(a, b) = a+b
@test roundtrip(add, 2, 3) == 5
@test passthrough(add, 2, 3) == 5

relu(x) = x > 0 ? x : 0
@test roundtrip(relu, 1) == 1
@test passthrough(relu, 1) == 1

foo(x) = y = x > 0 ? x + 1 : x - 1
@test roundtrip(foo, 1) == 2
@test roundtrip(foo, -1) == -2
@test passthrough(foo, 1) == 2
