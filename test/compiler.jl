using IRTools, Test
using IRTools: @dynamo, IR, meta, isexpr, xcall
using MacroTools

@dynamo roundtrip(a...) = IR(a...)

@dynamo function passthrough(a...)
  ir = IR(a...)
  ir == nothing && return
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

@test_broken passthrough(() -> [1, 2, 3]) == [1, 2, 3]

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
