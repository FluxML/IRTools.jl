using IRTools, Test
using IRTools: roundtrip

add(a, b) = a+b
@test roundtrip(add, 2, 3) == 5

relu(x) = x > 0 ? x : 0
@test roundtrip(relu, 1) == 1

foo(x) = y = x > 0 ? x + 1 : x - 1
@test roundtrip(foo, 1) == 2
@test roundtrip(foo, -1) == -2
