using IRTools, Test
using IRTools: roundtrip

add(a, b) = a+b
relu(x) = x > 0 ? x : 0

@test roundtrip(add, 2, 3) == 5
@test roundtrip(relu, 1) == 1
