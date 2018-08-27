using IRTools, Test
using IRTools: roundtrip

add(a, b) = a+b

@test roundtrip(add, 2, 3) == 5
