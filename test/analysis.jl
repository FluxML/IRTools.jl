using IRTools, Test
using IRTools: CFG, dominators, domtree

relu(x) = (y = x > 0 ? x : 0)
ir = @code_ir relu(1)

@test dominators(CFG(ir)) ==
  Dict(1 => Set([1]), 2 => Set([1, 2]), 3 => Set([1, 3]), 4 => Set([1, 4]))

@test domtree(CFG(ir)) == (1 => [2 => [], 3 => [], 4 => []])

@test domtree(CFG(ir)', entry = 4) == (4 => [1 => [], 2 => [], 3 => []])
