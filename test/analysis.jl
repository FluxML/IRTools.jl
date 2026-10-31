using IRTools, Test
using IRTools: CFG, dominators, domtree

relu(x) = (y = x > 0 ? x : 0)
ir = @code_ir relu(1)

@test dominators(CFG(ir)) ==
  Dict(1 => Set([1]), 2 => Set([1, 2]), 3 => Set([1, 3]), 4 => Set([1, 4]))

@test domtree(CFG(ir)) == (1 => [2 => [], 3 => [], 4 => []])

@test domtree(CFG(ir)', entry = 4) == (4 => [1 => [], 2 => [], 3 => []])

# 2 ⇄ 3 is a loop, 5 has no predecessors and 6 ⇄ 7 is unreachable.
cfg = CFG([[2], [3, 4], [2], [], [4], [7], [6]])
@test IRTools.Inner.idoms(cfg) == [0, 1, 2, 1, 1, 0, 0]
@test domtree(cfg) == (1 => [2 => [3 => []], 4 => [], 5 => []])
@test dominators(cfg) == Dict(1 => Set([1]), 2 => Set([1, 2]), 3 => Set([1, 2, 3]),
                              4 => Set([1, 4]), 5 => Set([1, 5]), 6 => Set(1:7), 7 => Set(1:7))

@test domtree(CFG([[2], [1, 3], []])) == (1 => [2 => [3 => []]])
