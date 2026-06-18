using IRTools, Test
using IRTools: CFG, dominators, domtree, dependencies, var

relu(x) = (y = x > 0 ? x : 0)
ir = @code_ir relu(1)

@test dominators(CFG(ir)) ==
  Dict(1 => Set([1]), 2 => Set([1, 2]), 3 => Set([1, 3]), 4 => Set([1, 4]))

@test domtree(CFG(ir)) == (1 => [2 => [], 3 => [], 4 => []])

@test domtree(CFG(ir)', entry = 4) == (4 => [1 => [], 2 => [], 3 => []])

function f(x)
  x = sin(x)
  y = cos(x)

  if x > 1
    x = cos(x) + 1
  else
    x = y + 1
  end
  return x
end

ir = @code_ir f(1.0)

deps = dependencies(ir)

@test deps[var(9)] == Set(var.([2, 8, 7, 3, 4, 6]))
@test deps[var(8)] == Set(var.([3, 2, 4]))
@test deps[var(7)] == Set(var.([6, 3, 2]))
@test deps[var(6)] == Set(var.([3, 2]))
@test deps[var(5)] == Set(var.([3, 2]))
@test deps[var(4)] == Set(var.([3, 2]))
@test deps[var(3)] == Set([var(2)])

function pow(x, n)
  r = 1
  while n > 0
    n -= 1
    r *= x
  end
  return r
end

ir = @code_ir pow(1.0, 2)
deps = dependencies(ir)

@test deps[var(8)] == Set(var.([5, 2]))
@test deps[var(7)] == Set(var.([4, 3]))
@test deps[var(6)] == Set(var.([4, 3]))
@test deps[var(5)] == Set(var.([2, 8]))
@test deps[var(4)] == Set(var.([3, 7]))
