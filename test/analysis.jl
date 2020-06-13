using IRTools, Test
using IRTools: CFG, dominators, domtree, dependencies, find_dependency_path, var

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

@test dependencies(ir) == Dict(
  var(9) => Set([var(8), var(7)]),
  var(8) => Set([var(4)]),
  var(7) => Set([var(6)]),
  var(3) => Set([var(2)]),
  var(5) => Set([var(3)]),
  var(6) => Set([var(3)]),
  var(4) => Set([var(3)]),
)

@test find_dependency_path(ir, var(9)) == Set(var.([2, 8, 7, 3, 4, 6]))

# function f(x)
#   x = sin(x)
#   y = cos(x)

#   @info "warning" x

#   if x > 1
#     x = cos(x) + 1
#   else
#     x = y + 1
#   end
#   return x
# end

# ir = @code_ir f(1.0)

# @test find_dependency_path(ir, var(48)) == Set(var.([47, 46, 42, 45, 41, 3, 4, 2]))
