using IRTools, Test
using IRTools: CFG, Simple, Loop, Multiple, reloop

relu(x) = (y = x > 0 ? x : 0)

relu_cfg = CFG(@code_ir relu(1))

@test relu_cfg == CFG([[3,2],[4],[4],[]])

@test reloop(relu_cfg) == Simple(1, Multiple([Simple(3), Simple(2)], Simple(4)))

function pow(x, n)
  r = 1
  while n > 0
    n -= 1
    r *= x
  end
  return r
end

pow_cfg = CFG(@code_ir pow(2, 3))

@test pow_cfg == CFG([[2],[4,3],[2],[]])

@test reloop(pow_cfg) == Simple(1, Loop(Simple(2, Simple(3)), Simple(4)))
