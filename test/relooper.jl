using IRTools, Test
using IRTools: @meta, IR, CFG, Simple, Loop, Multiple, reloop, explicitbranch!

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

# AST recovery

ir = explicitbranch!(IR(@meta(relu(1)), slots = true))
ex = reloop(ir)

@test eval(:(let arg2 = 5; $ex; end)) == 5
@test eval(:(let arg2 = -5; $ex; end)) == 0

ir = explicitbranch!(IR(@meta(pow(1,1)), slots = true))
ex = reloop(ir)

@test eval(:(let arg2 = 5, arg3 = 3; $ex; end)) == 125

ir = explicitbranch!(IR(@meta(gcd(1,1)), slots = true))
ex = reloop(ir)

@test_broken eval(:(let arg2 = 85, arg3 = 391; $ex; end)) == gcd(85, 391)
