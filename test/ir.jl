using IRTools, Test
using IRTools: IR

function f(x)
    N = 4
    for i1 in 1:3    # single loop works without issue
        for i2 in 1:N   # needs to be a variable `1:4` works fine
        end
    end
    0.0 # same error with `x`
end

@test @code_ir(f(1)) isa IR
