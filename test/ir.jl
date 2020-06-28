using IRTools, Test
using IRTools: IR, @dynamo

function f(x)
    N = 4
    for i1 in 1:3    # single loop works without issue
        for i2 in 1:N   # needs to be a variable `1:4` works fine
        end
    end
    0.0 # same error with `x`
end

@test @code_ir(f(1)) isa IR

function f(a, b)
    u = 1
    while true
        if true
        end
    end
    f(u)
end

@test @code_ir(f(1, 2)) isa IR

# issue 30
@dynamo function foo(a...)
    ir = IR(a...)
    return ir
end

mylog2(x) = ccall((:log2, Base.Math.libm), Float64, (Float64,), x)

@test foo(mylog2, 3.3) === mylog2(3.3)



function mm3x3!(C, A, B)
    A11 = A[1,1]; A12 = A[1,2]; A13 = A[1,3];
    A21 = A[2,1]; A22 = A[2,2]; A23 = A[2,3];
    A31 = A[3,1]; A32 = A[3,2]; A33 = A[3,3];

    B11 = B[1,1]; B12 = B[1,2]; B13 = B[1,3];
    B21 = B[2,1]; B22 = B[2,2]; B23 = B[2,3];
    B31 = B[3,1]; B32 = B[3,2]; B33 = B[3,3];

    C[1,1] = A11*B11 + A12*B21 + A13*B31
    C[1,2] = A11*B12 + A12*B22 + A13*B32
    C[1,3] = A11*B13 + A12*B23 + A13*B33

    C[2,1] = A21*B11 + A22*B21 + A23*B31
    C[2,2] = A21*B12 + A22*B22 + A23*B32
    C[2,3] = A21*B13 + A22*B23 + A23*B33

    C[3,1] = A31*B11 + A32*B21 + A33*B31
    C[3,2] = A31*B12 + A32*B22 + A33*B32
    C[3,3] = A31*B13 + A32*B23 + A33*B33
end