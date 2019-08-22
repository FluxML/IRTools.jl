import IRTools.restructure

# utility macro which creates an anonymous function from round-tripped function call
macro round_trip(ex)
    @assert ex.head == :call
    argnames = ntuple(_ -> gensym(), length(ex.args)-1)
    return quote
        eval(:(($($argnames...),) -> $(restructure(@code_ir($ex), $argnames))))
    end
end


function controlflow_1(a, b)
    if a == 10
        x = if b == 22
            7
        else
            8
        end
        for i = 1:100
            x += i
            x -= 77
            i == 77 && continue
            if i == 99 && (i % 2 == 0)
                break
            end
        end
        return x
    else
        return 77
    end
end

apply_vararg2(x...) = +(x...)


@testset "round trips" begin
    @testset "control flow" begin
        round_tripped = @round_trip controlflow_1(0, 0)
        for (x, y) in Iterators.product([1, 10], [22; rand(1:100, 3)])
            @test round_tripped(x, y) == controlflow_1(x, y)
        end
    end
    @testset "complex exp" begin
        # exp was complex enough to break Matcha, so lets test it
        round_tripped = @round_trip exp(0.0 + 0.0im)
        for i = 1:10
            x = rand(ComplexF64)
            @test round_tripped(x) == exp(x)
        end
    end
    @testset "apply vararg" begin
        ex = restructure(@code_ir(apply_vararg2(1, 2, 3)), (:t,))
        round_tripped = eval(:((t...) -> $ex))
        # note that you need to pass a tuple, since that's what it expects
        # TODO, move the construction of the tuple into function body?
        @test apply_vararg2(1, 2, 3) == round_tripped(1, 2, 3)
    end
end


function fortest(x)
    acc = x
    for i = 1:5
        if i == 1
            acc += x
        elseif i == 2
            acc -= x
        else
            acc += x * x
        end
    end
    return acc
end

@testset "fortest" begin
    round_tripped = @round_trip fortest(0)
    for i = 1:10
        x = rand(Int)
        @test round_tripped(x) == fortest(x)
    end
end


function test1(a, b)
    c = a + b
    a == c && (c = a)
    c
end
function test2(a, b)
    c = a + b
    if a == c
        c = a
    end
    c
end

@testset "test1" begin
    round_tripped = @round_trip test1(0, 0)
    x, y = rand(Int, 2)
    @test round_tripped(x, x) == test1(x, x)
    @test round_tripped(x, y) == test1(x, y)
end

@testset "test1" begin
    round_tripped = @round_trip test2(0, 0)
    x, y = rand(Int, 2)
    @test round_tripped(x, x) == test2(x, x)
    @test round_tripped(x, y) == test2(x, y)
end
