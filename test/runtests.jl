using IRTools, Documenter, Test

@testset "IRTools" begin

@testset "Reflection" begin
  include("reflection.jl")
end

@testset "Compiler" begin
  include("compiler.jl")
end

@testset "Tools" begin
    @testset "Relooper" begin
        include("tools/relooper.jl")
    end
end

doctest(IRTools)

end
