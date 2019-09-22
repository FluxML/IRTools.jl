using IRTools, Documenter, Test

@testset "IRTools" begin

@testset "IR" begin
  include("ir.jl")
end

@testset "Analysis" begin
  include("analysis.jl")
end

@testset "Reflection" begin
  include("reflection.jl")
end

@testset "Compiler" begin
  include("compiler.jl")
end

@testset "Relooper" begin
  include("relooper.jl")
end

doctest(IRTools)

end
