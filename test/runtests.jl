using IRTools, Test

@testset "IRTools" begin

@testset "Reflection" begin
  include("reflection.jl")
end

@testset "Compiler" begin
  include("compiler.jl")
end

end
