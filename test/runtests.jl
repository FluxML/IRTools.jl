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

if Base.GIT_VERSION_INFO.tagged_commit # skip doctests on nightly
    doctest(IRTools)
end

end
