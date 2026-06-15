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

# Doctests print Julia IR, whose exact form is compiler-version-specific, so
# only run them on the minimum supported Julia version (matching the docs CI job).
if v"1.10" <= VERSION < v"1.11"
    doctest(IRTools)
end

end
