using IRTools, Test
using IRTools: Meta, meta

@generated f(x) = :(x+x)

@test meta(Tuple{typeof(gcd),Int,Int}) isa Meta

@test meta(Tuple{typeof(f),Int}) isa Meta

# https://github.com/FluxML/IRTools.jl/issues/106
function g end
@test meta(Tuple{typeof(g),Int,Int}) === nothing
g(a) = 2a
@test meta(Tuple{typeof(g),Int,Int}) === nothing

@test @code_ir(map([1,2], [3,4]) do x, y
  x + y
end) isa IR

# https://github.com/FluxML/IRTools.jl/issues/103
kwf(a, b; c = 1) = a + b + c
@test @code_ir(kwf(1, 2; c = 3)) isa IR
@test @code_ir(kwf(1, 2, c = 3)) isa IR
let c = 3
  @test @code_ir(kwf(1, 2; c)) isa IR
  @test @code_ir(kwf(1, 2; (; c)...)) isa IR
end
@test length(IRTools.arguments(@code_ir(kwf(1, 2; c = 3)))) == 5
@test IRTools.@meta(kwf(1, 2; c = 3)).method == only(methods(Core.kwcall, Tuple{NamedTuple{(:c,),Tuple{Int}},typeof(kwf),Int,Int}))
kwh(f, a; c = 1) = f(a) + c
@test @code_ir(kwh(1; c = 2) do x; x; end) isa IR

# code_ir
@test IRTools.code_ir(gcd, Tuple{Int,Int}) isa IR

# https://github.com/FluxML/IRTools.jl/issues/31
let f3(x) = (y = x + 1; sin(2y)), f4(a, b = 2) = a + b
  for slots in (false, true)
    @test IRTools.argnames(IR(typeof(f3), Int; slots)) == [Symbol("#self#"), :x]
  end
  @test IRTools.argnames(IR(typeof(f4), Int, Int)) == [Symbol("#self#"), :a, :b]
  @test IRTools.argnames(@code_ir f4(1, 2)) == [Symbol("#self#"), :a, :b]
  ir = IR(typeof(f3), Int)
  IRTools.argument!(ir)
  @test IRTools.argnames(ir) == [Symbol("#self#"), :x, Symbol("")]
  @test IRTools.argnames(IR()) === nothing
end
