module IRTools

export @code_ir

module Inner

  using MacroTools
  using MacroTools: @q, prewalk, postwalk
  import ..IRTools

  export @code_ir

  include("reflection/reflection.jl")

  include("ir/ir.jl")
  include("ir/utils.jl")
  include("ir/wrap.jl")
  include("ir/print.jl")
  include("ir/parse.jl")

  include("reflection/utils.jl")
  include("reflection/dynamo.jl")

  include("passes/passes.jl")
  include("passes/relooper.jl")

  include("interpret.jl")
  include("eval.jl")

  function __init__()
    define_typeinf_code2()
  end

end

"""
    @code_ir f(args...)

Convenience macro similar to `@code_lowered` or `@code_typed`. Retrieves the IR
for the given function call.

    julia> @code_ir gcd(10, 5)
    1: (%1, %2, %3)
      %4 = %2 == 0
      br 4 unless %4
    2: ...
"""
macro code_ir(ex...)
  Inner.code_irm(ex...)
end

"""
    @meta f(args...)

Convenience macro for retrieving metadata without writing a full type signature.

    julia> IRTools.@meta gcd(10, 5)
    Metadata for gcd(a::T, b::T) where T<:Union{Int128, Int16, Int32, Int64, Int8, UInt128, UInt16, UInt32, UInt64, UInt8} in Base at intfuncs.jl:31
"""
macro meta(ex)
  isexpr(ex, :call) || error("@meta f(args...)")
  f, args = ex.args[1], ex.args[2:end]
  :(meta(Inner.typesof($(esc.((f, args...))...))))
end

"""
    @typed_meta f(args...)

Convenience macro for retrieving typed metadata without writing a full type signature.

    julia> IRTools.@typed_meta gcd(10, 5)
    Typed metadata for gcd(a::T, b::T) where T<:Union{Int128, Int16, Int32, Int64, Int8, UInt128, UInt16, UInt32, UInt64, UInt8} in Base at intfuncs.jl:31
"""
macro typed_meta(ex)
  isexpr(ex, :call) || error("@meta f(args...)")
  f, args = ex.args[1], ex.args[2:end]
  :(typed_meta(Inner.typesof($(esc.((f, args...))...))))
end

macro dynamo(ex...)
  Inner.dynamom(__module__, ex...)
end

let exports = :[
  # IR
  IR, Block, BasicBlock, Variable, Statement, Branch, Pipe, CFG, branch, var, stmt, arguments, argtypes,
  branches, undef, unreachable, isreturn, isconditional, block!, branch!, argument!, return!,
  canbranch, returnvalue, emptyargs!, deletearg!, block, blocks, successors, predecessors,
  xcall, exprtype, exprline, isexpr, insertafter!, explicitbranch!,
  # Passes/Analysis
  definitions, usages, dominators, domtree, domorder, domorder!, renumber,
  merge_returns!, expand!, prune!, ssa!, inlineable!, log!, pis!, func, evalir,
  Simple, Loop, Multiple, reloop,
  # Reflection, Dynamo
  Meta, TypedMeta, meta, typed_meta, dynamo, transform, refresh, recurse!, self
  ].args
  for x in exports
    @eval const $x = Inner.$x
  end
  @eval module All
    $([:(import ..IRTools: $x) for x in exports]...)
    export $(exports...)
    import IRTools: @code_ir, @dynamo, @meta, @typed_meta
    export @code_ir, @dynamo, @meta, @typed_meta
  end
end

end # module
