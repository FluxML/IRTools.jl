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

let exports = :[
      # IR
      IR, Block, BasicBlock, Variable, Statement, Branch, Pipe, CFG, branch, var, stmt, arguments, argtypes,
      branches, undef, unreachable, isreturn, isconditional, block!, branch!, argument!, return!,
      canbranch, returnvalue, emptyargs!, deletearg!, block, blocks, successors, predecessors,
      xcall, exprtype, exprline, isexpr, insertafter!, explicitbranch!, prewalk, postwalk,
      prewalk!, postwalk!, finish, substitute!, substitute,
      # Passes/Analysis
      definitions, usages, dominators, domtree, domorder, domorder!, renumber,
      merge_returns!, expand!, prune!, ssa!, inlineable!, log!, pis!, func, evalir,
      Simple, Loop, Multiple, reloop,
      # Reflection, Dynamo
      Meta, TypedMeta, meta, typed_meta, dynamo, transform, refresh, recurse!, self,
      varargs!, slots!,
      ].args
  append!(exports, Symbol.(["@code_ir", "@dynamo", "@meta", "@typed_meta"]))
  for x in exports
    @eval const $x = Inner.$x
  end
  @eval module All
    $([:(import ..IRTools: $x) for x in exports]...)
    export $(exports...)
  end
end

end # module
