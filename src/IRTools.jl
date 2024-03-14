module IRTools

export @code_ir

module Inner

  @nospecialize
  @static if VERSION > v"1.5.0-"
    Base.Experimental.@optlevel 0
  end

  using MacroTools
  using MacroTools: @q, prewalk, postwalk
  import ..IRTools

  export @code_ir

  @nospecialize

  include("reflection/reflection.jl")
  include("utils.jl")

  include("ir/ir.jl")
  include("ir/utils.jl")
  include("ir/wrap.jl")
  include("ir/print.jl")
  include("ir/parse.jl")

  include("reflection/utils.jl")
  include("reflection/dynamo.jl")

  include("passes/passes.jl")
  include("passes/inline.jl")
  include("passes/cps.jl")
  include("passes/relooper.jl")
  include("passes/stackifier.jl")
  include("passes/registers.jl")

  include("interpret.jl")
  include("eval.jl")

end

let exports = :[
      # IR
      IRTools, IR, Block, BasicBlock, Variable, Statement, Branch, Pipe, CFG, Slot, branch, var, stmt, arguments, argtypes,
      branches, undef, unreachable, isreturn, isconditional, block!, deleteblock!, branch!, argument!, return!,
      canbranch, returnvalue, returntype, emptyargs!, deletearg!, block, blocks, successors, predecessors,
      xcall, exprtype, exprline, isexpr, insertafter!, explicitbranch!, prewalk, postwalk,
      prewalk!, postwalk!, finish, substitute!, substitute,
      # Passes/Analysis
      definitions, usages, dominators, domtree, domorder, domorder!, renumber,
      merge_returns!, expand!, prune!, ssa!, inlineable!, log!, pis!, func, evalir,
      Simple, Loop, Multiple, reloop, stackify, functional, cond, WorkQueue,
      Graph, liveness, interference, colouring, inline,
      # Reflection, Dynamo
      Meta, Lambda, meta, dynamo, transform, refresh, recurse!, self,
      varargs!, slots!,
      ].args
  append!(exports, Symbol.(["@code_ir", "@dynamo", "@meta"]))
  for x in exports
    @eval const $x = Inner.$x
  end
  @eval module All
    $([:(import ..IRTools: $x) for x in exports]...)
    export $(exports...)
  end
end

end # module
