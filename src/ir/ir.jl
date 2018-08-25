using Core.Compiler: Argument, SSAValue

struct Statement
  expr::Any
  type::Any
  line::Int
end

struct Block
  stmts::Vector{Any}
  branches::Vector{Pair{SSAValue,Int}}
end

struct IR
  defs::Vector{Tuple{Int,Int}}
  blocks::Vector{Block}
end
