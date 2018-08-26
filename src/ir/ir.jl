using Core.Compiler: Argument, SSAValue
import Base: push!, insert!, getindex, setindex!

struct Statement
  expr::Any
  type::Any
  line::Int
end

Statement(x; type = Any, line = 0) =
  Statement(x, type, line)

Statement(x::Statement; type = x.type, line = x.line) =
  Statement(x.expr, type, line)

struct BasicBlock
  stmts::Vector{Any}
  branches::Vector{Pair{SSAValue,Int}}
end

BasicBlock() = BasicBlock([], [])

struct IR
  defs::Vector{Tuple{Int,Int}}
  blocks::Vector{BasicBlock}
end

IR() = IR([],[BasicBlock()])

struct Block
  ir::IR
  idx::Int
  bb::BasicBlock
end

block(ir::IR, i) = Block(ir, i, ir.blocks[i])
blocks(ir::IR) = [block(ir, i) for i = 1:length(ir.blocks)]

function blockidx(ir::IR, x::SSAValue)
  b, i = ir.defs[x.id]
  block(ir, b), i
end

function getindex(ir::IR, x::SSAValue)
  b, i = blockidx(ir, x)
  b.bb.stmts[i]
end

function push!(b::Block, x)
  push!(b.bb.stmts, Statement(x))
  push!(b.ir.defs, (b.idx, length(b.bb.stmts)))
  return SSAValue(length(b.ir.defs))
end

push!(ir::IR, x) = push!(block(ir, length(ir.blocks)), x)
