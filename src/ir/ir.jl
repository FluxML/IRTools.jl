using Core.Compiler: Argument, SSAValue, PhiNode, GotoNode, GotoIfNot, ReturnNode
import Base: push!, insert!, getindex, setindex!

iscontrol(x) = x isa Union{GotoNode,GotoIfNot,ReturnNode}

struct Statement
  expr::Any
  type::Any
  line::Int
end

Statement(x; type = Any, line = 0) =
  Statement(x, type, line)

Statement(x::Statement, expr = x.expr; type = x.type, line = x.line) =
  Statement(x.expr, type, line)

struct BasicBlock
  stmts::Vector{Statement}
  gotos::Vector{Statement}
end

BasicBlock() = BasicBlock([], [])

struct IR
  defs::Vector{Tuple{Int,Int}}
  blocks::Vector{BasicBlock}
end

IR() = IR([],[BasicBlock()])

function block!(ir::IR)
  push!(ir.blocks, BasicBlock())
  return ir
end

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
  x = Statement(x)
  if iscontrol(x.expr)
    push!(b.bb.gotos, x)
    return
  else
    push!(b.bb.stmts, x)
    push!(b.ir.defs, (b.idx, length(b.bb.stmts)))
    return SSAValue(length(b.ir.defs))
  end
end

push!(ir::IR, x) = push!(block(ir, length(ir.blocks)), x)
