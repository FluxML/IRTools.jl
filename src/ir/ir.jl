using Core.Compiler: Argument, SSAValue, PhiNode, GotoNode, GotoIfNot, ReturnNode
import Base: push!, insert!, getindex, setindex!, iterate, length

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

length(bb::BasicBlock) = length(bb.stmts) + length(bb.gotos)

struct IR
  defs::Vector{Tuple{Int,Int}}
  blocks::Vector{BasicBlock}
end

IR() = IR([],[BasicBlock()])

length(ir::IR) = sum(length, ir.blocks)

function block!(ir::IR)
  push!(ir.blocks, BasicBlock())
  return ir
end

struct Block
  ir::IR
  id::Int
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

length(b::Block) = length(b.bb)

function iterate(b::Block, i = 1)
  i > length(b) && return
  el = i <= length(b.bb.stmts) ? b.bb.stmts[i] : b.bb.gotos[i-length(b.bb.stmts)]
  def = findfirst(==((b.id,i)), b.ir.defs)
  def == nothing || (def = SSAValue(def))
  return ((def, el), i+1)
end

function iterate(ir::IR, (b, i) = (1,1))
  b > length(ir.blocks) && return
  r = iterate(block(ir, b), i)
  r == nothing && return iterate(ir, (b+1, 1))
  x, i = r
  return x, (b, i)
end

function push!(b::Block, x)
  x = Statement(x)
  if iscontrol(x.expr)
    push!(b.bb.gotos, x)
    return
  else
    push!(b.bb.stmts, x)
    push!(b.ir.defs, (b.id, length(b.bb.stmts)))
    return SSAValue(length(b.ir.defs))
  end
end

push!(ir::IR, x) = push!(block(ir, length(ir.blocks)), x)
