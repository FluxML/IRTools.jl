using Core.Compiler: Argument, SSAValue, PhiNode, GotoNode, GotoIfNot, ReturnNode, LineInfoNode
import Base: push!, insert!, getindex, setindex!, iterate, length

isgoto(x) = x isa Union{GotoNode,GotoIfNot}
iscontrol(x) = isgoto(x) || x isa ReturnNode
label(x::GotoNode) = x.label
label(x::GotoIfNot) = x.dest

struct Statement
  expr::Any
  type::Any
  line::Int
end

Statement(x; type = Any, line = 0) =
  Statement(x, type, line)

Statement(x::Statement, expr = x.expr; type = x.type, line = x.line) =
  Statement(expr, type, line)

struct BasicBlock
  stmts::Vector{Statement}
  gotos::Vector{Statement}
end

BasicBlock() = BasicBlock([], [])

length(bb::BasicBlock) = length(bb.stmts) + length(bb.gotos)

struct IR
  defs::Vector{Tuple{Int,Int}}
  blocks::Vector{BasicBlock}
  lines::Vector{LineInfoNode}
  args::Vector{Any}
end

IR() = IR([],[BasicBlock()],[],[])
IR(lines::Vector{LineInfoNode},args) = IR([],[BasicBlock()],lines,args)

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

getindex(b::Block, i::Integer) = b.bb.stmts[i]
setindex!(b::Block, x::Statement, i::Integer) = (b.bb.stmts[i] = x)
setindex!(b::Block, x, i::Integer) = (b[i] = Statement(b[i], x))

function getindex(ir::IR, i::SSAValue)
  b, i = blockidx(ir, i)
  return b[i]
end

function setindex!(ir::IR, x, i::SSAValue)
  b, i = blockidx(ir, i)
  b[i] = x
end

length(b::Block) = length(b.bb)

function successors(b::Block)
  gotos = [st.expr for st in b.bb.gotos]
  succs = Int[label(x) for x in filter(isgoto, gotos)]
  all(x -> x isa GotoIfNot, gotos) && push!(succs, b.id+1)
  return succs
end

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

function insert!(b::Block, idx::Integer, x)
  insert!(b.bb.stmts, idx, Statement(x))
  for i = 1:length(b.ir.defs)
    c, j = b.ir.defs[i]
    if c == b.id && j >= idx
      b.ir.defs[i] = (c, j+1)
    end
  end
  push!(b.ir.defs, (b.id, idx))
  return SSAValue(length(b.ir.defs))
end

Base.pushfirst!(b::Block, x) = insert!(b, 1, x)

push!(ir::IR, x) = push!(block(ir, length(ir.blocks)), x)

Base.pushfirst!(ir::IR, x) = pushfirst!(block(ir, 1), x)

function insert!(ir::IR, i::SSAValue, x; after = false)
  b, i = blockidx(ir, i)
  insert!(b, i+after, x)
end

insertafter!(ir::IR, i::SSAValue, x) = insert!(ir, i, x, after=true)

Base.keys(ir::IR) = first.(sort([SSAValue(i) => v for (i, v) in enumerate(ir.defs)], by = x->x[2]))
