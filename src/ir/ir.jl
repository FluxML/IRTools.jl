using Core.Compiler: LineInfoNode
import Base: push!, insert!, getindex, setindex!, iterate, length

# We have our own versions of these in order to
# (1) be more robust to Base IR changes, and
# (2) make sure that mistakes/bugs do not cause bad LLVM IR.

struct Variable
  id::Int
end

var(id::Integer) = Variable(id)

struct Argument
  id::Int
end

const Reference = Union{Variable,Argument}

struct Branch
  condition::Union{Reference,Nothing}
  block::Int
  args::Vector{Reference}
end

Branch(br::Branch; condition = br.condition,
                   block = br.block, args = br.args) =
  Branch(condition, block, args)

isreturn(b::Branch) = b.block == 0 && length(b.args) == 1

const unreachable = Branch(nothing, 0, [])

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
  args::Vector{Variable}
  branches::Vector{Branch}
end

BasicBlock(stmts = []) = BasicBlock(stmts, [], [])

length(bb::BasicBlock) = length(bb.stmts)

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
end

basicblock(b::Block) = b.ir.blocks[b.id]

block(ir::IR, i) = Block(ir, i)
blocks(ir::IR) = [block(ir, i) for i = 1:length(ir.blocks)]

function blockidx(ir::IR, x::Variable)
  b, i = get(ir.defs, x.id, (-1, -1))
  b == -1 && error("No such variable $x")
  block(ir, b), i
end

getindex(b::Block, i::Integer) = basicblock(b).stmts[i]
setindex!(b::Block, x::Statement, i::Integer) = (basicblock(b).stmts[i] = x)
setindex!(b::Block, x, i::Integer) = (b[i] = Statement(b[i], x))

function getindex(ir::IR, i::Variable)
  b, i = blockidx(ir, i)
  return b[i]
end

function setindex!(ir::IR, x, i::Variable)
  b, i = blockidx(ir, i)
  b[i] = x
end

function Base.delete!(ir::IR, i::Variable)
  ir.defs[i.id] = (-1, -1)
  return ir
end

length(b::Block) = length(basicblock(b))

function successors(b::Block)
  brs = basicblock(b).branches
  succs = Int[br.block for br in brs if br.block > 0]
  all(br -> br.condition != nothing, brs) && push!(succs, b.id+1)
  return succs
end

function iterate(b::Block, i = 1)
  i > length(b) && return
  el = basicblock(b).stmts[i]
  def = findfirst(==((b.id,i)), b.ir.defs)
  def == nothing && return iterate(b, i+1)
  def = Variable(def)
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
  push!(basicblock(b).stmts, x)
  push!(b.ir.defs, (b.id, length(basicblock(b).stmts)))
  return Variable(length(b.ir.defs))
end

function insert!(b::Block, idx::Integer, x)
  insert!(basicblock(b).stmts, idx, Statement(x))
  for i = 1:length(b.ir.defs)
    c, j = b.ir.defs[i]
    if c == b.id && j >= idx
      b.ir.defs[i] = (c, j+1)
    end
  end
  push!(b.ir.defs, (b.id, idx))
  return Variable(length(b.ir.defs))
end

Base.pushfirst!(b::Block, x) = insert!(b, 1, x)

push!(ir::IR, x) = push!(block(ir, length(ir.blocks)), x)

Base.pushfirst!(ir::IR, x) = pushfirst!(block(ir, 1), x)

function insert!(ir::IR, i::Variable, x; after = false)
  b, i = blockidx(ir, i)
  insert!(b, i+after, x)
end

insertafter!(ir::IR, i::Variable, x) = insert!(ir, i, x, after=true)

Base.keys(ir::IR) = first.(sort([Variable(i) => v for (i, v) in enumerate(ir.defs)], by = x->x[2]))
