using Core.Compiler: LineInfoNode
import Base: push!, insert!, getindex, setindex!, iterate, length

# We have our own versions of these in order to
# (1) be more robust to Base IR changes, and
# (2) make sure that mistakes/bugs do not cause bad LLVM IR.

struct Undefined end
const undef = Undefined()

struct Variable
  id::Int
end

var(id::Integer) = Variable(id)

struct Branch
  condition::Any
  block::Int
  args::Vector{Any}
end

Branch(br::Branch; condition = br.condition,
                   block = br.block, args = br.args) =
  Branch(condition, block, args)

isreturn(b::Branch) = b.block == 0 && length(b.args) == 1
isconditional(b::Branch) = b.condition != nothing

Base.:(==)(a::Branch, b::Branch) =
  (a.condition, a.block, a.args) == (b.condition, b.block, b.args)

arguments(b::Branch) = b.args

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

const stmt = Statement

struct BasicBlock
  stmts::Vector{Statement}
  args::Vector{Any}
  argtypes::Vector{Any}
  branches::Vector{Branch}
end

BasicBlock(stmts = []) = BasicBlock(stmts, [], [], Branch[])

branches(bb::BasicBlock) = bb.branches
arguments(bb::BasicBlock) = bb.args

struct IR
  defs::Vector{Tuple{Int,Int}}
  blocks::Vector{BasicBlock}
  lines::Vector{LineInfoNode}
  meta::Any
end

IR(; meta = nothing) = IR([],[BasicBlock()],[],meta)
IR(lines::Vector{LineInfoNode}; meta = nothing) = IR([],[BasicBlock()],lines,meta)

length(ir::IR) = sum(x -> x[2] > 0, ir.defs)

function block!(ir::IR, i = length(blocks(ir))+1)
  insert!(ir.blocks, i, BasicBlock())
  if i != length(blocks(ir))
    for b in blocks(ir), i = 1:length(branches(b))
      br = branches(b)[i]
      br.block >= i && (branches(b)[i] = Branch(br, block = br.block+1))
    end
  end
  for (ii, (b, j)) = enumerate(ir.defs)
    b >= i && (ir.defs[ii] = (b+1, j))
  end
  return block(ir, i)
end

struct Block
  ir::IR
  id::Int
end

basicblock(b::Block) = b.ir.blocks[b.id]
branches(b::Block) = branches(basicblock(b))
arguments(b::Block) = arguments(basicblock(b))
arguments(ir::IR) = arguments(block(ir, 1))

canbranch(bb::Block) = length(branches(bb)) == 0 || isconditional(branches(bb)[end])

isreturn(b::Block) = any(isreturn, branches(b))

function explicitbranch!(b::Block)
  b.id == 1 && return
  a = block(b.ir, b.id-1)
  if all(isconditional, branches(a))
    branch!(a, b.id)
  end
  return
end

explicitbranch!(ir::IR) = foreach(explicitbranch!, blocks(ir))

function branches(b::Block, c::Block)
  c.id == b.id+1 && explicitbranch!(c)
  filter(br -> br.block == c.id, branches(b))
end

branches(b::Block, c::Integer) = branches(b, block(b.ir, c))

function argument!(b::Block, value = nothing, type = Any; insert = true, at = length(arguments(b))+1)
  if at < length(arguments(b))
    for i = 1:length(b.ir.defs)
      (c, j) = b.ir.defs[i]
      c == b.id && -j >= at && (b.ir.defs[i] = (c, j-1))
    end
  end
  push!(b.ir.defs, (b.id, -at))
  arg = var(length(b.ir.defs))
  insert!(arguments(b), at, arg)
  insert!(basicblock(b).argtypes, at, type)
  if insert
    explicitbranch!(b)
    for c in blocks(b.ir), br in branches(c)
      br.block == b.id && insert!(arguments(br), at, value)
    end
  end
  return arg
end

argument!(ir::IR, a...; kw...) =
  argument!(block(ir, 1), nothing, a...; kw..., insert = false)

function emptyargs!(b::Block)
  empty!(arguments(b))
  for c in blocks(b.ir), br in branches(c)
    br.block == b.id && empty!(arguments(br))
  end
  return
end

function deletearg!(b::Block, i)
  arg = arguments(b)[i]
  deleteat!(arguments(b), i)
  for c in blocks(b.ir), br in branches(c)
    br.block == b.id && deleteat!(arguments(br), i)
  end
  b.ir.defs[arg.id] = (-1, -1)
  return
end

function deletearg!(b::Block, i::AbstractVector)
  for i in sort(i, lt = >)
    deletearg!(b, i)
  end
end

deletearg!(ir::IR, i) = deletearg!(block(ir, 1), i)

block(ir::IR, i) = Block(ir, i)
blocks(ir::IR) = [block(ir, i) for i = 1:length(ir.blocks)]

function blockidx(ir::IR, x::Variable)
  b, i = get(ir.defs, x.id, (-1, -1))
  i > 0 || error("No such variable $x")
  block(ir, b), i
end

function Base.haskey(ir::IR, x::Variable)
  b, i = get(ir.defs, x.id, (-1, -1))
  return i > 0
end

getindex(b::Block, i::Integer) = basicblock(b).stmts[i]
getindex(b::Block, i::Variable) = b.ir[i]
setindex!(b::Block, x::Statement, i::Integer) = (basicblock(b).stmts[i] = x)
setindex!(b::Block, x, i::Integer) = (b[i] = Statement(b[i], x))

branch(block::Integer, args...; unless = nothing) =
  Branch(unless, block, Any[args...])

branch(block::Block, args...; kw...) = branch(block.id, args...; kw...)

function branch!(b::Block, block, args...; unless = nothing)
  push!(branches(b), branch(block, args...; unless = unless))
  return b
end

function branch!(ir::IR, args...; kw...)
  branch!(blocks(ir)[end], args...; kw...)
  return ir
end

return!(ir, x) = branch!(ir, 0, x)

function getindex(ir::IR, i::Variable)
  b, i = blockidx(ir, i)
  return b[i]
end

Base.get(ir::IR, i::Variable, default) = haskey(ir, i) ? ir[i] : default

function setindex!(ir::IR, x, i::Variable)
  b, i = blockidx(ir, i)
  b[i] = x
end

function Base.delete!(ir::IR, i::Variable)
  ir[i] = nothing
  ir.defs[i.id] = (-1, -1)
  return ir
end

length(b::Block) = count(x -> x[1] == b.id, b.ir.defs)

function successors(b::Block)
  brs = basicblock(b).branches
  succs = Int[br.block for br in brs if br.block > 0]
  all(br -> br.condition != nothing, brs) && push!(succs, b.id+1)
  return [block(b.ir, succ) for succ in succs]
end

predecessors(b::Block) = [c for c in blocks(b.ir) if b in successors(c)]

Base.keys(b::Block) = first.(sort([Variable(i) => v for (i, v) in enumerate(b.ir.defs) if v[1] == b.id && v[2] > 0], by = x->x[2]))

function iterate(b::Block, (ks, i) = (keys(b), 1))
  i > length(ks) && return
  return (ks[i]=>b.ir[ks[i]], (ks, i+1))
end

Base.keys(ir::IR) = first.(sort([Variable(i) => v for (i, v) in enumerate(ir.defs) if v[2] > 0], by = x->x[2]))

function iterate(ir::IR, (ks, i) = (keys(ir), 1))
  i > length(ks) && return
  return (ks[i]=>ir[ks[i]], (ks, i+1))
end

applyex(f, x) = x
applyex(f, x::Expr) =
  Expr(x.head, [x isa Expr ? f(x) : x for x in x.args]...)
applyex(f, x::Statement) = Statement(x, applyex(f, x.expr))

function push!(b::Block, x::Statement)
  x = applyex(a -> push!(b, Statement(a, line = x.line)), x)
  x = Statement(x)
  push!(basicblock(b).stmts, x)
  push!(b.ir.defs, (b.id, length(basicblock(b).stmts)))
  return Variable(length(b.ir.defs))
end

push!(b::Block, x) = push!(b, Statement(x))

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
  if after && ir.defs[i.id][2] < 0
    pushfirst!(block(ir, ir.defs[i.id][1]), x)
  else
    b, i = blockidx(ir, i)
    insert!(b, i+after, x)
  end
end

insertafter!(ir, i, x) = insert!(ir, i, x, after=true)

Base.empty(ir::IR) = IR(copy(ir.lines))

function Base.permute!(ir::IR, perm::AbstractVector)
  explicitbranch!(ir)
  permute!(ir.blocks, perm)
  iperm = invperm(perm)
  for v = 1:length(ir.defs)
    b, i = ir.defs[v]
    b == -1 && continue
    ir.defs[v] = (iperm[b], i)
  end
  for b in blocks(ir), i = 1:length(branches(b))
    branches(b)[i].block > 0 || continue
    br = branches(b)[i]
    branches(b)[i] = Branch(br, block = iperm[br.block])
  end
  return ir
end

# Pipe

struct NewVariable
  id::Int
end

mutable struct Pipe
  from::IR
  to::IR
  map::Dict{Any,Any}
  var::Int
end

var!(p::Pipe) = NewVariable(p.var += 1)

substitute!(p::Pipe, x, y) = (p.map[x] = y; x)
substitute(p::Pipe, x::Union{Variable,NewVariable}) = p.map[x]
substitute(p::Pipe, x) = get(p.map, x, x)
substitute(p::Pipe) = x -> substitute(p, x)

function Pipe(ir)
  p = Pipe(ir, IR(copy(ir.lines), meta = ir.meta), Dict(), 0)
  for (x, T) in zip(p.from.blocks[1].args, p.from.blocks[1].argtypes)
    y = argument!(blocks(p.to)[end], nothing, T, insert = false)
    substitute!(p, x, y)
  end
  return p
end

function pipestate(ir::IR)
  ks = sort([Variable(i) => v for (i, v) in enumerate(ir.defs) if v[2] > 0], by = x->x[2])
  [first.(filter(x -> x[2][1] == b, ks)) for b = 1:length(ir.blocks)]
end

function iterate(p::Pipe, (ks, b, i) = (pipestate(p.from), 1, 1))
  if i == 1 && b != 1
    for (x, T) in zip(p.from.blocks[b].args, p.from.blocks[b].argtypes)
      y = argument!(blocks(p.to)[end], nothing, T, insert = false)
      substitute!(p, x, y)
    end
  end
  if i > length(ks[b])
    for br in branches(block(p.from, b))
      push!(p.to.blocks[end].branches, map(substitute(p), br))
    end
    b == length(ks) && return
    block!(p.to)
    return iterate(p, (ks, b+1, 1))
  end
  v = ks[b][i]
  st = p.from[v]
  substitute!(p, v, push!(p.to, prewalk(substitute(p), st)))
  ((v, st), (ks, b, i+1))
end

finish(p::Pipe) = p.to

islastdef(ir::IR, v::Variable) =
  v.id == length(ir.defs) &&
  ir.defs[v.id] == (length(ir.blocks), length(ir.blocks[end].stmts))

setindex!(p::Pipe, x, v) = p.to[substitute(p, v)] = prewalk(substitute(p), x)

function Base.push!(p::Pipe, x)
  tmp = var!(p)
  substitute!(p, tmp, push!(p.to, prewalk(substitute(p), x)))
  return tmp
end

function Base.delete!(p::Pipe, v)
  v′ = substitute(p, v)
  delete!(p.map, v)
  if islastdef(p.to, v′)
    pop!(p.to.defs)
    pop!(p.to.blocks[end].stmts)
  else
    delete!(p.to, v′)
  end
end

function insert!(p::Pipe, v, x; after = false)
  v′ = substitute(p, v)
  x = prewalk(substitute(p), x)
  tmp = var!(p)
  if islastdef(p.to, v′) # we can make this case efficient by renumbering
    if after
      substitute!(p, tmp, push!(p.to, x))
    else
      substitute!(p, v, push!(p.to, p.to[v′]))
      p.to[v′] = Statement(x)
      substitute!(p, tmp, v′)
    end
  else
    substitute!(p, tmp, insert!(p.to, v′, x, after = after))
  end
  return tmp
end

argument!(p::Pipe, a...; kw...) =
  substitute!(p, var!(p), argument!(p.to, a...; kw...))
