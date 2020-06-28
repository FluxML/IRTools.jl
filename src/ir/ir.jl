using Core.Compiler: LineInfoNode
import Base: push!, insert!, getindex, setindex!, iterate, length

# We have our own versions of these in order to
# (1) be more robust to Base IR changes, and
# (2) make sure that mistakes/bugs do not cause bad LLVM IR.

struct Undefined end
const undef = Undefined()

"""
    Variable(id::Integer)
    var(id::Integer)

Represents an SSA variable. Primarily used as an index into `IR` objects.
"""
struct Variable
  id::Int
end

var(id::Integer) = Variable(id)



"""
    Branch(condition::Any, block::Int, args::Vector{Any})

Represents a generalized branching instruction, consisting of a `condition` (usually a `Variable` of
boolean type), a target `block`, and a vector of `args` passed to the jump target.  There are three
types of branches that exist by the following convention:

- A return branch is represented by `Branch(nothing, 0, [<return value>])`.
- An unconditional branch is represented by `Branch(nothing, <target>, [<optional arguments ...>])`
- A conditional branch is represented by `Branch(<condition>, <target>, [<optional arguments ...>])`

See also: [`branch!`](@branch!), [`return!`](@return), [`isreturn`](@isreturn),
[`isconditional`](@isconditional)
"""
struct Branch
  condition::Any
  block::Int
  args::Vector{Any}
end

Branch(br::Branch; condition = br.condition,
                   block = br.block, args = br.args) =
  Branch(condition, block, args)

"""
    isreturn(br::Branch)

Check whether `br` has the form of a return branch (see [`Branch`](@Branch)).
"""
isreturn(b::Branch) = b.block == 0 && length(b.args) == 1

"""
    returnvalue(b::Branch)

Get the return value of `b` (the first argument of the branch).
"""
returnvalue(b::Branch) = b.args[1]

"""
    isconditional(br::Branch)

Check whether `br` has the form of a conditional branch (see [`Branch`](@Branch)).
"""
isconditional(b::Branch) = b.condition != nothing

Base.:(==)(a::Branch, b::Branch) =
  (a.condition, a.block, a.args) == (b.condition, b.block, b.args)

Base.copy(br::Branch) = Branch(br.condition, br.block, copy(br.args))

"""
    arguments(br::Branch)

Return the argument vector of the branch `br`.  (These are the arguments passed to a jumped-to
block.)
"""
arguments(b::Branch) = b.args

const unreachable = Branch(nothing, 0, [])



"""
    Statement(expr; type, line)
    stmt(expr; type, line)

Represents a single statement in the IR. The `expr` is a non-nested Julia
expression (`Expr`). `type` represents the return type of the statement; in most
cases this can be ignored and defaults to `Any`. `line` represents the source
location of the statement; it is an integer index into the IR's line table.

As a convenience, if `expr` is already a statement, the new statement will
inherit its type and line number.
"""
struct Statement
  expr::Any
  type::Any
  line::Int
end

Statement(x; expr = x, type = Any, line = 0) =
  Statement(expr, type, line)

"""
    Statement(stmt::Statement; expr, type, line)

Copy of `stmt` with optionally updated fields.
"""
Statement(x::Statement; expr = x.expr, type = x.type, line = x.line) =
  Statement(expr, type, line)

const stmt = Statement


"""
    BasicBlock(stmts::Vector{Statement}, args::Vector{Any},
               argtypes::Vector{Any}, branches::Vector{Branch})
    BasicBlock([stmts])

Represents a [basic
block](https://en.wikipedia.org/wiki/Static_single_assignment_form#Converting_to_SSA) of code in
SSA form.  A block consists of a list of statements, followed by optional branching instructions and
arguments, with optional types.
"""
struct BasicBlock
    stmts::Vector{Statement}
    args::Vector{Any}
    argtypes::Vector{Any}
    branches::Vector{Branch}
end

BasicBlock(stmts = []) = BasicBlock(stmts, [], [], Branch[])

Base.copy(bb::BasicBlock) = BasicBlock(copy(bb.stmts), copy(bb.args), copy(bb.argtypes), copy.(bb.branches))

"""
    branches(bb::BasicBlock)

Return the vector of branching instructions in block `bb`.
"""
branches(bb::BasicBlock) = bb.branches

"""
    arguments(bb::BasicBlock)

Return the argument vector of the basic block `bb`.  (These are the arguments given by the branch
to this block.)
"""
arguments(bb::BasicBlock) = bb.args

"""
    argtypes(bb::BasicBlock)

Return the argument types of the basic block `bb`.  (These are the arguments given by the branch
to this block.)
"""
argtypes(bb::BasicBlock) = bb.argtypes

"""
    IR()
    IR(metadata; slots = false)

Represents a fragment of SSA-form code.

IR can be constructed from scratch, but more usually an existing Julia method is
used as a starting point (see [`meta`](@ref) for how to get metadata for a
method). The `slots` argument determines whether the IR preserves mutable
variable slots; by default, these are converted to SSA-style variables.

As a shortcut, IR can be constructed directly from a type signature, e.g.

    julia> IR(typeof(gcd), Int, Int)
    1: (%1, %2, %3)
      %4 = %2 == 0
      br 4 unless %4
    2: ...

See also: [`code_ir`](@code_ir)
"""
struct IR
  defs::Vector{Tuple{Int,Int}}
  blocks::Vector{BasicBlock}
  lines::Vector{LineInfoNode}
  meta::Any
end

IR(; meta = nothing) = IR([],[BasicBlock()],[],meta)
IR(lines::Vector{LineInfoNode}; meta = nothing) = IR([],[BasicBlock()],lines,meta)

Base.copy(ir::IR) = IR(copy(ir.defs), copy.(ir.blocks), copy(ir.lines), ir.meta)

length(ir::IR) = sum(x -> x[2] > 0, ir.defs)

"""
    block!(ir::IR)
    block!(ir::IR, i)

Insert a new block in `ir`.  If `i` is given, the block is inserted at this position, otherwise it
is appended at the end.  Branches in all other blocks are updated to preserve the original
behaviour.
"""
function block!(ir::IR, i = length(blocks(ir))+1)
  insert!(ir.blocks, i, BasicBlock())
  if i != length(blocks(ir))
    for b in blocks(ir), bi = 1:length(branches(b))
      br = branches(b)[bi]
      br.block >= i && (branches(b)[bi] = Branch(br, block = br.block+1))
    end
    for (ii, (b, j)) = enumerate(ir.defs)
      b >= i && (ir.defs[ii] = (b+1, j))
    end
  end
  return block(ir, i)
end

"""
    deleteblock!(ir::IR, i)

Delete the block at position `i`.  Branches in all other blocks are updated to preserve the original
behaviour.
"""
function deleteblock!(ir::IR, i::Integer)
  deleteat!(ir.blocks, i)
  if i != length(blocks(ir))+1
    for b in blocks(ir), bi = 1:length(branches(b))
      br = branches(b)[bi]
      br.block >= i && (branches(b)[bi] = Branch(br, block = br.block-1))
    end
  end
  for (ii, (b, j)) = enumerate(ir.defs)
    b == i && (ir.defs[ii] = (-1, -1))
    b > i && (ir.defs[ii] = (b-1, j))
  end
  return
end

struct Block
  ir::IR
  id::Int
end


BasicBlock(b::Block) = b.ir.blocks[b.id]
branches(b::Block) = branches(BasicBlock(b))

branches(ir::IR) = length(blocks(ir)) == 1 ? branches(block(ir, 1)) :
  error("IR has multiple blocks, so `branches(ir)` is ambiguous.")

arguments(b::Block) = arguments(BasicBlock(b))
arguments(ir::IR) = arguments(block(ir, 1))

argtypes(b::Block) = argtypes(BasicBlock(b))
argtypes(ir::IR) = argtypes(block(ir, 1))


"""
    canbranch(b::Block)

Check whether adding a branch to block `b` will be valid (i.e, the last existing branch is not
conditional).
"""
canbranch(b::Block) = length(branches(b)) == 0 || isconditional(branches(b)[end])

"""
    isreturn(b::Block)

Check whether the block `b` has a return branch.
"""
isreturn(b::Block) = any(isreturn, branches(b))

"""
    explicitbranch!(b::Block)
    explicitbranch!(ir::IR)

Convert implicit fallthroughs to explicit branches to the next block (these occur when the last
branch in a block is conditional):

    1: (%1, %2)
      %3 = %2 < 0
      br 3 unless %3
    2:
      return 0
    3:
      return %2

will become

    1: (%1, %2)
      %3 = %2 < 0
      br 3 unless %3
      br 2
    2:
      return 0
    3:
      return %2
"""
function explicitbranch!(b::Block)
  b.id == 1 && return
  a = block(b.ir, b.id-1)
  if all(isconditional, branches(a))
    branch!(a, b.id)
  end
  return b
end

explicitbranch!(ir::IR) = (foreach(explicitbranch!, blocks(ir)); return ir)

"""
    branches(b::Block, c::Block)

Return the vector of all branches from block `b` to block `c`.
"""
function branches(b::Block, c::Block)
  c.id == b.id+1 && explicitbranch!(c)
  filter(br -> br.block == c.id, branches(b))
end

branches(b::Block, c::Integer) = branches(b, block(b.ir, c))


"""
    returnvalue(b::Block)

Retreive the return value of a block.

    julia> f(x) = 3x + 2;

    julia> IRTools.block(@code_ir(f(1)), 1)
    1: (%1, %2)
      %3 = 3 * %2
      %4 = %3 + 2
      return %4

    julia> IRTools.returnvalue(ans)
    %4
"""
function returnvalue(b::Block)
  isreturn(branches(b)[end]) || error("Block does not return")
  return returnvalue(branches(b)[end])
end

"""
    returnvalue(b::Block)

Retreive the return type of a block.
"""
returntype(b::Block) = exprtype(b.ir, returnvalue(b))

"""
    argument!(block, [value, type]; at, insert)
    argument!(ir, [value, type]; at, insert)

Create a new argument for the given block / IR fragment, and return the variable
representing the argument.

    julia> ir = IR();

    julia> argument!(ir)
    %1

    julia> ir
    1: (%1)

The `at` keyword argument can be used to specify where the new argument should
go; by default it is appended to the end of the argument list.

Unless `insert = false`, if there are branches to this block, they will be updated to pass `value`
(`nothing` by default) as an argument (by default, `insert = true`).
"""
function argument!(b::Block, value = nothing, t = Any;
                   insert = true, at = length(arguments(b))+1, type = t)
  if at < length(arguments(b))
    for i = 1:length(b.ir.defs)
      (c, j) = b.ir.defs[i]
      c == b.id && -j >= at && (b.ir.defs[i] = (c, j-1))
    end
  end
  push!(b.ir.defs, (b.id, -at))
  arg = var(length(b.ir.defs))
  insert!(arguments(b), at, arg)
  insert!(BasicBlock(b).argtypes, at, type)
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


"""
    emptyargs!(b::Block)

Delete all arguments from block `b`, and automatically remove them from all
branches to this block.
"""
function emptyargs!(b::Block)
  empty!(arguments(b))
  for c in blocks(b.ir), br in branches(c)
    br.block == b.id && empty!(arguments(br))
  end
  return
end

"""
    deletearg!(b::Block, i::Integer)

Delete the `i`-th argument from block `b`, and automatically remove it from all
branches to this block.
"""
function deletearg!(b::Block, i::Integer)
  arg = arguments(b)[i]
  deleteat!(arguments(b), i)
  deleteat!(argtypes(b), i)
  for c in blocks(b.ir), br in branches(c)
    br.block == b.id && deleteat!(arguments(br), i)
  end
  b.ir.defs[arg.id] = (-1, -1)
  for arg in arguments(b)[i:end]
    (bl, pos) = b.ir.defs[arg.id]
    b.ir.defs[arg.id] = (bl, pos+1)
  end
  return
end

function deletearg!(b::Block, i::AbstractVector)
  for i in sort(i, lt = >)
    deletearg!(b, i)
  end
end

"""
    deletearg!(ir::IR, i)

Delete the `i`-th argument from the first block of `ir`, and automatically remove it from all
branches to this block.
"""
deletearg!(ir::IR, i) = deletearg!(block(ir, 1), i)

"""
    block(ir, i)

Return the `i`-th `Block` of `ir`.
"""
block(ir::IR, i) = Block(ir, i)
block(ir::IR, v::Variable) = blockidx(ir, v)[1]

"""
    blocks(ir)

Return the list of blocks `Block` of `ir`.
"""
blocks(ir::IR) = [block(ir, i) for i = 1:length(ir.blocks)]

function blockidx(ir::IR, x::Variable)
  b, i = get(ir.defs, x.id, (-1, -1))
  i > 0 || error("No such variable $x")
  block(ir, b), i
end

"""
    haskey(ir, var)

Check whether the variable `var` was defined in `ir`.

    julia> f(x) = 3x + 2;

    julia> ir = @code_ir f(1)
    1: (%1, %2)
      %3 = 3 * %2
      %4 = %3 + 2
      return %4

    julia> haskey(ir, var(3))
    true

    julia> haskey(ir, var(7))
    false
"""
function Base.haskey(ir::IR, x::Variable)
  b, i = get(ir.defs, x.id, (-1, -1))
  return i > 0
end

getindex(b::Block, i::Integer) = BasicBlock(b).stmts[i]
getindex(b::Block, i::Variable) = b.ir[i]
setindex!(b::Block, x::Statement, i::Integer) = (BasicBlock(b).stmts[i] = x)
setindex!(b::Block, x, i::Integer) = (b[i] = Statement(b[i], expr = x))

branch(block::Integer, args...; unless = nothing) =
  Branch(unless, block, Any[args...])

branch(block::Block, args...; kw...) = branch(block.id, args...; kw...)

"""
    branch!(b::Block, block, args...; unless = nothing)

Add to block `b` a new branch to `block`, with arguments `args` and condition `unless`,
and return it.
"""
function branch!(b::Block, block, args...; unless = nothing)
  brs = branches(b)
  unless === nothing && deleteat!(brs, findall(br -> br.condition === nothing, brs))
  args = map(a -> a isa Expr ? push!(b, a) : a, args)
  push!(brs, branch(block, args...; unless = unless))
  return b
end

"""
    branch!(ir::IR, block, args...; unless = nothing)

Add to the last block of `ir` a new branch to `block`, with arguments `args` and condition `unless`,
and return it.
"""
function branch!(ir::IR, args...; kw...)
  branch!(blocks(ir)[end], args...; kw...)
  return ir
end


"""
    return!(block, x)
    return!(ir, x)

Add to `block` or the last block of `ir` a return branch with argument `x`, and return it.
"""
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

setindex!(b::Block, x, i::Variable) = setindex!(b.ir, x, i)

function Base.delete!(ir::IR, i::Variable)
  ir[i] = nothing
  ir.defs[i.id] = (-1, -1)
  return ir
end

Base.delete!(b::Block, i::Variable) = delete!(b.ir, i)

length(b::Block) = count(x -> x[1] == b.id, b.ir.defs)

"""
    successors(b::Block)

Returns all `Block`s from which you can reach `b` in one jump; basically, all x such that
`branches(x, b)` is non-empty.  Implicit jumps by fall-through are noticed as well.

See: [`predecessors`](@predecessors)
"""
function successors(b::Block)
  brs = BasicBlock(b).branches
  succs = Int[br.block for br in brs if br.block > 0]
  all(br -> br.condition != nothing, brs) && b.id < length(blocks(b.ir)) && push!(succs, b.id+1)
  return [block(b.ir, succ) for succ in succs]
end

"""
    predecessors(b::Block)

Returns all `Block`s of which `b` is a successor; basically, all x such that
`branches(x, b)` is non-empty.  Implicit jumps by fall-through are noticed as well.

See: [`successors`](@successors)
"""
predecessors(b::Block) = [c for c in blocks(b.ir) if b in successors(c)]

"""
    keys(ir)

Return the variable keys for all statements defined in `ir`.

    julia> f(x) = 3x + 2;

    julia> ir = @code_ir f(1)
    1: (%1, %2)
      %3 = 3 * %2
      %4 = %3 + 2
      return %4

    julia> keys(ir)
    2-element Array{IRTools.Variable,1}:
     %3
     %4
"""
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
applyex(f, x::Statement) = Statement(x, expr = applyex(f, x.expr))

"""
    push!(ir, x)

Append the statement or expression `x` to the IR or block `ir`, returning the
new variable. See also [`pushfirst!`](@ref), [`insert!`](@ref).

    julia> ir = IR();

    julia> x = argument!(ir)
    %1

    julia> push!(ir, xcall(:*, x, x))
    %2

    julia> ir
    1: (%1)
      %2 = %1 * %1
"""
function push!(b::Block, x::Statement)
  if !isexpr(x.expr, :foreigncall) # needed to avoid https://github.com/MikeInnes/IRTools.jl/issues/30
    x = applyex(a -> push!(b, Statement(x, expr = a)), x)
  end
  x = Statement(x)
  push!(BasicBlock(b).stmts, x)
  push!(b.ir.defs, (b.id, length(BasicBlock(b).stmts)))
  return Variable(length(b.ir.defs))
end

push!(b::Block, x) = push!(b, Statement(x))

push!(b::Block, x::Variable) = x

# TODO make this work on nested Exprs.
function insert!(b::Block, idx::Integer, x)
  insert!(BasicBlock(b).stmts, idx, Statement(x))
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

"""
    pushfirst!(ir, x)

Insert the expression or statement `x` into the given IR or block at the
beginning, returning the new variable. See also [`push!`](@ref),
[`insert!`](@ref).

    julia> f(x) = 3x + 2
    f (generic function with 1 method)

    julia> ir = @code_ir f(1)
    1: (%1, %2)
      %3 = 3 * %2
      %4 = %3 + 2
      return %4

    julia> pushfirst!(ir, :(println("hello, world")))
    %5

    julia> ir
    1: (%1, %2)
      %5 = println("hello, world")
      %3 = 3 * %2
      %4 = %3 + 2
      return %4
"""
Base.pushfirst!(ir::IR, x) = pushfirst!(block(ir, 1), x)

"""
    insert!(ir, v, x)

Insert the expression or statement `x` into the given IR, just before the
variable `v` is defined, returning the new variable for `x`. See also
[`insertafter!`](@ref).

    julia> f(x) = 3x + 2
    f (generic function with 1 method)

    julia> ir = @code_ir f(1)
    1: (%1, %2)
      %3 = 3 * %2
      %4 = %3 + 2
      return %4

    julia> insert!(ir, IRTools.var(4), :(println("hello, world")))
    %5

    julia> ir
    1: (%1, %2)
      %3 = 3 * %2
      %5 = println("hello, world")
      %4 = %3 + 2
      return %4
"""
function insert!(ir::IR, i::Variable, x; after = false)
  if after && ir.defs[i.id][2] < 0
    pushfirst!(block(ir, ir.defs[i.id][1]), x)
  else
    b, i = blockidx(ir, i)
    insert!(b, i+after, x)
  end
end

insert!(b::Block, i::Variable, x; after = false) =
  insert!(b.ir, i, x; after = after)

"""
    insertafter!(ir, v, x)

Insert the expression or statement `x` into the given IR, just before the
variable `v` is defined, returning the new variable for `x`. See also
[`insert!`](@ref).

    julia> f(x) = 3x + 2
    f (generic function with 1 method)

    julia> ir = @code_ir f(1)
    1: (%1, %2)
      %3 = 3 * %2
      %4 = %3 + 2
      return %4

    julia> IRTools.insertafter!(ir, IRTools.var(4), :(println("hello, world")))
    %5

    julia> ir
    1: (%1, %2)
      %3 = 3 * %2
      %4 = %3 + 2
      %5 = println("hello, world")
      return %4
"""
insertafter!(ir, i, x) = insert!(ir, i, x, after=true)

"""
    empty(ir)

Create an empty IR fragment based on the given IR. The line number table and
any metadata are preserved from the original IR.

    julia> ir = empty(@code_ir gcd(10, 5))
    1:

    julia> ir.meta
    Metadata for gcd(a::T, b::T) where T<:Union{Int128, Int16, Int32, Int64, Int8, UInt128, UInt16, UInt32, UInt64, UInt8} in Base at intfuncs.jl:31
"""
Base.empty(ir::IR) = IR(copy(ir.lines), meta = ir.meta)

"""
    permute!(ir::IR, perm::AbstractVector)

Permutes block order in-place, keeping track of internal references (like branch targets).
"""
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

function IR(b::Block)
  ir = IR(copy(b.ir.defs), [copy(BasicBlock(b))], b.ir.lines, b.ir.meta)
  for i in 1:length(ir.defs)
    if ir.defs[i][1] == b.id
      ir.defs[i] = (1, ir.defs[i][2])
    else
      ir.defs[i] = (-1, -1)
    end
  end
  return ir
end

# Pipe

struct NewVariable
  id::Int
end

"""
    Pipe(ir)

In general, it is not efficient to insert statements into IR; only appending is
fast, for the same reason as with `Vector`s.

For this reason, the `Pipe` construct makes it convenient to incrementally build
an new IR fragment from an old one, making efficient modifications as you go.

The general pattern looks like:

    pr = IRTools.Pipe(ir)
    for (v, st) in pr
      # do stuff
    end
    ir = IRTools.finish(pr)

Iterating over `pr` is just like iterating over `ir`, except that within the
loop, inserting and deleting statements in `pr` around `v` is efficient. Later,
`finish(pr)` converts it back to a normal IR fragment (in this case just a plain
copy of the original).
"""
mutable struct Pipe
  from::IR
  to::IR
  map::Dict{Any,Any}
  var::Int
  branch
end

var!(p::Pipe) = NewVariable(p.var += 1)

substitute!(p::Pipe, x, y) = (p.map[x] = y; x)
substitute(p::Pipe, x::Union{Variable,NewVariable}) = p.map[x]
substitute(p::Pipe, x) = get(p.map, x, x)
substitute(p::Pipe, x::Statement) = stmt(x, expr = substitute(p, x.expr))
substitute(p::Pipe, x::Expr) = Expr(x.head, substitute.((p,), x.args)...)
substitute(p::Pipe) = x -> substitute(p, x)

function Pipe(ir)
  p = Pipe(ir, IR(copy(ir.lines), meta = ir.meta), Dict(), 0, identity)
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

branches(f, p::Pipe) = (p.branch = f)

function iterate(p::Pipe, (ks, b, i) = (pipestate(p.from), 1, 1))
  if i == 1 && b != 1
    for (x, T) in zip(p.from.blocks[b].args, p.from.blocks[b].argtypes)
      y = argument!(blocks(p.to)[end], nothing, T, insert = false)
      substitute!(p, x, y)
    end
  end
  if i > length(ks[b])
    for br in branches(block(p.from, b))
      push!(p.to.blocks[end].branches, map(substitute(p), p.branch(br)))
    end
    b == length(ks) && return
    block!(p.to)
    return iterate(p, (ks, b+1, 1))
  end
  v = ks[b][i]
  st = p.from[v]
  substitute!(p, v, push!(p.to, substitute(p, st)))
  ((v, st), (ks, b, i+1))
end

finish(p::Pipe) = p.to

islastdef(ir::IR, v::Variable) =
  v.id == length(ir.defs) &&
  ir.defs[v.id] == (length(ir.blocks), length(ir.blocks[end].stmts))

setindex!(p::Pipe, x, v) = p.to[substitute(p, v)] = substitute(p, x)

function setindex!(p::Pipe, x::Variable, v)
  v′ = substitute(p, v)
  if islastdef(p.to, v′)
    delete!(p, v)
    substitute!(p, v, substitute(p, x))
  else
    p.to[v′] = substitute(p, x)
  end
end

function Base.push!(p::Pipe, x)
  tmp = var!(p)
  substitute!(p, tmp, push!(p.to, substitute(p, x)))
  return tmp
end

function Base.pushfirst!(p::Pipe, x)
  tmp = var!(p)
  substitute!(p, tmp, pushfirst!(p.to, substitute(p, x)))
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
  x = substitute(p, x)
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

function branch!(ir::Pipe, b, args...; unless = nothing)
  args = map(a -> substitute(ir, a), args)
  cond = substitute(ir, unless)
  branch!(blocks(ir.to)[end], b, args...; unless = cond)
  return ir
end

function block!(p::Pipe)
  block!(p.to)
  return
end

function blockargument!(p::Pipe, type)
  x = argument!(blocks(p.to)[end], nothing, type, insert = false)
  substitute!(p, var!(p), x)
end
