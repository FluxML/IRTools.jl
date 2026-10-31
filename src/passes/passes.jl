struct CFG
  graph::Vector{Vector{Int}}
end

function CFG(ir::IR)
  graph = Vector{Int}[]
  for b in blocks(ir)
    push!(graph, Int[])
    for c in successors(b)
      push!(graph[end], c.id)
    end
  end
  return CFG(graph)
end

Base.:(==)(a::CFG, b::CFG) = a.graph == b.graph

Base.length(c::CFG) = length(c.graph)
Base.getindex(c::CFG, b::Integer) = c.graph[b]

function Base.transpose(cfg::CFG)
  cfg′ = CFG([Int[] for _ = 1:length(cfg)])
  for i = 1:length(cfg), j in cfg[i]
    push!(cfg′[j], i)
  end
  return cfg′
end

Base.adjoint(cfg::CFG) = transpose(cfg)

definitions(b::Block) = Variable[arguments(b); keys(b)]

function usages(b::Block)
  uses = Set{Variable}()
  prewalk(b) do x
    x isa Variable && push!(uses, x)
    return x
  end
  return uses
end

function usecounts(ir::IR)
  counts = Dict{Variable,Int}()
  prewalk(ir) do x
    x isa Variable && (counts[x] = get(counts, x, 0)+1)
    return x
  end
  return counts
end

# The immediate dominator of each block, or 0 for `entry` and the blocks it can't reach.
# Cooper, Harvey & Kennedy, "A Simple, Fast Dominance Algorithm".
function idoms(cfg; entry = 1)
  n = length(cfg)
  preds = cfg'
  # Julia sometimes creates blocks with no predecessors, which would otherwise
  # be unreachable; treat them as successors of `entry`.
  roots = filter(b -> b != entry && isempty(preds[b]), 1:n)
  succs = [b == entry ? [cfg[b]; roots] : cfg[b] for b = 1:n]
  foreach(b -> push!(preds[b], entry), roots)
  post = Int[]
  index = zeros(Int, n)
  visited = falses(n)
  visited[entry] = true
  stack = [(entry, 1)]
  while !isempty(stack)
    b, i = stack[end]
    if i <= length(succs[b])
      stack[end] = (b, i+1)
      c = succs[b][i]
      visited[c] || (visited[c] = true; push!(stack, (c, 1)))
    else
      pop!(stack)
      push!(post, b)
      index[b] = length(post)
    end
  end
  ds = zeros(Int, n)
  ds[entry] = entry
  function meet(a, b)
    while a != b
      while index[a] < index[b]; a = ds[a]; end
      while index[b] < index[a]; b = ds[b]; end
    end
    return a
  end
  changed = true
  while changed
    changed = false
    for b in Iterators.reverse(post)
      b == entry && continue
      d = 0
      for p in preds[b]
        ds[p] == 0 && continue
        d = d == 0 ? p : meet(p, d)
      end
      ds[b] == d || (ds[b] = d; changed = true)
    end
  end
  ds[entry] = 0
  return ds
end

# The children of each block in the dominator tree, in increasing order.
function domchildren(cfg; entry = 1)
  ds = idoms(cfg, entry = entry)
  cs = [Int[] for _ = 1:length(cfg)]
  for b = 1:length(cfg)
    ds[b] == 0 || push!(cs[ds[b]], b)
  end
  return cs
end

function preorder(cs, entry)
  order = Int[]
  stack = [entry]
  while !isempty(stack)
    b = pop!(stack)
    push!(order, b)
    append!(stack, Iterators.reverse(cs[b]))
  end
  return order
end

function dominators(cfg; entry = 1)
  cs = domchildren(cfg, entry = entry)
  doms = Dict(entry => Set([entry]))
  for b in preorder(cs, entry), c in cs[b]
    doms[c] = push!(copy(doms[b]), c)
  end
  # Blocks that `entry` can't reach are dominated by every block.
  for b = 1:length(cfg)
    haskey(doms, b) || (doms[b] = Set(1:length(cfg)))
  end
  return doms
end

function domtree(cfg::CFG; entry = 1)
  cs = domchildren(cfg, entry = entry)
  tree(b) = Pair{Int,Any}(b,tree.(cs[b]))
  tree(entry)
end

domtree(ir::IR; entry = 1) = domtree(CFG(ir), entry = entry)

function domorder(ir, start = 1; full = false)
  cs = domchildren(CFG(ir), entry = start)
  order = preorder(cs, start)
  if full
    seen = BitSet(order)
    append!(order, filter(b -> !(b in seen), 1:length(ir.blocks)))
  end
  return order
end

domorder!(ir::IR, start = 1) = permute!(ir, domorder(ir, start, full = true))

function verify(ir::IR)
  @assert issorted(domorder(ir)) "Blocks are not in domtree order."
  doms = dominators(ir)
  # TODO check definitions within a block
  for (b, ds) in doms
    defs = union(definitions.(ds)...)
    for x in setdiff(usages(b), defs)
      error("Variable $x in block $(b.id) is not defined.")
    end
  end
  return
end

function renumber(ir)
  p = Pipe(ir)
  for (v, st) in p
    ex = st.expr
    if isbits(ex) # Trivial expressions can be inlined
      delete!(p, v)
      substitute!(p, v, substitute(p, ex))
    end
  end
  return finish(p)
end

function merge_returns!(ir)
  bs = [b for b in blocks(ir) if isreturn(b)]
  length(bs) == 1 && bs[1] == blocks(ir)[end] && return ir
  block!(ir)
  ret = argument!(blocks(ir)[end])
  return!(ir, ret)
  for b in bs
    branches(b)[end] = branch(length(ir.blocks), arguments(branches(b)[end])[1])
  end
  return ir
end

function expand!(ir::IR)
  worklist = collect(1:length(ir.blocks))
  queued = trues(length(ir.blocks))
  spats = [Dict() for _ in ir.blocks]
  preds = predecessorids(ir)
  while !isempty(worklist)
    id = pop!(worklist)
    queued[id] = false
    id == 1 && continue
    b = block(ir, id)
    defs = definitions(b)
    uses = usages(b)
    for v in setdiff(uses, defs)
      haskey(spats[id], v) && continue
      spats[id][v] = argument!(b, insert = false)
      explicitbranch!(b)
      foreach(br -> push!(arguments(br), v), branchesto(b, preds[id]))
      for c in preds[id]
        queued[c] || (queued[c] = true; push!(worklist, c))
      end
    end
    ir.blocks[id] = prewalk(x -> get(spats[id], x, x), ir.blocks[id])
  end
  return ir
end

# Whether `prewalk` can reach a key of `env` (which holds only variables) in `x`,
# checked without rebuilding `x`.
function mayuse(env, @nospecialize(x))
  x isa Variable && return haskey(env, x)
  x isa Expr && return any(a -> mayuse(env, a), x.args)
  x isa PhiNode && return any(i -> isassigned(x.values, i) && mayuse(env, x.values[i]), eachindex(x.values))
  x isa PiNode && return mayuse(env, x.val)
  return x isa IR
end

# The blocks whose statements or branches may use each variable.
function userblocks(ir::IR)
  users = Dict{Variable,BitSet}()
  for b in blocks(ir), v in usages(b)
    push!(get!(BitSet, users, v), b.id)
  end
  return users
end

function prune!(ir::IR)
  usages = usecounts(ir)
  users = userblocks(ir)
  preds = predecessorids(ir)
  worklist = blocks(ir)
  queued = trues(length(ir.blocks))
  queue!(b) = queued[b.id] || (queued[b.id] = true; push!(worklist, b))
  # Called once per block with redundant arguments, so only visit the blocks that may use
  # `env` (whose uses move to the replacements), and only rebuild what does.
  function rename!(env, ir)
    bs = BitSet()
    for (y, x) in env
      haskey(users, y) || continue
      union!(bs, users[y])
      varmap(v -> (union!(get!(BitSet, users, v), users[y]); v), x)
    end
    for id in bs
      b = block(ir, id)
      bb = BasicBlock(b)
      f(x) = x isa IR ? x : prewalk(x) do y
        y isa Variable && haskey(env, y) || return y
        foreach(queue!, successors(b))
        env[y]
      end
      for (i, st) in enumerate(bb.stmts)
        mayuse(env, st.expr) && (bb.stmts[i] = Statement(st, expr = f(st.expr)))
      end
      for (i, br) in enumerate(bb.branches)
        mayuse(env, br.condition) || any(a -> mayuse(env, a), br.args) || continue
        bb.branches[i] = Branch(br, condition = f(br.condition), args = map(f, br.args))
      end
    end
  end
  while !isempty(worklist)
    b = popfirst!(worklist)
    queued[b.id] = false
    isempty(arguments(b)) && continue
    brs = branchesto(b, preds[b.id])
    isempty(brs) && continue
    # Redundant due to all inputs being the same
    inputs = [setdiff(in, (a,)) for (a, in) in zip(arguments(b), zip(arguments.(brs)...))]
    del = findall(x -> length(x) == 1, inputs)
    rename = Dict(zip(arguments(b)[del], first.(inputs[del])))
    if !isempty(rename)
      deletearg!(b, del, brs)
      rename!(rename, ir)
    end
    # Redundant due to not being used
    unused = findall(x -> get(usages, x, 0) == 0, arguments(b))
    if !isempty(unused)
      brs = branchesto(b, preds[b.id]) # `rename!` may have replaced them
      for br in brs, i in unused
        arguments(br)[i] isa Variable && (usages[arguments(br)[i]] -= 1)
      end
      foreach(a -> queue!(block(ir, a)), preds[b.id])
      deletearg!(b, unused, brs)
    end
  end
  return ir
end

struct CatchBranch
    defs::Dict{Slot,Any}
    v::Variable
end

# Definedness of a slot's reaching value `r`, for translating `Expr(:isdefined, slot)`.
# A possibly-undefined slot becomes a block argument that carries the `Undefined()`
# sentinel at run time when the slot was never assigned, so:
#   * `r === undef`   -- statically never defined (e.g. reached the entry block with
#                        no definition)             -> `false`
#   * `r isa Variable` -- an SSA value or block argument that may be the sentinel
#                        at run time                 -> `!(r isa Undefined)`
#   * otherwise       -- a concrete constant, always defined -> `true`
definedness(r) =
  r === undef ? false :
  r isa Variable ? xcall(:!, xcall(Core, :isa, r, Undefined)) :
  true

# Number of handlers popped by a `leave`: a count before Julia 1.11, and one
# `enter` token per handler since.
nleave(ex) = ex.args[1] isa Integer ? ex.args[1] : count(!isnothing, ex.args)

# The stack of active exception handlers on entry to each (reachable) block. This
# follows the CFG rather than the block order, since a `try` region can have
# several exits (e.g. an early `return` or `break`), each with its own `leave`.
# Catch blocks start with the stack of their `enter`, minus their own handler (so
# the `leave` with which they start before Julia 1.11 is skipped).
function handler_stacks(ir::IR, catches)
  stacks = Dict(1 => Int[])
  queue = [1]
  while !isempty(queue)
    b = block(ir, pop!(queue))
    handlers = copy(stacks[b.id])
    for (_, st) in b
      ex = st.expr
      if isexpr(ex, :enter)
        haskey(stacks, ex.args[1]) || (stacks[ex.args[1]] = copy(handlers); push!(queue, ex.args[1]))
        push!(handlers, ex.args[1])
      elseif isexpr(ex, :leave) && !(b.id in catches)
        foreach(_ -> pop!(handlers), 1:nleave(ex))
      end
    end
    for c in successors(b)
      haskey(stacks, c.id) && continue
      stacks[c.id] = copy(handlers)
      push!(queue, c.id)
    end
  end
  return stacks
end

function ssa!(ir::IR)
  current = 1
  defs = Dict(b => Dict{Slot,Any}() for b in 1:length(ir.blocks))
  todo = Dict(b => Dict{Int,Vector{Slot}}() for b in 1:length(ir.blocks))
  catch_branches = Dict{Int,Vector{CatchBranch}}()
  catches = Set{Int}(st.expr.args[1] for (_, st) in ir if isexpr(st.expr, :enter))
  stacks = handler_stacks(ir, catches)
  preds = predecessorids(ir)
  handlers = Int[]
  function reaching(b, slot)
    haskey(defs[b.id], slot) && return defs[b.id][slot]
    b.id == 1 && return undef
    x = add_slot_argument!(b, slot)
    return x
  end
  function add_slot_argument!(b, slot)
    x = argument!(b, type = slot.type, insert = false)
    if !haskey(defs[b.id], slot)
      defs[b.id][slot] = x
    end

    for pred in block.((ir,), preds[b.id])
      if pred.id < current
        for br in branches(pred, b)
          push!(br.args, reaching(pred, slot))
        end
      else
        push!(get!(Vector{Slot}, todo[pred.id], b.id), slot)
      end
    end

    if haskey(catch_branches, b.id)
      # for each 'catch' branch to this catch block (catch block has `length(predecessors(b)) == 0`),
      # we try to find the dominating definition for slot v.
      # defs[block(ir, cbr.v).id] contains the defs at the end of
      # the block, so we use the cached defs in catch_branches instead.
      for cbr in catch_branches[b.id]
        # Get the definition for a slot from a given catch branch.
        cbr_v = cbr.v
        stmt = ir[cbr_v]
        if haskey(cbr.defs, slot)
          # Slot v was defined at catch branch
          push!(stmt.expr.args, cbr.defs[slot])
        else
          # Find slot v definition from instruction cbr_v
          cbr_b = block(ir, cbr_v)
          if cbr_b.id == 1
            push!(stmt.expr.args, undef)
            continue
          end

          # there is already a def for this slot as an argument to the block
          # but which was added after the catch branch.
          if haskey(defs[cbr_b.id], slot) && defs[cbr_b.id][slot] isa Variable
            bdef = defs[cbr_b.id][slot]
            (def_b, loc) = ir.defs[bdef.id]
            if def_b == cbr_b.id && loc < 0
              push!(stmt.expr.args, bdef)
              continue
            end
          end

          # add an argument to cbr_b for slot
          new_arg = add_slot_argument!(cbr_b, slot)
          push!(stmt.expr.args, new_arg)
        end
      end
    end

    return x
  end
  function catchbranch!(v)
    for handler in handlers
      cbr = CatchBranch(copy(defs[current]), insertafter!(ir, v, Expr(:catch, handler)))
      push!(get!(Vector{CatchBranch}, catch_branches, handler), cbr)
    end
  end
  for b in blocks(ir)
    current = b.id
    handlers = copy(get(stacks, b.id, Int[]))
    rename(ex) = prewalk(ex) do x
      if x isa Slot
        reaching(b, x)
      elseif isexpr(x, :isdefined) && length(x.args) == 1 && x.args[1] isa Slot
        # `isdefined` must not be renamed like an ordinary use: substituting the
        # slot with its reaching value discards the definedness information.
        definedness(reaching(b, x.args[1]))
      elseif isexpr(x, :isdefined) && length(x.args) == 1 && x.args[1] isa Variable
        # An argument slot is already lowered to a variable by the time we get here
        # (see `IR(::Meta)`), so `isdefined` of it arrives pre-substituted; recover
        # the definedness test the same way.
        definedness(x.args[1])
      else
        x
      end
    end
    for (v, st) in b
      ex = st.expr
      if isexpr(ex, :(=)) && ex.args[1] isa Slot
        defs[b.id][ex.args[1]] = rename(ex.args[2])
        catchbranch!(v)
        delete!(ir, v)
      elseif isexpr(ex, :enter)
        push!(handlers, ex.args[1])
        catchbranch!(v)
      elseif isexpr(ex, :leave) && !(current in catches)
        foreach(_ -> pop!(handlers), 1:nleave(ex))
      else
        ir[v] = rename(ex)
      end
    end
    for i = 1:length(BasicBlock(b).branches)
      BasicBlock(b).branches[i] = rename(BasicBlock(b).branches[i])
    end
    for (succ, ss) in todo[b.id], br in branches(b, succ)
      append!(br.args, [reaching(b, v) for v in ss])
    end
  end
  return ir
end

function reachable_blocks(cfg::CFG)
  bs = Int[]
  reaches(b) = b in bs || (push!(bs, b); reaches.(cfg[b]))
  reaches(1)
  return bs
end

function trimblocks!(ir::IR)
  for b in sort(setdiff(1:length(blocks(ir)), reachable_blocks(CFG(ir))), rev = true)
    deleteblock!(ir, b)
  end
  return ir
end

function inlineable!(ir)
  pushfirst!(ir, Expr(:meta, :inline))
  return ir
end

function log!(ir, msg)
  pushfirst!(ir, xcall(Core, :println, msg))
  return ir
end

totype(T::Type) = T
totype(T::Core.Compiler.PartialStruct) = T.typ

function pis!(ir::IR)
  for (v, st) in ir
    ex = st.expr
    ex isa PiNode || continue
    ir[v] = xcall(Core, :typeassert, ex.val, totype(ex.typ))
  end
  return ir
end
