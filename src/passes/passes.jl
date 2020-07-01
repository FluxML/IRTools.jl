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

function definitions(b::Block)
  defs = [Variable(i) for i = 1:length(b.ir.defs) if b.ir.defs[i][1] == b.id]
end

function usages(b::Block)
  uses = Set{Variable}()
  prewalk(b) do x
    x isa Variable && push!(uses, x)
    return x
  end
  return uses
end

usages(st::Statement) = usages(st.expr)
usages(ex) = Set{Variable}()

function usages(ex::Expr)
  uses = Set{Variable}()
  for x in ex.args
    x isa Variable && push!(uses, x)
  end
  return uses
end

function block_changes_deps(deps, ir, b)
  for (v, st) in b
    if haskey(deps, v)
      (usages(st) ⊆ deps[v]) || return true
    else
      return true
    end
  end

  brs = branches(b)
  for br in brs
    if br.block > 0
      next_block = block(ir, br.block)
      if !isempty(br.args)
        for (x, y) in zip(arguments(next_block), br.args)
          haskey(deps, x) && (y in deps[x]) && return true
        end
      end
    end
  end
  return false
end

function update_deps!(deps, v, direct)
  set = get!(deps, v, Set{Variable}())
  union!(set, setdiff(direct, (v, )))
  
  for x in direct
    if (v != x) && haskey(deps, x) && !(deps[x] ⊆ set)
      update_deps!(deps, v, deps[x])
    end
  end
  return deps
end

"""
  dependencies(ir::IR)

Return the list of direct dependencies for each variable.
"""
function dependencies(ir::IR)
  worklist = [block(ir, 1)]
  deps = Dict()
  while !isempty(worklist)
    b = pop!(worklist)
    for (v, st) in b
      update_deps!(deps, v, usages(st))
    end

    brs = branches(b)
    jump_next_block = true
    for br in brs
      if br.condition === nothing
        jump_next_block = false
      end

      if br.block > 0 # reachable
        next_block = block(ir, br.block)
        if !isempty(br.args) # pass arguments
          for (x, y) in zip(arguments(next_block), br.args)
            y isa Variable && update_deps!(deps, x, (y, ))
          end
        end

        if block_changes_deps(deps, ir, next_block)
          push!(worklist, next_block)
        end
      end
    end

    if jump_next_block
      next_block = block(ir, b.id + 1)
      if block_changes_deps(deps, ir, next_block)
        push!(worklist, next_block)
      end
    end
  end
  return deps
end

function usecounts(ir::IR)
  counts = Dict{Variable,Int}()
  prewalk(ir) do x
    x isa Variable && (counts[x] = get(counts, x, 0)+1)
    return x
  end
  return counts
end

function dominators(cfg; entry = 1)
  preds = cfg'
  blocks = [1:length(cfg.graph);]
  doms = Dict(b => Set(blocks) for b in blocks)
  while !isempty(blocks)
    b = popfirst!(blocks)
    # We currently special case the first block here,
    # since Julia sometimes creates blocks with no predecessors,
    # which otherwise throw off the analysis.
    ds = isempty(preds[b]) ? Set([b, entry]) :
      push!(intersect([doms[c] for c in preds[b]]...), b)
    if ds != doms[b]
      doms[b] = ds
      for c in cfg[b]
        c in blocks || push!(blocks, c)
      end
    end
  end
  return doms
end

function domtree(cfg::CFG; entry = 1)
  doms = dominators(cfg, entry = entry)
  doms = Dict(b => filter(c -> b != c && b in doms[c], 1:length(cfg)) for b in 1:length(cfg))
  children(b) = filter(c -> !(c in union(map(c -> doms[c], doms[b])...)), doms[b])
  tree(b) = Pair{Int,Any}(b,tree.(children(b)))
  tree(entry)
end

domtree(ir::IR; entry = 1) = domtree(CFG(ir), entry = entry)

function idoms(cfg; entry = 1)
  ds = zeros(Int, length(cfg))
  _idoms((a, bs)) = foreach(((b, cs),) -> (ds[b] = a; _idoms(b=>cs)), bs)
  _idoms(domtree(cfg, entry = entry))
  return ds
end

function domorder(ir, start = 1; full = false)
  tree = domtree(CFG(ir), entry = start)
  flatten((b,cs)) = vcat(b, flatten.(cs)...)
  tree = flatten(tree)
  if full
    for b = 1:length(ir.blocks)
      b in tree || push!(tree, b)
    end
  end
  return tree
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
  worklist = blocks(ir)
  spats = Dict(b => Dict() for b in blocks(ir))
  while !isempty(worklist)
    b = pop!(worklist)
    b.id == 1 && continue
    defs = definitions(b)
    uses = usages(b)
    for v in setdiff(uses, defs)
      haskey(spats[b], v) && continue
      spats[b][v] = argument!(b, v)
      for c in predecessors(b)
        c in worklist || push!(worklist, c)
      end
    end
    ir.blocks[b.id] = prewalk(x -> get(spats[b], x, x), ir.blocks[b.id])
  end
  return ir
end

function prune!(ir::IR)
  usages = usecounts(ir)
  worklist = blocks(ir)
  queue!(b) = (b in worklist || push!(worklist, b))
  function rename!(env, ir)
    for b in blocks(ir)
      prewalk!(b) do x
        haskey(env, x) || return x
        foreach(queue!, successors(b))
        env[x]
      end
    end
  end
  while !isempty(worklist)
    b = popfirst!(worklist)
    isempty(arguments(b)) && continue
    brs = filter(br -> br.block == b.id, [br for a in blocks(ir) for br in branches(a)])
    isempty(brs) && continue
    # Redundant due to all inputs being the same
    inputs = [setdiff(in, (a,)) for (a, in) in zip(arguments(b), zip(arguments.(brs)...))]
    del = findall(x -> length(x) == 1, inputs)
    rename = Dict(zip(arguments(b)[del], first.(inputs[del])))
    if !isempty(rename)
      deletearg!(b, del)
      rename!(rename, ir)
    end
    # Redundant due to not being used
    unused = findall(x -> get(usages, x, 0) == 0, arguments(b))
    if !isempty(unused)
      for a in predecessors(b)
        for br in branches(a, b), i in unused
          arguments(br)[i] isa Variable &&
            (usages[arguments(br)[i]] -= 1)
        end
        a in worklist || push!(worklist, a)
      end
      deletearg!(b, unused)
    end
  end
  return ir
end

function slotsused(bl)
  slots = []
  walk(ex) = prewalk(x -> (x isa Slot && !(x in slots) && push!(slots, x); x), ex)
  for (v, st) in bl
    ex = st.expr
    isexpr(ex, :(=)) ? walk(ex.args[2]) : walk(ex)
  end
  return slots
end

function ssa!(ir::IR)
  current = 1
  defs = Dict(b => Dict{Slot,Any}() for b in 1:length(ir.blocks))
  todo = Dict(b => Dict{Int,Vector{Slot}}() for b in 1:length(ir.blocks))
  catches = Dict()
  handlers = []
  function reaching(b, v)
    haskey(defs[b.id], v) && return defs[b.id][v]
    b.id == 1 && return undef
    x = defs[b.id][v] = argument!(b, type = v.type, insert = false)
    for pred in predecessors(b)
      if pred.id < current
        for br in branches(pred, b)
          push!(br.args, reaching(pred, v))
        end
      else
        push!(get!(todo[pred.id], b.id, Slot[]), v)
      end
    end
    return x
  end
  function catchbranch!(v, slot = nothing)
    for handler in handlers
      args = reaching.((block(ir, v),), catches[handler])
      insertafter!(ir, v, Expr(:catch, handler, args...))
    end
  end
  for b in blocks(ir)
    current = b.id
    rename(ex) = prewalk(x -> x isa Slot ? reaching(b, x) : x, ex)
    for (v, st) in b
      ex = st.expr
      if isexpr(ex, :(=)) && ex.args[1] isa Slot
        defs[b.id][ex.args[1]] = rename(ex.args[2])
        catchbranch!(v, ex.args[1])
        delete!(ir, v)
      elseif isexpr(ex, :enter)
        catches[ex.args[1]] = slotsused(block(ir, ex.args[1]))
        push!(handlers, ex.args[1])
        catchbranch!(v)
      elseif isexpr(ex, :leave) && !haskey(catches, current)
        pop!(handlers)
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

if isdefined(Core.Compiler, :PartialStruct)
  totype(T::Core.Compiler.PartialStruct) = T.typ
end

function pis!(ir::IR)
  for (v, st) in ir
    ex = st.expr
    ex isa PiNode || continue
    ir[v] = xcall(Core, :typeassert, ex.val, totype(ex.typ))
  end
  return ir
end
