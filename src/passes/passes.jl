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

struct CatchBranch
    defs::Dict{Slot,Any}
    v::Variable
end

function ssa!(ir::IR)
  current = 1
  defs = Dict(b => Dict{Slot,Any}() for b in 1:length(ir.blocks))
  todo = Dict(b => Dict{Int,Vector{Slot}}() for b in 1:length(ir.blocks))
  catch_branches = Dict{Int,Vector{CatchBranch}}()
  handlers = Int[]
  function reaching(b, slot)
    haskey(defs[b.id], slot) && return defs[b.id][slot]
    b.id == 1 && return undef
    x = defs[b.id][slot] = argument!(b, type = slot.type, insert = false)
    for pred in predecessors(b)
      if pred.id < current
        for br in branches(pred, b)
          push!(br.args, reaching(pred, slot))
        end
      else
        push!(get!(todo[pred.id], b.id, Slot[]), slot)
      end
    end

    if haskey(catch_branches, b.id)
      # for each 'catch' branch to this catch block (catch block has `length(predecessors(b)) == 0`),
      # we try to find the dominating definition for slot v.
      # defs[block(ir, cbr.v).id] contains the defs at the end of
      # the block, so we use the cached defs in catch_branches instead.
      for cbr in catch_branches[b.id]
        cbr_v = cbr.v
        stmt = ir[cbr_v]
        if haskey(cbr.defs, slot)
          # Slot v was defined at catch branch
          push!(stmt.expr.args, cbr.defs[slot])
        else
          # Find slot v definition from instruction cbr_v
          b = block(ir, cbr_v)
          if b.id == 1
            push!(stmt.expr.args, undef)
            continue
          end

          # there is already a def for this slot as an argument to the block
          # but which was added after the catch branch.
          if haskey(defs[b.id], slot) && defs[b.id][slot] isa Variable
            bdef = defs[b.id][slot]
            (def_b, loc) = ir.defs[bdef.id]
            if def_b == b.id && loc < 0
              push!(stmt.expr.args, bdef)
              continue
            end
          end

          # get the slot definition from each predecessors of the block owning the catch 'branch'
          new_arg = defs[b.id][slot] = argument!(b; type=slot.type, insert=false)
          push!(stmt.expr.args, new_arg)
          for pred in predecessors(b)
            if pred.id < current
              for br in branches(pred, b)
                push!(br.args, reaching(pred, slot))
              end
            else
              push!(get!(todo[pred.id], b.id, Slot[]), slot)
            end
          end
        end
      end
    end

    return x
  end
  function catchbranch!(v, slot = nothing)
    for handler in handlers
      cbr = CatchBranch(copy(defs[current]), insertafter!(ir, v, Expr(:catch, handler)))
      push!(get!(Vector{CatchBranch}, catch_branches, handler), cbr)
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
        push!(handlers, ex.args[1])
        catchbranch!(v)
      elseif isexpr(ex, :leave) && !haskey(catch_branches, current)
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
