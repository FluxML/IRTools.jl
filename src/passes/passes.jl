function definitions(b::Block)
  defs = [Variable(i) for i = 1:length(b.ir.defs) if b.ir.defs[i][1] == b.id]
  append!(defs, arguments(b))
end

function usages(b::Block)
  uses = Set{Union{Variable,Argument}}()
  prewalk(b) do x
    x isa Union{Variable,Argument} && push!(uses, x)
    return x
  end
  return uses
end

function dominators(ir)
  doms = Dict(b => Set(blocks(ir)) for b in blocks(ir))
  worklist = blocks(ir)
  while !isempty(worklist)
    b = popfirst!(worklist)
    ds = isempty(predecessors(b)) ? Set([b]) :
      push!(intersect([doms[c] for c in predecessors(b)]...), b)
    if ds != doms[b]
      doms[b] = ds
      for c in successors(b)
        c in worklist || push!(worklist, c)
      end
    end
  end
  return doms
end

function domtree(ir, start = 1)
  doms = dominators(ir)
  doms = Dict(b => filter(c -> b != c && b in doms[c], blocks(ir)) for b in blocks(ir))
  children(b) = filter(c -> !(c in union(map(c -> doms[c], doms[b])...)), doms[b])
  tree(b) = Pair{Int,Any}(b.id,tree.(children(b)))
  tree(block(ir, start))
end

function domorder(ir, start = 1; full = false)
  tree = domtree(ir, start)
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
      if x isa Argument
        @assert x.id <= length(ir.args) "Used argument $x of $(length(ir.args))"
      else
        error("Variable $x in block $(b.id) is not defined.")
      end
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
      substitute!(p, v, ex)
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

function merge_entry!(ir)
  isempty(predecessors(block(ir, 1))) && return ir
  block!(ir, 1)
  return ir
end

function allspats!(ir::IR)
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

function trimspats!(ir::IR)
  worklist = blocks(ir)
  while !isempty(worklist)
    b = popfirst!(worklist)
    isempty(arguments(b)) && continue
    brs = filter(br -> br.block == b.id, [br for a in blocks(ir) for br in branches(a)])
    isempty(brs) && continue
    inputs = [setdiff(in, (a,)) for (a, in) in zip(arguments(b), zip(arguments.(brs)...))]
    del = findall(x -> length(x) == 1, inputs)
    rename = Dict(zip(arguments(b)[del], first.(inputs[del])))
    if !isempty(rename)
      prewalk!(x -> get(rename, x, x), ir)
      deletearg!(b, del)
      for c in successors(b)
        c in worklist || push!(worklist, c)
      end
    end
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

function pis!(ir::IR)
  for (v, st) in ir
    ex = st.expr
    ex isa PiNode || continue
    ir[v] = xcall(Core, :typeassert, ex.val, ex.typ)
  end
  return ir
end
