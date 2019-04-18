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

function renumber(ir)
  p = Pipe(ir)
  for v in p
    ex = p[v].expr
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
  for b in bs
    branches(b)[end] = branch(length(ir.blocks), arguments(branches(b)[end])[1])
  end
  ret = argument!(blocks(ir)[end])
  return!(ir, ret)
  return ir
end

function merge_entry!(ir)
  @assert isempty(predecessors(block(ir, 1)))
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
    moot(a, in) = length(setdiff(in, (a,))) == 1
    del = map(moot, arguments(b), zip(arguments.(brs)...))
    rename = Dict(zip(arguments(b)[del], arguments(brs[1])[del]))
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
