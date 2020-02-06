# TODO: https://hal.inria.fr/inria-00558509v1/document
function liveness(ir)
  result = Dict(v => Set{Variable}() for v in keys(ir))
  result = merge(result, Dict(b.id => Set{Variable}() for b in blocks(ir)))
  queue = WorkQueue(1:length(blocks(ir)))
  while !isempty(queue)
    b = block(ir, pop!(queue))
    for br in branches(b)
      foreach(x -> x isa Variable && push!(result[b.id], x), arguments(br))
    end
    live = Set(result[b.id])
    for v in reverse(keys(b))
      varmap(x -> push!(live, x), b[v])
      union!(result[v], live)
      delete!(live, v)
    end
    setdiff!(live, arguments(b))
    for a in predecessors(b)
      isempty(setdiff(live, result[a.id])) && continue
      union!(result[a.id], live)
      push!(queue, a.id)
    end
  end
  return result
end
