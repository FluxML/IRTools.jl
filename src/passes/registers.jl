# TODO: https://hal.inria.fr/inria-00558509v1/document
function liveness(ir)
  result = Dict(v => Set{Variable}() for v in keys(ir))
  result = merge(result, Dict(b.id => Set{Variable}() for b in blocks(ir)))
  queue = WorkQueue(1:length(blocks(ir)))
  while !isempty(queue)
    b = block(ir, pop!(queue))
    for br in branches(b)
      foreach(x -> x isa Variable && push!(result[b.id], x), arguments(br))
      br.condition isa Variable && push!(result[b.id], br.condition)
    end
    live = Set(result[b.id])
    for v in reverse(keys(b))
      delete!(live, v)
      varmap(x -> push!(live, x), b[v])
      union!(result[v], live)
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

function interference(liveness)
  g = Graph{Variable}()
  for set in liveness
    set = collect(set)
    for i = 1:length(set), j = i+1:length(set)
      connect!(g, set[i], set[j])
    end
  end
  return g
end

interference(ir::IR) = interference(values(liveness(ir)))

function colouring(g::Graph, pre = Dict())
  nodes = sort(collect(keys(g)), by = x -> length(g[x]), rev = true)
  colours = Dict(n => get(pre, n, -1) for n in nodes)
  for n in nodes
    colour = 1
    while any(m -> colours[m] == colour, g[n])
      colour += 1
    end
    colours[n] = colour
  end
  return colours
end
