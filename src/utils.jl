# Unique Work Queue

struct WorkQueue{T}
  items::Vector{T}
end

WorkQueue{T}() where T = WorkQueue(T[])

WorkQueue() = WorkQueue{Any}()

WorkQueue(xs) = WorkQueue(collect(xs))

function Base.push!(q::WorkQueue, x)
  i = findfirst(==(x), q.items)
  i === nothing || (deleteat!(q.items, i))
  push!(q.items, x)
  return q
end

Base.pop!(q::WorkQueue) = pop!(q.items)
Base.isempty(q::WorkQueue) = isempty(q.items)

# Simple Graph

struct Graph{T}
  nodes::Dict{T,Int}
  links::Vector{Set{T}}
end

Graph{T}() where T = Graph{T}(Dict(), [])

Base.length(g::Graph) = length(g.links)
Base.keys(g::Graph) = keys(g.nodes)

function Base.getindex(g::Graph, x)
  if !haskey(g.nodes, x)
    push!(g.links, Set())
    g.nodes[x] = length(g)
  end
  return g.links[g.nodes[x]]
end

function connect!(g::Graph, a, b)
  push!(g[a], b)
  push!(g[b], a)
  return g
end
