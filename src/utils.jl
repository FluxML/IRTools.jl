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
