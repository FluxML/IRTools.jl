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

function reaching(c::CFG, i::Integer, rs = Int[]; ignore = Int[])
  for j in c.graph[i]
    j in rs && return rs
    j in ignore && continue
    push!(rs, j)
    reaching(c, j, rs, ignore = ignore)
  end
  return rs
end

struct Simple
  block::Int
  next
end

Simple(n) = Simple(n, nothing)

struct Loop
  inner
  next
end

struct Multiple
  inner::Vector{Any}
  next
end

for T in [Simple, Loop, Multiple]
  @eval Base.:(==)(a::$T, b::$T) = $(reduce((a, b) -> :($a&&$b), [:(a.$f == b.$f) for f in fieldnames(T)]))
end

function reaches_unique(rs)
  rs = Dict(b => union(b, cs) for (b, cs) in rs)
  others(b) = union([cs for (c, cs) in rs if b != c]...)
  Dict(b => filter(c -> !any(rs -> c in rs, others(b)), cs) for (b, cs) in rs)
end

function reloop_loop(cfg::CFG, blocks, entry, done, rs)
  body = filter(b -> any(e -> e in rs[b], entry), blocks)
  next = setdiff(blocks, body)
  Loop(reloop(cfg, blocks = body, entry = entry, done = union(done, entry)),
       reloop(cfg, blocks = next, entry = union([intersect(rs[b], next) for b in body]...), done = done))
 end

function reloop(cfg::CFG; blocks = 1:length(cfg.graph), entry = [1], done = Int[])
  isempty(blocks) && return
  rs = Dict(b => reaching(cfg, b, ignore = done) for b in blocks)
  if length(entry) == 1 && entry[] ∉ rs[entry[]]
    next = setdiff(blocks, entry[])
    Simple(entry[],
           reloop(cfg, blocks = next,
                  entry = intersect(cfg.graph[entry[]], blocks), done = done))
  elseif all(b -> isempty(setdiff(entry, rs[b])), entry)
    reloop_loop(cfg, blocks, entry, done, rs)
  elseif (rsu = reaches_unique(rs); !all(isempty, values(rsu)))
    unique = filter(b -> !isempty(rsu[b]), entry)
    inner = [reloop(cfg, blocks = union(b, rsu[b]), entry = [b], done = done) for b in unique]
    next = setdiff(blocks, union([union(b, rsu[b]) for b in unique]...))
    es = intersect(next, union(entry, reaching.((cfg,), unique)...))
    Multiple(inner, reloop(cfg, blocks = next, entry = es, done = done))
  else
    reloop_loop(cfg, blocks, entry, done, rs)
  end
end

printstructure(io::IO, ::Nothing, level) = nothing

function printstructure(io::IO, s::Simple, level)
  println(io, "  "^level, s.block)
  printstructure(io, s.next, level)
end

function printstructure(io::IO, s::Multiple, level)
  println(io, "  "^level, "Multiple:")
  for b in s.inner
    printstructure(io, b, level+1)
  end
  printstructure(io, s.next, level)
end

function printstructure(io::IO, s::Loop, level)
  println(io, "  "^level, "Loop:")
  printstructure(io, s.inner, level+1)
  printstructure(io, s.next, level)
end

function Base.show(io::IO, b::Union{Simple,Multiple,Loop})
  println(io, "Structured CFG:")
  printstructure(io, b, 0)
end
