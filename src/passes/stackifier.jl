# Find strongly connected components
function strongconnected(cfg; blocks = 1:length(cfg))
  preorder = zeros(Int, length(cfg))
  S, P = Int[], Int[]
  C = Ref(0)
  components = Vector{Int}[]
  let # avoid boxing `search`
    function search(v)
      preorder[v] = (C[] += 1)
      push!(S, v)
      push!(P, v)
      for w in cfg[v]
        w in blocks || continue
        if preorder[w] == 0
          search(w)
        elseif !any(c -> w in c, components)
          while preorder[P[end]] > preorder[w] pop!(P) end
        end
      end
      if P[end] == v
        pop!(P)
        pushfirst!(components, Int[])
        while true
          push!(components[1], pop!(S))
          components[1][end] == v && break
        end
      end
    end
    for b in blocks
      preorder[b] == 0 && search(b)
    end
  end
  return components
end

struct Component
  children::Vector{Union{Component,Int}}
end

blocks(c::Integer) = [c]
blocks(c::Component) = reduce(vcat, blocks.(c.children))

entries(c::Integer) = [c]
entries(c::Component) = entries(first(c.children))

function components(cfg::CFG; blocks = 1:length(cfg))
  # Assume the first block is the entry.
  cs = strongconnected(cfg, blocks = blocks[2:end])
  Component([blocks[1],
             [length(c) == 1 ? c[1] : components(cfg, blocks = sort(c)) for c in cs]...])
end

# Aaaargh
_union(xs...) = union(xs...)
_union() = []

function branchesto(cfg::CFG, cs, i)
  c = cs.children[i]
  valid = _union([entries(cs.children[j]) for j = 1:length(cs.children) if i != j]...)
  preds = _union([cfg'[c] for c in blocks(c)]...)
  forw = filter(next -> next < entries(c)[1] && next in valid, preds)
  back = filter(next -> next > entries(c)[1] && next in valid, preds)
  forw = isempty(forw) ? nothing : minimum(forw)
  back = isempty(back) ? nothing : maximum(back)
  forw, back
end

function stackify(cfg::CFG, cs::Component = components(cfg), forw = [], back = [])
  for (i, c) = enumerate(cs.children)
    target = minimum(entries(c))
    f, b = branchesto(cfg, cs, i)
    f == nothing || push!(forw, f=>target)
    b == nothing || push!(back, b=>target)
    c isa Component && stackify(cfg, c, forw, back)
  end
  return forw, back
end
