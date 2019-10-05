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

function stackify(cfg::CFG; blocks = 1:length(cfg))
  # Assume the first block is the entry.
  components = strongconnected(cfg, blocks = blocks[2:end])
  [blocks[1],
   [length(c) == 1 ? c[1] : stackify(cfg, blocks = sort(c)) for c in components]...]
end
