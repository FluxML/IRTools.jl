# Find strongly connected components
function strongconnected(cfg)
  preorder = zeros(Int, length(cfg))
  S, P = Int[], Int[]
  C::Int = 0
  components = Vector{Int}[]
  function search(v)
    preorder[v] = (C += 1)
    push!(S, v)
    push!(P, v)
    for w in cfg[v]
      if preorder[w] == 0
        search(w)
      else
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
  search(1)
  return components
end
