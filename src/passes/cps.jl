cond(c, t, f) = c ? t() : f()

allsuccs(ir, (id,chs)) =
  union(map(b -> b.id, successors(block(ir, id))),
        allsuccs.((ir,), chs)...)

function captures(ir, (id, chs), cs = Dict())
  bl = block(ir, id)
  captures.((ir,), chs, (cs,))
  cs[id] = setdiff(union([cs[i] for (i, _) in chs]..., usages(bl)), definitions(bl))
  return cs
end

function return_thunk(x)
  ir = IR()
  return!(ir, xcall(:getindex, argument!(ir), 1))
  Expr(:lambda, ir, x)
end

function br_thunk(args...)
  ir = IR()
  self = argument!(ir)
  return!(ir, xcall([xcall(:getindex, self, i) for i = 1:length(args)]...))
  Expr(:lambda, ir, args...)
end

function functionalbranches!(bl, pr, labels)
  if length(branches(bl)) == 1
    br = branches(bl)[1]
    if !isreturn(br)
      r = push!(pr, Expr(:call, labels[br.block], br.args...))
      empty!(branches(pr.to))
      return!(pr, r)
    end
  else
    @assert length(branches(bl)) == 2
    f, t = branches(bl)
    function brfunc(br)
      isreturn(br) && return return_thunk(returnvalue(br))
      isempty(arguments(br)) && return labels[br.block]
      br_thunk(labels[br.block], arguments(br)...)
    end
    r = push!(pr, xcall(IRTools, :cond, f.condition, brfunc(t), brfunc(f)))
    empty!(branches(pr.to))
    return!(pr, r)
  end
end

function _functional(ir, tree, vars = [], cs = captures(ir, tree))
  id = tree[1]
  bl = IR(block(ir, id))
  labels = Dict()
  labels[id] = self = id == 1 ? arguments(bl)[1] : argument!(bl, at = 1)
  pr = Pipe(bl)
  for (i, v) in enumerate(vars)
    v′ = push!(pr, xcall(:getindex, self, i))
    v isa Integer ? (labels[v] = v′) : substitute!(pr, v, substitute(pr, v′))
  end
  for _ in pr end
  for t in reverse(tree[2])
    bs = filter(id -> haskey(labels, id), allsuccs(ir, t))
    λ = Expr(:lambda, _functional(ir, t, [bs..., cs[t[1]]...]),
             map(b -> labels[b], bs)..., cs[t[1]]...)
    labels[t[1]] = push!(pr, λ)
  end
  functionalbranches!(bl, pr, labels)
  return finish(pr)
end

# A chain of conditional branches is a nested if/else: move all but the first
# branch of blocks with more than two into a fresh block.
function splitbranches!(ir)
  i = 1
  while i <= length(blocks(ir))
    bl = block(ir, i)
    if length(branches(bl)) > 2
      new = block!(ir, i+1)
      brs = copy(branches(bl))
      empty!(branches(bl))
      push!(branches(bl), brs[1])
      branch!(bl, new)
      append!(branches(new), brs[2:end])
    end
    i += 1
  end
  return ir
end

# Unreachable blocks (e.g. left behind by a `break`) can branch anywhere, which
# can't be expressed by nesting block lambdas along the dominator tree, and they
# skew the dominators of the blocks they branch to. So drop them first.
function functional(ir)
  ir = trimblocks!(splitbranches!(explicitbranch!(copy(ir))))
  _functional(ir, domtree(ir))
end
