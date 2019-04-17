definitions(b::Block) = [Variable(i) for i = 1:length(b.ir.defs) if b.ir.defs[i][1] == b.id]

function usages(b::Block)
  uses = Set{Union{Variable,Argument}}()
  prewalk(b) do x
    x isa Union{Variable,Argument} && push!(uses, x)
    return x
  end
  return uses
end

function trivials!(ir)
  rename = Dict()
  for (v, st) in ir
    (!isexpr(st.expr) && isbits(st.expr)) || continue
    rename[v] = st.expr
    delete!(ir, v)
  end
  prewalk!(x -> get(rename, x, x), ir)
  return ir
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
  for b in reverse(blocks(ir)[2:end])
    defs = vcat(definitions(b), arguments(b))
    uses = usages(b)
    rename = Dict()
    for v in setdiff(uses, defs)
      rename[v] = argument!(b, v)
    end
    ir.blocks[b.id] = prewalk(x -> get(rename, x, x), ir.blocks[b.id])
  end
  return ir
end

allequal() = true
allequal(x) = true
allequal(xs...) = reduce(==, xs)

function trimspats!(ir::IR)
  for b in blocks(ir)
    isempty(arguments(b)) && continue
    brs = filter(br -> br.block == b.id, [br for a in blocks(ir) for br in branches(a)])
    del = allequal.(arguments.(brs)...)
    rename = Dict(zip(arguments(b)[del], arguments(brs[1])[del]))
    ir.blocks[b.id] = prewalk(x -> get(rename, x, x), ir.blocks[b.id])
    deletearg!(b, del)
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
