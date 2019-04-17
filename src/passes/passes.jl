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
