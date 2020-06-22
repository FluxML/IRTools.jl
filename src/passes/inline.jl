function inlinehere!(ir, source, args...)
  source = merge_returns!(copy(source)) # TODO preserve type info
  env = Dict()
  retvalue = nothing
  rename(x::Variable) = env[x]
  rename(x::Expr) = Expr(x.head, rename.(x.args)...)
  rename(x::Statement) = stmt(x, expr = rename(x.expr))
  rename(x) = x
  for (name, arg) in zip(arguments(source), args)
    env[name] = arg
  end
  for bl in blocks(source)
    if bl.id != 1
      block!(ir)
      for (arg, T) in zip(arguments(bl), argtypes(bl))
        env[arg] = blockargument!(ir, T)
      end
    end
    for (v, st) in bl
      env[v] = push!(ir, rename(st))
    end
    for br in branches(bl)
      if isreturn(br)
        retvalue = rename(returnvalue(br))
      else
        branch!(ir, br.block, rename.(br.args)..., unless = br.condition)
      end
    end
  end
  return retvalue
end

function inline(ir::IR, loc::Variable, source::IR)
  pr = Pipe(ir)
  for (v, st) in pr
    if v === loc
      ex = ir[loc].expr
      delete!(pr, v)
      v′ = inlinehere!(pr, source, ex.args...)
      substitute!(pr, v, substitute(pr, v′))
    end
  end
  return finish(pr)
end
