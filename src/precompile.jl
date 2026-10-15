if ccall(:jl_generating_output, Cint, ()) == 1
  let
    f(x, y) = x > 0 ? x * y + sin(y) : y^2
    g(x) = (s = 0.0; for i in 1:x; s += i; end; s)

    for T in (Tuple{typeof(f),Float64,Float64}, Tuple{typeof(g),Int})
      m = meta(T)
      ir = IR(m)
      sprint(show, ir)
      for (v, st) in ir
        exprline(ir, v)
        isexpr(st.expr, :call)
      end
      pr = Pipe(ir)
      for (v, st) in pr
        pr[v] = stmt(st, expr = st.expr)
        insertafter!(pr, v, xcall(:sin, v))
        substitute(pr)
      end
      ir = renumber(finish(pr))
      push!(ir, xcall(:sin, 1.0))
      prewalk(x -> x, ir)
      merge_returns!(ir)
      expand!(ir)
      prune!(ir)
      domorder!(ir)
      functional(copy(ir))
      ir = varargs!(m, copy(ir), 1)
      Inner.update!(m.code, slots!(pis!(inlineable!(ir))))
    end
  end
end
