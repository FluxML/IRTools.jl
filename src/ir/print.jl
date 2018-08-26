import Base: show

function show(io::IO, ir::IR)
  defs = Dict((b, i) => s for (s, (b, i)) in enumerate(ir.defs))
  for b in 1:length(ir.blocks)
    println(io, b, ":")
    for i in 1:length(ir.blocks[b].stmts)
      print(io, lpad(string("%", defs[(b, i)]), 4), " = ")
      st = ir.blocks[b].stmts[i]
      println(io, st.expr)
    end
  end
end
