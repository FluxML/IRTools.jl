import Base: show

function show(io::IO, b::Block)
  println(io, b.id, ":")
  for (x, st) in b
    print(io, "  ")
    x == nothing || print(io, string("%", x.id), " = ")
    st.expr == nothing ? println(io, "nothing") :
      println(io, st.expr)
  end
end

show(io::IO, ir::IR) = foreach(b -> show(io, b), blocks(ir))
