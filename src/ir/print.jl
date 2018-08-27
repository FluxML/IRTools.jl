import Base: show

function show(io::IO, b::Block)
  println(io, b.id, ":")
  for (x, st) in b
    x == nothing ?
      print(io, "       ") :
      print(io, lpad(string("%", x.id), 4), " = ")
    st.expr == nothing ? println(io, "nothing") :
      println(io, st.expr)
  end
end

show(io::IO, ir::IR) = foreach(b -> show(io, b), blocks(ir))
