import Base: show

function show(io::IO, b::Branch)
  if b == unreachable
    print(io, "unreachable")
  elseif isreturn(b)
    print(io, "return $(b.args[1])")
  else
    print(io, "br #$(b.block) (")
    join(io, b.args, ", ")
    print(io, ")")
    b.condition != nothing && print(io, " if $(b.condition)")
  end
end

function show(io::IO, b::Block)
  println(io, b.id, ":")
  for (x, st) in b
    print(io, "  ")
    x == nothing || print(io, string("%", x.id), " = ")
    st.expr == nothing ? println(io, "nothing") :
      println(io, st.expr)
  end
  for br in basicblock(b).branches
    println(io, "  ", br)
  end
end

show(io::IO, ir::IR) = foreach(b -> show(io, b), blocks(ir))
