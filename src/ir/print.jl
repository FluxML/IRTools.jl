import Base: show

# TODO: real expression printing
Base.show(io::IO, x::Variable) = print(io, "%", x.id)
Base.show(io::IO, x::Argument) = print(io, "_", x.id)

function show(io::IO, b::Branch)
  if b == unreachable
    print(io, "unreachable")
  elseif isreturn(b)
    print(io, "return $(b.args[1])")
  else
    print(io, "br $(b.block)")
    if !isempty(b.args)
      print(io, " (")
      join(io, b.args, ", ")
      print(io, ")")
    end
    b.condition != nothing && print(io, " unless $(b.condition)")
  end
end

function show(io::IO, b::Block)
  print(io, b.id, ":")
  if !isempty(basicblock(b).args)
    print(io, " (")
    join(io, basicblock(b).args, ", ")
    print(io, ")")
  end
  println(io)
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
