import Base: show

# TODO: real expression printing
Base.show(io::IO, x::Variable) = print(io, "%", x.id)
Base.show(io::IO, x::Argument) = print(io, "_", x.id)

function show(io::IO, b::Branch)
  if b == unreachable
    print(io, "unreachable")
  elseif isreturn(b)
    print(io, "return $(repr(b.args[1]))")
  else
    print(io, "br $(b.block)")
    if !isempty(b.args)
      print(io, " (")
      join(io, repr.(b.args), ", ")
      print(io, ")")
    end
    b.condition != nothing && print(io, " unless $(b.condition)")
  end
end

function show(io::IO, b::Block)
  bb = basicblock(b)
  print(io, b.id, ":")
  if !isempty(bb.args)
    print(io, " (")
    for i = 1:length(bb.args)
      print(io, bb.args[i])
      bb.argtypes[i] != Any && print(io, " :: ", bb.argtypes[i])
      i != length(bb.args) && print(io, ", ")
    end
    print(io, ")")
  end
  println(io)
  for (x, st) in b
    print(io, "  ")
    x == nothing || print(io, string("%", x.id), " = ")
    st.expr == nothing ? print(io, "nothing") :
      print(io, st.expr)
    st.type == Any || print(io, " :: ", st.type)
    println(io)
  end
  for br in bb.branches
    println(io, "  ", br)
  end
end

show(io::IO, ir::IR) = foreach(b -> show(io, b), blocks(ir))
