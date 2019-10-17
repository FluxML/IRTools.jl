import Base: show

# TODO: real expression printing
Base.show(io::IO, x::Variable) = print(io, "%", x.id)

print_stmt(io::IO, ex) = print(io, ex)

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

const tab = "  "

function printargs(io::IO, args, types = [Any for arg in args])
  print(io, "(")
  for i = 1:length(args)
    print(io, args[i])
    types[i] != Any && print(io, " :: ", types[i])
    i != length(args) && print(io, ", ")
  end
  print(io, ")")
end

function show(io::IO, b::Block)
  indent = get(io, :indent, 0)
  bb = BasicBlock(b)
  print(io, tab^indent)
  print(io, b.id, ":")
  if !isempty(bb.args)
    print(io, " ")
    printargs(io, bb.args, bb.argtypes)
  end
  for (x, st) in b
    println(io)
    print(io, tab^indent, "  ")
    x == nothing || print(io, string("%", x.id), " = ")
    st.expr == nothing ? print(io, "nothing") :
      print_stmt(io, st.expr)
    st.type == Any || print(io, " :: ", st.type)
  end
  for br in bb.branches
    println(io)
    print(io, tab^indent, "  ", br)
  end
end

function show(io::IO, ir::IR)
  show(io, block(ir, 1))
  for b in blocks(ir)[2:end]
    println(io)
    show(io, b)
  end
end

function print_stmt(io::IO, ex::IR)
  io = IOContext(io, :indent=>get(io, :indent, 0)+2)
  println(io)
  show(io, ex)
end
