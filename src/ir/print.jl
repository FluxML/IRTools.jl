import Base: show

# TODO: real expression printing
function Base.show(io::IO, x::Variable)
  bs = get(io, :bindings, Dict())
  haskey(bs, x) ? print(io, bs[x]) : print(io, "%", x.id)
end

print_stmt(io::IO, ex::Expr) = print_stmt(io::IO, Val(ex.head), ex)
print_stmt(io::IO, ::Val, ex) = print(io, ex)
print_stmt(io::IO, ex) = print(io, ex)

function show(io::IO, b::Branch)
  if b == unreachable
    print(io, "unreachable")
  elseif isreturn(b)
    print(io, "return ", b.args[1])
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
  bs = get(io, :bindings, Dict())
  bb = BasicBlock(b)
  print(io, tab^indent)
  print(io, b.id, ":")
  if !isempty(bb.args)
    print(io, " ")
    printargs(io, bb.args, bb.argtypes)
  end
  for (x, st) in b
    haskey(bs, x) && continue
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

function print_stmt(io::IO, ::Val{:enter}, ex)
  print(io, "try #$(ex.args[1])")
end

function print_stmt(io::IO, ::Val{:leave}, ex)
  print(io, "end try")
end

function print_stmt(io::IO, ::Val{:catch}, ex)
  print(io, "catch $(ex.args[1])")
  args = ex.args[2:end]
  if !isempty(args)
    print(io, " (")
    join(io, args, ", ")
    print(io, ")")
  end
end

function print_stmt(io::IO, ::Val{:pop_exception}, ex)
  print(io, "pop exception $(ex.args[1])")
end

function lambdacx(io, ex)
  bs = get(io, :bindings, Dict())
  ir = ex.args[1]
  args = ex.args[2:end]
  bs′ = Dict()
  for (v, st) in ir
    ex = st.expr
    if iscall(ex, GlobalRef(Base, :getindex)) &&
        ex.args[2] == arguments(ir)[1] &&
        ex.args[3] isa Integer
      x = args[ex.args[3]]
      bs′[v] = string(get(bs, x, x), "'")
    end
  end
  return bs′
end

function print_stmt(io::IO, ::Val{:lambda}, ex)
  print(io, "λ :")
  # printargs(io, ex.args[2:end])
  io = IOContext(io, :indent   => get(io, :indent, 0)+2,
                     :bindings => lambdacx(io, ex))
  println(io)
  print(io, ex.args[1])
end
