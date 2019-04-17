import Base: map, map!
import Core.Compiler: PhiNode, ssamap, userefs
import MacroTools: walk

walk(x::PhiNode, inner, outer) = outer(PhiNode(x.edges, inner.(x.values)))

xcall(mod::Module, f::Symbol, args...) = Expr(:call, GlobalRef(mod, f), args...)
xcall(f::Symbol, args...) = xcall(Base, f, args...)

function map(f, b::BasicBlock)
  stmts = map(x -> Statement(x, f(x.expr)), b.stmts)
  branches = map(br -> Branch(br, condition = f(br.condition), args = f.(br.args)), b.branches)
  BasicBlock(stmts, b.args, branches)
end

function map!(f, b::BasicBlock)
  map!(x -> Statement(x, f(x.expr)), b.stmts, b.stmts)
  map!(br -> Branch(br, condition = f(br.condition), args = f.(br.args)), b.branches, b.branches)
  return b
end

function map!(f, b::Block)
  map!(f, basicblock(b))
end

function map(f, ir::IR)
  IR(ir.defs, map.(f, ir.blocks), ir.lines, ir.args)
end

function map!(f, ir::IR)
  for b in blocks(ir)
    map!(f, b)
  end
  return ir
end

walk(bb::BasicBlock, inner, outer) = map(inner, bb)
walk(b::Block, inner, outer) = walk(basicblock(b), inner, outer)

walk(ir::IR, inner, outer) = outer(map(inner, ir))

prewalk!(f, ir::Union{IR,Block}) = map!(x -> prewalk(f, x), ir)
postwalk!(f, ir::Union{IR,Block}) = map!(x -> postwalk(f, x), ir)

varmap(f, x) = prewalk(x -> x isa Variable ? f(x) : x, x)

argmap(f, x) = prewalk(x -> x isa Argument ? f(x) : x, x)

exprtype(ir::IR, x::Argument) = widenconst(ir.args[x.id])
exprtype(ir::IR, x::Variable) = widenconst(ir[x].type) # TODO: spats
exprtype(ir::IR, x::GlobalRef) = isconst(x.mod, x.name) ? Typeof(getfield(x.mod, x.name)) : Any
exprtype(ir::IR, x::QuoteNode) = Typeof(x.value)
exprtype(ir::IR, x::Expr) = error(x)
exprtype(ir::IR, x) = Typeof(x)
