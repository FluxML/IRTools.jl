import Base: map, map!
import Core.Compiler: PhiNode, PiNode, ssamap, userefs
import MacroTools: walk, prewalk, postwalk

walk(x::PhiNode, inner, outer) =
  outer(PhiNode(x.edges,
                [inner(isassigned(x.values, i) ? x.values[i] : undef)
                 for i in 1:length(x.values)]))

walk(x::PiNode, inner, outer) = outer(PiNode(inner(x.val), x.typ))

xcall(mod::Module, f::Symbol, args...) = Expr(:call, GlobalRef(mod, f), args...)
xcall(f::Symbol, args...) = xcall(Base, f, args...)
xcall(f, args...) = Expr(:call, f, args...)

map(f, br::Branch) = Branch(br, condition = f(br.condition), args = f.(br.args))

function map(f, b::BasicBlock)
  stmts = map(x -> Statement(x, expr = f(x.expr)), b.stmts)
  branches = map(br -> map(f, br), b.branches)
  BasicBlock(stmts, b.args, b.argtypes, branches)
end

function map!(f, b::BasicBlock)
  map!(x -> Statement(x, expr = f(x.expr)), b.stmts, b.stmts)
  map!(br -> Branch(br, condition = f(br.condition), args = f.(br.args)), b.branches, b.branches)
  return b
end

function map!(f, b::Block)
  map!(f, BasicBlock(b))
end

function map(f, ir::IR)
  IR(ir.defs, map.(f, ir.blocks), ir.lines, ir.meta)
end

function map!(f, ir::IR)
  for b in blocks(ir)
    map!(f, b)
  end
  return ir
end

walk(st::Statement, inner, outer) = Statement(st, expr = inner(st.expr))
walk(bb::BasicBlock, inner, outer) = map(inner, bb)
walk(bb::Branch, inner, outer) = map(inner, bb)
walk(b::Block, inner, outer) = walk(BasicBlock(b), inner, outer)

walk(ir::IR, inner, outer) = outer(map(inner, ir))

# Avoid recursing into lambdas
prewalk(f, ir::Union{IR,Block})  = walk(f(ir), x -> x isa IR ? x : prewalk(f, x), identity)
postwalk(f, ir::Union{IR,Block}) = walk(ir, x -> x isa IR ? x : postwalk(f, x), f)

prewalk!(f, ir::Union{IR,Block})  = map!(x -> x isa IR ? x :  prewalk(f, x), ir)
postwalk!(f, ir::Union{IR,Block}) = map!(x -> x isa IR ? x : postwalk(f, x), ir)

varmap(f, x) = prewalk(x -> x isa Variable ? f(x) : x, x)

exprtype(x::GlobalRef; typeof = Typeof) = isconst(x.mod, x.name) ? typeof(getfield(x.mod, x.name)) : Any

exprtype(ir::IR, x::GlobalRef; typeof = Typeof) = exprtype(x, typeof = typeof)
exprtype(ir::IR, x::QuoteNode; typeof = Typeof) = typeof(x.value)
exprtype(ir::IR, x::Expr; typeof = Typeof) = error(x)
exprtype(ir::IR, x; typeof = Typeof) = typeof(x)

function exprtype(ir::IR, x::Variable; typeof = Typeof)
  b, i = get(ir.defs, x.id, (-1, -1))
  b == -1 && error("No such variable $x")
  if i > 0
    widenconst(ir[x].type)
  else
    widenconst(ir.blocks[b].argtypes[-i])
  end
end

function exprline(ir::IR, x::Variable)
  b, i = get(ir.defs, x.id, (-1, -1))
  i > 0 || return
  get(ir.lines, ir[x].line, nothing)
end
