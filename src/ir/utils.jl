import Base: map
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

function map(f, ir::IR)
  IR(ir.defs, map.(f, ir.blocks), ir.lines, ir.args)
end

walk(ir::IR, inner, outer) = outer(map(inner, ir))

varmap(f, x) = prewalk(x -> x isa Variable ? f(x) : x, x)

argmap(f, x) = prewalk(x -> x isa Argument ? f(x) : x, x)
