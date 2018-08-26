import Base: map
import Core.Compiler: ssamap

function map(f, b::BasicBlock)
  f′(x) = Statement(x, f(x.expr))
  BasicBlock(f′.(b.stmts), f′.(b.gotos))
end

function map(f, ir::IR)
  IR(ir.defs, map.(f, ir.blocks))
end

ssamap(f, ir::IR) = map(x -> ssamap(f, x), ir)
