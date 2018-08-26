import Base: map
import Core.Compiler: ssamap

map(f, b::BasicBlock) = BasicBlock(f.(b.stmts), f.(b.gotos))

function map(f, ir::IR)
  IR(ir.defs, map.(f, ir.blocks))
end

ssamap(f, ir::IR) = map(x -> Statement(x, ssamap(f, x.expr)), ir)
