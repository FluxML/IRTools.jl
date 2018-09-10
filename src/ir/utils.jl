import Base: map
import Core.Compiler: ssamap, userefs
import MacroTools: walk

const unreachable = ReturnNode()

walk(x::GotoIfNot, inner, outer) = outer(GotoIfNot(inner(x.cond), x.dest))

walk(x::ReturnNode, inner, outer) =
  x == unreachable ? outer(x) :
  outer(ReturnNode(inner(x.val)))

walk(x::PhiNode, inner, outer) = outer(PhiNode(x.edges, inner.(x.values)))

xcall(mod::Module, f::Symbol, args...) = Expr(:call, GlobalRef(mod, f), args...)
xcall(f::Symbol, args...) = xcall(Base, f, args...)

function map(f, b::BasicBlock)
  f′(x) = Statement(x, f(x.expr))
  BasicBlock(f′.(b.stmts), f′.(b.gotos))
end

function map(f, ir::IR)
  IR(ir.defs, map.(f, ir.blocks), ir.lines, ir.args)
end

walk(ir::IR, inner, outer) = outer(map(inner, ir))

# TODO non-mutating ssamap/argmap
ssamap(f, ir::IR) = map(x -> ssamap(f, x), ir)

function argmap(f, @nospecialize(stmt))
    urs = userefs(stmt)
    for op in urs
        val = op[]
        if isa(val, Argument)
            op[] = f(val)
        end
    end
    return urs[]
end

argmap(f, ir::IR) = map(x -> argmap(f, x), ir)
