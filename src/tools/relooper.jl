import MacroTools.postwalk

function condition(block)
    br = branches(block)
    isempty(br) && return nothing
    return br[1].condition
end

function findcycles(b::Block, tape=Block[])
    succ = successors(b)
    isempty(succ) && return Vector{Block}[]
    tape = vcat(tape, b)
    first(tape) in succ && return [tape]
    # loops that don't jump back to the first Block don't count
    filter!(!in(tape), succ)
    next = mapreduce(i->findcycles(i, tape), vcat, succ, init=Vector{Block}[])
    filter!(!isempty, next)
    return next
end

accessible(block, ignore=[]) = count(x->!any(i->i.id in ignore, x), findcycles(block))>0

function extract_expr(b::Block, symbols)
    return map(pairs(b)) do (var, (_, stmt))
        Expr(
             :(=),
             get!(gensym, symbols, var),
             postwalk(stmt.expr) do i
                 if i isa Variable
                     return get!(gensym, symbols, i)
                 end
                 return i
             end
            )
    end
end

function nextargs(succ, brs, symbols)
    nextargs = []
    for i in succ
        br = findfirst(x->x.block==i, brs)
        push!(nextargs,
              if br === nothing
                  []
              else
                  [
                   arg isa Variable ? get!(gensym, symbols, arg) : arg
                   for arg in brs[br].args
                  ]
              end
             )
    end
    return nextargs
end


# NOTE: this will break if arbitrary gotos are used.
function restructure(
                     blocks::Vector,
                     args,
                     symbols = Dict(),
                     entries = 1,
                     loops = [],
                     cond = nothing
                    )

    if isempty(entries)
        return []
    elseif length(entries) == 1
        l = entries[1]
        block = blocks[l]

        assign_args = Expr.(:(=), get!.(gensym, Ref(symbols), arguments(block)), args[1])

        if !isempty(loops)
            l == loops[end] && return [assign_args..., :(continue)]
            l in loops && return [assign_args..., :(break)]
            l == successors(blocks[loops[end]])[end] && return [assign_args..., :(break)]
        end

        code = extract_expr(block, symbols)
        cond = condition(block)

        if isreturn(block)
            val = returnvalue(block)
            if val isa Variable
                val = get!(gensym, symbols, val)
            end
            code = [code..., cond === nothing ?
                    :(return $val) : Expr(:if, get!(gensym, symbols, cond), :(return $val))]
        end

        succ = [i.id for i in successors(block)]
        isempty(succ) && return [assign_args..., code...]
        brs = branches(block)
        nargs = nextargs(succ, branches(block), symbols)

        if !accessible(block, loops)
            return [
                    assign_args...,
                    code...,
                    restructure(blocks, nargs, symbols, succ, loops, cond)...
                   ]
        else
            return [
                    assign_args...,
                    Expr(:while, true,
                         Expr(:block,
                              code...,
                              restructure(blocks, nargs, symbols, succ, [loops...,l], cond)...
                             )
                        )
                   ]
        end
    elseif length(entries) == 2 && cond != nothing
        return [
                Expr(:if, get!(gensym, symbols, cond),
                     Expr(:block, restructure(blocks, [args[2]], symbols, entries[2], loops)...),
                     Expr(:block, restructure(blocks, [args[1]], symbols, entries[1], loops)...))
               ]
    else
        @show entries
        error("Rebuild error")
    end
end

function restructure(code::IR, args=[])
    return Expr(:block, restructure(blocks(code), [["foo", args...]])...)
end
