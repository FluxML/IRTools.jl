struct CompileError
  transform
  args
  err
end

struct Self end
const self = Self()

function Base.showerror(io::IO, err::CompileError)
  println(io, "Error compiling @dynamo $(err.transform) on $(err.args):")
  showerror(io, err.err)
end

function transform end
function refresh end

function dynamo(f, args...)
  try
    ir = transform(f, args...)::Union{IR,Expr,Nothing}
  catch e
    rethrow(CompileError(f, args, e))
  end
  ir isa Expr && return ir
  ir == nothing && return :(args[1](args[2:end]...))
  m = ir.meta::Meta
  ir = varargs!(m, ir)
  argnames!(m, :args)
  _self = splicearg!(m, ir, Symbol("#self#"))
  prewalk!(x -> x === self ? _self : x, ir)
  return update!(m.code, ir)
end

unesc(x) = prewalk(x -> isexpr(x, :escape) ? x.args[1] : x, x)

macro dynamo(ex)
  @capture(shortdef(ex), (name_(args__) = body_) |
                         (name_(args__) where {Ts__} = body_)) ||
    error("@dynamo needs a function definition.")
  Ts = Ts == nothing ? [] : esc.(Ts)
  f, T = isexpr(name, :(::)) ?
    (length(name.args) == 1 ? (esc(gensym()), esc(name.args[1])) : esc.(name.args)) :
    (esc(gensym()), :(Core.Typeof($(esc(name)))))
  gendef = :(@generated ($f::$T)($(esc(:args))...) where $(Ts...) = return IRTools.dynamo($f, args...))
  quote
    $(isexpr(name, :(::)) || esc(:(function $name end)))
    function IRTools.transform(::Type{<:$T}, $(esc.(args)...)) where $(Ts...)
      $(esc(body))
    end
    $gendef
    IRTools.refresh(::$T) where $(Ts...) = (Core.eval($__module__, $(QuoteNode(unesc(gendef)))); return)
  end
end

macro code_ir(dy, ex)
  @capture(ex, f_(args__)) || error("@code_dynamo f(x...)")
  :(transform(typeof($(esc(dy))), meta(typesof($(esc(f)), $(esc.(args)...)))))
end

function recurse!(ir)
  for (x, st) in ir
    isexpr(st.expr, :call) || continue
    ir[x] = Expr(:call, self, st.expr.args...)
  end
end
