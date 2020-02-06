struct CompileError
  transform
  args
  err
end

struct Self end
const self = Self()

# S -> function signature
# I -> lambda index
# T -> environment type
struct Lambda{S,I,T}
  data::T
end

Lambda{D,S}(data...) where {D,S} =
  Lambda{D,S,typeof(data)}(data)

@inline Base.getindex(l::Lambda, i::Integer) = l.data[i]

Base.show(io::IO, l::Lambda) = print(io, "λ")

function Base.showerror(io::IO, err::CompileError)
  println(io, "Error compiling @dynamo $(err.transform) on $(err.args):")
  showerror(io, err.err)
end

function transform end
function refresh end

function fallthrough(args...)
  Expr(:block,
       Expr(:meta, :inline),
       Expr(:call, [:(args[$i]) for i = 1:length(args)]...))
end

function lambdalift!(ir, S, I = ())
  i = 0
  for (v, st) in ir
    isexpr(st.expr, :lambda) || continue
    ir[v] = Expr(:call, Lambda{S,(I...,i+=1)}, st.expr.args[2:end]...)
  end
  return ir
end

function getlambda(ir, I)
  isempty(I) && return ir
  i = 0
  for (v, st) in ir
    isexpr(st.expr, :lambda) || continue
    (i += 1) == I[1] && return getlambda(st.expr.args[1], Base.tail(I))
  end
  error("Something has gone wrong in IRTools; couldn't find lambda in IR")
end

# Used only for its CodeInfo
dummy(args...) = nothing

function dynamo(cache, f, args...)
  try
    ir = transform(f, args...)::Union{IR,Expr,Nothing}
  catch e
    rethrow(CompileError(f, args, e))
  end
  ir isa Expr && return ir
  ir == nothing && return fallthrough(args...)
  cache[args] = ir
  ir = lambdalift!(copy(ir), Tuple{f,args...})
  if ir.meta isa Meta
    m = ir.meta
    ir = varargs!(m, ir)
    argnames!(m, :args)
    pushfirst!(m.code.slotnames, Symbol("#self#"))
  else
    m = @meta dummy(1)
    m.code.method_for_inference_limit_heuristics = nothing
  end
  _self = splicearg!(ir)
  prewalk!(x -> x === self ? _self : x, ir)
  return update!(m.code, ir)
end

function dynamo_lambda(cache, f::Type{<:Lambda{S,I}}) where {S,I}
  ir = cache[(S.parameters[2:end]...,)]
  ir = getlambda(ir, I)
  ir = lambdalift!(copy(ir), S, I)
  closureargs!(ir)
  m = @meta dummy(1)
  m.code.method_for_inference_limit_heuristics = nothing
  return update!(m.code, ir)
end

unesc(x) = prewalk(x -> isexpr(x, :escape) ? x.args[1] : x, x)

function lifttype(x)
  isexpr(x, :(::)) || return x
  named = length(x.args) == 2
  T = named ? x.args[2] : x.args[1]
  T = :(Type{$T})
  named ? Expr(:(::), x.args[1], T) : Expr(:(::), T)
end

macro dynamo(ex)
  @capture(shortdef(ex), (name_(args__) = body_) |
                         (name_(args__) where {Ts__} = body_)) ||
    error("@dynamo needs a function definition.")
  Ts = Ts == nothing ? [] : esc.(Ts)
  f, T = isexpr(name, :(::)) ?
    (length(name.args) == 1 ? (esc(gensym()), esc(name.args[1])) : esc.(name.args)) :
    (esc(gensym()), :(Core.Typeof($(esc(name)))))
  gendef = quote
    local cache = Dict()
    @generated ($f::$T)($(esc(:args))...) where $(Ts...) =
      return IRTools.dynamo(cache, $f, args...)
    @generated (f::IRTools.Inner.Lambda{<:Tuple{<:$T,Vararg{Any}}})(args...) where $(Ts...) =
      return IRTools.Inner.dynamo_lambda(cache, f)
  end
  quote
    $(isexpr(name, :(::)) || esc(:(function $name end)))
    function IRTools.transform(::Type{<:$T}, $(esc.(lifttype.(args))...)) where $(Ts...)
      $(esc(body))
    end
    $gendef
    IRTools.refresh(::$T) where $(Ts...) = (Core.eval($__module__, $(QuoteNode(unesc(gendef)))); return)
  end
end

macro code_ir(dy, ex)
  @capture(ex, f_(args__)) || error("@code_dynamo f(x...)")
  :(transform(typeof($(esc(dy))), meta($typesof($(esc(f)), $(esc.(args)...)))))
end

function recurse!(ir, to = self)
  for (x, st) in ir
    isexpr(st.expr, :call) || continue
    ir[x] = Expr(:call, to, st.expr.args...)
  end
  return ir
end
