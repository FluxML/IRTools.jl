using Core: CodeInfo, Typeof
using Core.Compiler: InferenceState, MethodInstance, svec
using Base: typesof

hasgenerator(x) = Base.hasgenerator(x)

worldcounter() = ccall(:jl_get_world_counter, UInt, ())

isprecompiling() = ccall(:jl_generating_output, Cint, ()) == 1

struct Meta
  method::Method
  instance::MethodInstance
  code::CodeInfo
  nargs::Int
  sparams
end

function Base.show(io::IO, meta::Meta)
  print(io, "Metadata for ")
  print(io, meta.method)
end

# Workaround for what appears to be a Base bug
untvar(t::TypeVar) = t.ub
untvar(x) = x

const spoofed_world = Ref{Union{Nothing,UInt}}(nothing)

"""
    meta(Tuple{...})

Construct metadata for a given method signature. Metadata can then be used to
construct [`IR`](@ref) or used to perform other reflection on the method.

See also [`@meta`](@ref).

    julia> IRTools.meta(Tuple{typeof(gcd),Int,Int})
    Metadata for gcd(a::T, b::T) where T<:Union{Int128, Int16, Int32, Int64, Int8, UInt128, UInt16, UInt32, UInt64, UInt8} in Base at intfuncs.jl:31
"""
function meta(T; types = T, world=nothing)
  if world === nothing
    # if the user didn't specify a world, use the current world,
    # or a spoofed world if we're executing a dynamo
    world = something(spoofed_world[], worldcounter())
  end
  F = T.parameters[1]
  F == typeof(invoke) && return invoke_meta(T; world = world)
  F isa DataType && (F.name.module === Core.Compiler ||
                     F <: Core.Builtin ||
                     F <: Core.Builtin) && return nothing
  min_world = Ref{UInt}(typemin(UInt))
  max_world = Ref{UInt}(typemax(UInt))
  has_ambig = Ptr{Int32}(C_NULL)  # don't care about ambiguous results
  _methods = Base._methods_by_ftype(T, #=mt=# nothing, #=lim=# -1,
                                    world, #=ambig=# false,
                                    min_world, max_world, has_ambig)
  _methods === nothing && return nothing
  _methods isa Bool && return nothing
  length(_methods) == 0 && return nothing
  type_signature, sps, method = last(_methods)
  sps = svec(map(untvar, sps)...)
  mi = Core.Compiler.specialize_method(method, types, sps)
  ci = hasgenerator(mi) ? get_staged(mi, world) : Base.uncompressed_ast(method)
  Base.Meta.partially_inline!(ci.code, [], method.sig, Any[sps...], 0, 0, :propagate)
  Meta(method, mi, ci, method.nargs, sps)
end

function invoke_tweaks!(ci::CodeInfo)
  if ci.slottypes !== nothing
    ci.slottypes = [typeof(invoke), ci.slottypes[1], Type, ci.slottypes[2:end]...]
  end
  ci.slotnames = [:invoke, ci.slotnames[1], :T, ci.slotnames[2:end]...]
  ci.slotflags = [0x00, ci.slotflags[1], 0x00, ci.slotflags[2:end]...]
  shift(i) = i == 1 ? 2 : i+2
  f(x) = x isa SlotNumber ? SlotNumber(shift(x.id)) :
         x isa Core.Argument ? Core.Argument(shift(x.n)) :
         x
  # `prewalk` doesn't descend into these nodes, so their slots need handling here.
  ci.code = map(ci.code) do x
    prewalk(x) do x
      x isa Core.ReturnNode ? (isdefined(x, :val) ? Core.ReturnNode(f(x.val)) : x) :
      x isa Core.GotoIfNot ? Core.GotoIfNot(f(x.cond), x.dest) :
      x isa Core.NewvarNode ? Core.NewvarNode(f(x.slot)) :
      f(x)
    end
  end
end

function invoke_meta(T; world)
  F = T.parameters[2]
  A = T.parameters[3]::Type{<:Type{<:Tuple}}
  S = T.parameters[4:end]
  T = Tuple{F,A.parameters[1].parameters...}
  S = Tuple{F,S...}
  m = meta(T, types = S, world = world)
  invoke_tweaks!(m.code)
  return Meta(m.method, m.instance, m.code, m.nargs+2, m.sparams)
end

"""
    @meta [world] f(args...)

Convenience macro for retrieving metadata without writing a full type signature.

    julia> IRTools.@meta gcd(10, 5)
    Metadata for gcd(a::T, b::T) where T<:Union{Int128, Int16, Int32, Int64, Int8, UInt128, UInt16, UInt32, UInt64, UInt8} in Base at intfuncs.jl:31
"""
macro meta(ex...)
  if length(ex) == 1
    world = nothing
    call = ex[1]
  elseif length(ex) == 2
    world, call = ex
  else
    error("@meta [world] f(args...)")
  end
  isexpr(call, :call) || error("@meta [world] f(args...)")
  :(meta(typesof($(callargs(call)...)); world=$(esc(world))))
end

# Escaped arguments to `typesof` for a call; keyword calls go through `Core.kwcall`.
function callargs(call; first = [])
  f, args = call.args[1], call.args[2:end]
  kws = isempty(args) || !isexpr(args[1], :parameters) ? [] : popfirst!(args).args
  append!(kws, filter(a -> isexpr(a, :kw), args))
  args = [first; filter(a -> !isexpr(a, :kw), args)]
  isempty(kws) && return esc.((f, args...))
  kws = map(k -> k isa Symbol ? Expr(:kw, k, k) : k, kws)
  esc.((Core.kwcall, Expr(:tuple, Expr(:parameters, kws...)), f, args...))
end

"""
    code_ir(f, Tuple{...})

Retrieve the [`IR`](@ref) of the method of `f` that matches the given argument
types.

See also [`@code_ir`](@ref).

    julia> code_ir(gcd, Tuple{Int,Int})
    1: (%1, %2, %3)
      %4 = %2 == 0
      br 4 unless %4
    2: ...
"""
function code_ir(f, T)
  m = meta(Tuple{Typeof(f),T.parameters...})
  return IR(m)
end

function code_irm(ex)
  if isexpr(ex, :do)
    f, T... = callargs(ex.args[1], first = [ex.args[2]])
  elseif isexpr(ex, :call)
    f, T... = callargs(ex)
  else
    error("@code_ir f(args...)")
  end
  :($code_ir($f, typesof($(T...))))
end

"""
    @code_ir f(args...)

Convenience macro similar to `@code_lowered` or `@code_typed`. Retrieves the IR
for the given function call. For calls with keyword arguments, this is the IR
of the `Core.kwcall` method that handles them.

    julia> @code_ir gcd(10, 5)
    1: (%1, %2, %3)
      %4 = %2 == 0
      br 4 unless %4
    2: ...
"""
macro code_ir(ex)
  code_irm(ex)
end

codeinfo(m::Meta) = m.code

function argnames!(meta, names...)
  meta.code.slotnames = [names...]
end
