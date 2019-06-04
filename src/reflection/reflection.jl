using Core: CodeInfo, Typeof
using Core.Compiler: InferenceState, widenconst, svec
using InteractiveUtils: typesof

worldcounter() = ccall(:jl_get_world_counter, UInt, ())

isprecompiling() = ccall(:jl_generating_output, Cint, ()) == 1

struct TypedMeta
  frame::InferenceState
  method::Method
  code::CodeInfo
  ret
end

function Base.show(io::IO, meta::TypedMeta)
  print(io, "Typed metadata for ")
  print(io, meta.method)
end

define_typeinf_code2() = isprecompiling() ||
@eval Core.Compiler function typeinf_code2(method::Method, @nospecialize(atypes), sparams::SimpleVector, run_optimizer::Bool, params::Params)
    if $(VERSION >= v"1.2-")
      code = specialize_method(method, atypes, sparams)
    else
      code = code_for_method(method, atypes, sparams, params.world)
    end
    code === nothing && return (nothing, Any)
    ccall(:jl_typeinf_begin, Cvoid, ())
    result = InferenceResult(code)
    frame = InferenceState(result, false, params)
    frame === nothing && return (nothing, Any)
    if typeinf(frame) && run_optimizer
        opt = OptimizationState(frame)
        optimize(opt, result.result)
        opt.src.inferred = true
    end
    ccall(:jl_typeinf_end, Cvoid, ())
    frame.inferred || return (nothing, Any)
    return frame
end

function typed_meta(T; world = worldcounter(), optimize = false)
  F = T.parameters[1]
  F isa DataType && (F.name.module === Core.Compiler ||
                     F <: Core.Builtin ||
                     F <: Core.Builtin) && return nothing
  _methods = Base._methods_by_ftype(T, -1, world)
  length(_methods) == 1 || return nothing
  type_signature, sps, method = first(_methods)
  params = Core.Compiler.Params(world)
  frame = Core.Compiler.typeinf_code2(method, type_signature, sps, optimize, params)
  ci = frame.src
  ci.inferred = true
  ci.method_for_inference_limit_heuristics = method
  if ci.ssavaluetypes == 0 # constant return; IRCode doesn't like this
    ci.ssavaluetypes = Any[Any]
  end
  return TypedMeta(frame, method, ci, widenconst(frame.result.result))
end

struct Meta
  method::Method
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

function meta(T; world = worldcounter())
  F = T.parameters[1]
  F == typeof(invoke) && return invoke_meta(T; world = world)
  F isa DataType && (F.name.module === Core.Compiler ||
                     F <: Core.Builtin ||
                     F <: Core.Builtin) && return nothing
  _methods = Base._methods_by_ftype(T, -1, world)
  length(_methods) == 0 && return nothing
  type_signature, sps, method = last(_methods)
  sps = svec(map(untvar, sps)...)
  @static if VERSION >= v"1.2-"
    mi = Core.Compiler.specialize_method(method, type_signature, sps)
    ci = Base.isgenerated(mi) ? Core.Compiler.get_staged(mi) : Base.uncompressed_ast(method)
  else
    mi = Core.Compiler.code_for_method(method, type_signature, sps, world, false)
    ci = Base.isgenerated(mi) ? Core.Compiler.get_staged(mi) : Base.uncompressed_ast(mi)
  end
  ci.method_for_inference_limit_heuristics = method
  if isdefined(ci, :edges)
    ci.edges = Core.MethodInstance[mi]
  end
  Base.Meta.partially_inline!(ci.code, [], method.sig, Any[sps...], 0, 0, :propagate)
  Meta(method, ci, method.nargs, sps)
end

function invoke_tweaks!(ci::CodeInfo)
  ci.slotnames = [:invoke, ci.slotnames[1], :T, ci.slotnames[2:end]...]
  ci.slotflags = [0x00, ci.slotflags[1], 0x00, ci.slotflags[2:end]...]
  ci.code = map(ci.code) do x
    prewalk(x) do x
      x isa SlotNumber ? SlotNumber(x.id == 1 ? 2 : x.id+2) : x
    end
  end
end

function invoke_meta(T; world)
  F = T.parameters[2]
  A = T.parameters[3]::Type{<:Type{<:Tuple}}
  T = Tuple{F,A.parameters[1].parameters...}
  m = meta(T, world = world)
  invoke_tweaks!(m.code)
  return Meta(m.method, m.code, m.nargs+2, m.sparams)
end

macro meta(ex)
  isexpr(ex, :call) || error("@meta f(args...)")
  f, args = ex.args[1], ex.args[2:end]
  :(meta(typesof($(esc.((f, args...))...))))
end

macro typed_meta(ex)
  isexpr(ex, :call) || error("@meta f(args...)")
  f, args = ex.args[1], ex.args[2:end]
  :(typed_meta(typesof($(esc.((f, args...))...))))
end

function code_ir(f, T)
  m = meta(Tuple{Typeof(f),T.parameters...})
  return IR(m)
end

function code_irm(ex)
  isexpr(ex, :call) || error("@code_ir f(args...)")
  f, args = ex.args[1], ex.args[2:end]
  :(code_ir($(esc(f)), typesof($(esc.(args)...))))
end

macro code_ir(ex)
  code_irm(ex)
end

codeinfo(m::Meta) = m.code
codeinfo(m::TypedMeta) = m.code

function argnames!(meta, names...)
  meta.code.slotnames = [names...]
end
