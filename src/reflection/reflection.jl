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
    code = code_for_method(method, atypes, sparams, params.world)
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
  F isa DataType && (F.name.module === Core.Compiler ||
                     F <: Core.Builtin ||
                     F <: Core.Builtin) && return nothing
  _methods = Base._methods_by_ftype(T, -1, world)
  length(_methods) == 1 || return nothing
  type_signature, sps, method = first(_methods)
  sps = svec(map(untvar, sps)...)
  mi = Core.Compiler.code_for_method(method, type_signature, sps, world, false)
  ci = Base.isgenerated(mi) ? Core.Compiler.get_staged(mi) : Base.uncompressed_ast(mi)
  ci.method_for_inference_limit_heuristics = method
  Base.Meta.partially_inline!(ci.code, [], method.sig, Any[sps...], 0, 0, :propagate)
  Meta(method, ci, sps)
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
