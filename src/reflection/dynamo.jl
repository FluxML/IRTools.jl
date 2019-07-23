struct CompileError
  transform
  args
  err
end

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
  ir = splicearg!(m, ir, Symbol("#self#"))
  return update!(m, ir)
end

macro dynamo(ex)
  @capture(shortdef(ex), f_(args__) = body_) || error("@dynamo needs a function definition.")
  gendef = :(@generated $f(args...) = return $IRTools.dynamo($f, args...))
  quote
    $(esc(:(function $f end)))
    function IRTools.transform(::typeof($(esc(f))), $(esc.(args)...))
      $(esc(body))
    end
    $(esc(gendef))
    IRTools.refresh(::typeof($(esc(f)))) = (Core.eval($__module__, $(QuoteNode(gendef))); return)
  end
end

macro code_ir(dy, ex)
  @capture(ex, f_(args__)) || error("@code_dynamo f(x...)")
  :(transform($(esc(dy)), meta(typesof($(esc(f)), $(esc.(args)...)))))
end
