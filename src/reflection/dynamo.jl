function transform end
function refresh end

function fallback(n)
  ir = IR()
  for i = 1:n+1 push!(ir.args, Any) end
  ret = push!(ir, Expr(:call, Argument(1), [Argument(n) for n = 2:n+1]...))
  return!(ir, ret)
end

function dynamo(f, args...)
  m = meta(Tuple{args...})
  ir = transform(f, m)::Union{IR,Nothing}
  ir == nothing && return :(args[1](args[2:end]...))
  ir = varargs!(m, ir)
  argnames!(m, :args)
  ir = spliceargs!(m, ir, (Symbol("#self#"), Any))
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
