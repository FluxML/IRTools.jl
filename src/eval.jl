using Base: invokelatest

dummy() = return
const dummy_m = which(dummy, Tuple{})

function build_codeinfo(ir::IR)
  ir = copy(ir)
  ci = Base.uncompressed_ir(dummy_m)
  ci.inlineable = true
  for arg in arguments(ir)
    @static if VERSION >= v"1.10.0-DEV.870"
      isnothing(ci.slottypes) && (ci.slottypes = Any[])
      push!(ci.slottypes, Type)
    end
    push!(ci.slotnames, Symbol(""))
    push!(ci.slotflags, 0)
  end
  if isdefined(Base, :__has_internal_change) && Base.__has_internal_change(v"1.12-alpha", :codeinfonargs)
    ci.nargs = length(arguments(ir)) + 1
    ci.isva = false
  end
  argument!(ir, at = 1)
  update!(ci, ir)
end

# JuliaLang/julia#48611: world age is exposed to generated functions.
if VERSION >= v"1.10.0-DEV.873"

function func(m::Module, ir::IR)
  generator = @eval m begin
    function $(gensym())(world::UInt, source, self,
                         $([Symbol(:arg, i) for i = 1:length(arguments(ir))]...))
      return $build_codeinfo($ir)
    end
  end
  @eval m begin
    function $(gensym())($([Symbol(:arg, i) for i = 1:length(arguments(ir))]...))
      $(Expr(:meta, :generated, generator))
      $(Expr(:meta, :generated_only))
    end
  end
end

else

function func(m::Module, ir::IR)
  @eval m (@generated function $(gensym())($([Symbol(:arg, i) for i = 1:length(arguments(ir))]...))
    return $build_codeinfo($ir)
  end)
end

end

func(ir::IR) = func(Main, ir)

evalir(m::Module, ir::IR, args...) = invokelatest(func(m, ir), args...)
evalir(ir::IR, args...) = evalir(Main, ir, args...)
