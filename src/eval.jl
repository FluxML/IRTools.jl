using Base: invokelatest

dummy() = return
const dummy_m = which(dummy, Tuple{})

function build_codeinfo(ir::IR)
  ir = copy(ir)
  ci = Base.uncompressed_ir(dummy_m)
  ci.inlineable = true
  for arg in arguments(ir)
    push!(ci.slottypes, Type)
    push!(ci.slotnames, Symbol(""))
    push!(ci.slotflags, 0)
  end
  argument!(ir, at = 1)
  update!(ci, ir)
end

function func(m::Module, ir::IR)
  @eval m (@generated function $(gensym())($([Symbol(:arg, i) for i = 1:length(arguments(ir))]...))
    return $build_codeinfo($ir)
  end)
end

func(ir::IR) = func(Main, ir)

evalir(m::Module, ir::IR, args...) = invokelatest(func(m, ir), args...)
evalir(ir::IR, args...) = evalir(Main, ir, args...)
