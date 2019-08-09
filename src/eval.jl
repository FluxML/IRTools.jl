using Base: invokelatest

dummy() = return

function build_codeinfo(ir::IR)
  ir = copy(ir)
  ci = code_lowered(dummy, Tuple{})[1]
  for arg in arguments(ir)
    push!(ci.slotnames, Symbol(""))
    push!(ci.slotflags, 0)
  end
  argument!(ir, at = 1)
  update!(ci, ir)
end

function func(m::Module, ir::IR)
  @eval @generated function $(gensym())($([Symbol(:arg, i) for i = 1:length(arguments(ir))]...))
    return build_codeinfo($ir)
  end
end

func(ir::IR) = func(Main, ir)

eval(m::Module, ir::IR, args...) = invokelatest(func(m, ir), args...)
eval(ir::IR, args...) = eval(Main, ir, args...)
