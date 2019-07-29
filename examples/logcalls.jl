using IRTools: @dynamo, IR, xcall, arguments, insertafter!, recurse!

function hook(f, args...)
  print("Called ", f, "(")
  join(stdout, args, ", ")
  println(")")
end

@dynamo function logcalls(m...)
  ir = IR(m...)
  ir == nothing && return
  recurse!(ir)
  pushfirst!(ir, xcall(Main, :hook, arguments(ir)...))
  return ir
end

# @code_ir logcalls 2+3.0
logcalls(+, 2, 3.0)
