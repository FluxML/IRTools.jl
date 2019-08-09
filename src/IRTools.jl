module IRTools

using MacroTools
using MacroTools: prewalk, postwalk

export @code_ir

include("reflection/reflection.jl")

include("ir/ir.jl")
include("ir/utils.jl")
include("ir/wrap.jl")
include("ir/print.jl")
include("ir/parse.jl")

include("reflection/utils.jl")
include("reflection/dynamo.jl")

include("passes/passes.jl")

include("interpret.jl")
include("eval.jl")

function __init__()
  define_typeinf_code2()
end

end # module
