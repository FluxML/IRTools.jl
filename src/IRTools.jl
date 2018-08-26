__precompile__(false)

module IRTools

using MacroTools

export @code_ir

include("reflection.jl")

include("ir/ir.jl")
include("ir/wrap.jl")
include("ir/print.jl")
include("ir/parse.jl")
include("ir/utils.jl")

include("interpret.jl")

end # module
