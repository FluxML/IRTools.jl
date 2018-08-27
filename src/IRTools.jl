__precompile__(false)

module IRTools

using MacroTools
using MacroTools: prewalk, postwalk

export @code_ir

include("reflection/reflection.jl")

include("ir/ir.jl")
include("ir/wrap.jl")
include("ir/print.jl")
include("ir/parse.jl")
include("ir/utils.jl")

include("reflection/utils.jl")

include("interpret.jl")

end # module
