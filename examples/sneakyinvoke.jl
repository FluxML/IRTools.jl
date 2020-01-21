using IRTools: @dynamo, argument!, IR


# Implementation of `invoke(f, argtypes::Type, args...`, where `args` don't have
# to conform to `argtypes`.  I.e., it allows you to call `f` on `args`, but using the
# method for `argtypes`.
# See https://julialang.slack.com/archives/CJ357CY2Y/p1579598240011500.

concrete(::Type{Type{T}}) where {T} = T

function _sneaky_transform(f::Type, Types::Type{<:Tuple})
    ir = IR(f, Types.parameters...)
    argument!(ir, at = 2)
    return ir
end

@dynamo function sneakyinvoke(f, T, args...)
    Types = concrete(T)
    return _sneaky_transform(f, Types)
end

@show sneakyinvoke(+, Tuple{Int32, Int32}, 2.0, 2.34)
@show Base.add_int(2.0, 2.34)
