using IRTools, Test
using IRTools: Meta, TypedMeta, meta, typed_meta

@generated f(x) = :(x+x)

@test meta(Tuple{typeof(gcd),Int,Int}) isa Meta

@test meta(Tuple{typeof(f),Int}) isa Meta

@test typed_meta(Tuple{typeof(gcd),Int,Int}) isa TypedMeta
@test typed_meta(Tuple{typeof(f),Int}) isa TypedMeta
