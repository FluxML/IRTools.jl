# API Reference

This page provides a comprehensive reference for IRTools functionality.

## Reflection

```@docs
@code_ir
IRTools.meta
IRTools.typed_meta
IRTools.@meta
IRTools.@typed_meta
```

## IR Manipulation

```@docs
IRTools.IR
IRTools.Statement
IRTools.Variable
IRTools.Branch
IRTools.BasicBlock
IRTools.arguments
IRTools.argtypes
IRTools.branches
IRTools.isconditional
IRTools.isreturn
IRTools.returnvalue
IRTools.Inner.returntype
IRTools.canbranch
IRTools.explicitbranch!
IRTools.argument!
IRTools.emptyargs!
IRTools.deletearg!
IRTools.block!
IRTools.Inner.deleteblock!
IRTools.block
IRTools.blocks
IRTools.successors
IRTools.predecessors
push!
pushfirst!
insert!
IRTools.insertafter!
empty
keys
haskey
IRTools.Pipe
```
