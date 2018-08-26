module Wrap

import Core: SSAValue, GotoNode, Compiler
import Core: Typeof
import Core.Compiler: CodeInfo, IRCode, CFG, BasicBlock, Argument, ReturnNode,
  NullLineInfo, just_construct_ssa, compact!, NewNode, InferenceState, OptimizationState,
  GotoIfNot, PhiNode, PiNode, StmtRange, IncrementalCompact, insert_node!, insert_node_here!,
  compact!, finish, DomTree, construct_domtree, dominates, userefs, widenconst, types, verify_ir

for T in :[IRCode, IncrementalCompact, UseRef, UseRefIterator, TypesView].args
  @eval begin
    Base.getindex(ir::Compiler.$T, a...) = Compiler.getindex(ir, a...)
    Base.setindex!(ir::Compiler.$T, a...) = Compiler.setindex!(ir, a...)
  end
end

for T in :[UseRefIterator, IncrementalCompact, Pair].args
  @eval Base.iterate(x::Compiler.$T, a...) = Compiler.iterate(x, a...)
end

Base.getindex(r::StmtRange, i) = (r.first:r.last)[i]

PhiNode(x, y) = PhiNode(Any[x...], Any[y...])

CFG(bs) = CFG(bs, map(b -> b.stmts.first, bs[2:end]))

# SSA contruction (forked from Base for untyped code)

import Core.Compiler: normalize, strip_trailing_junk!, compute_basic_blocks,
  scan_slot_def_use, LineInfoNode, construct_ssa!, IR_FLAG_INBOUNDS

function just_construct_ssa(ci::CodeInfo, code::Vector{Any}, nargs::Int, sp)
  ci.ssavaluetypes = Any[Any for _ = 1:length(code)]
  slottypes = Any[Any for _ = 1:length(ci.slotnames)]
  inbounds_depth = 0 # Number of stacked inbounds
  meta = Any[]
  flags = fill(0x00, length(code))
  for i = 1:length(code)
    stmt = code[i]
    if isexpr(stmt, :inbounds)
      arg1 = stmt.args[1]
      if arg1 === true # push
        inbounds_depth += 1
      elseif arg1 === false # clear
        inbounds_depth = 0
      elseif inbounds_depth > 0 # pop
        inbounds_depth -= 1
      end
      stmt = nothing
    else
      stmt = normalize(stmt, meta)
    end
    code[i] = stmt
    if !(stmt === nothing)
      if inbounds_depth > 0
        flags[i] |= IR_FLAG_INBOUNDS
      end
    end
  end
  strip_trailing_junk!(ci, code, flags)
  cfg = compute_basic_blocks(code)
  defuse_insts = scan_slot_def_use(nargs, ci, code)
  domtree = construct_domtree(cfg)
  ir = let code = Any[nothing for _ = 1:length(code)]
    argtypes = slottypes[1:(nargs+1)]
    IRCode(code, Any[], ci.codelocs, flags, cfg, collect(LineInfoNode, ci.linetable), argtypes, meta, sp)
  end
  ir = construct_ssa!(ci, code, ir, domtree, defuse_insts, nargs, sp, slottypes)
  return ir
end

import ..IRTools: IR, TypedMeta, Meta

function IRCode(meta::TypedMeta)
  opt = OptimizationState(meta.frame)
  ir = just_construct_ssa(meta.code, deepcopy(meta.code.code),
                          Int(meta.method.nargs)-1, opt)
  # return inline_sparams!(ir, opt.sp)
  return ir
end

function IRCode(meta::Meta)
  ir = just_construct_ssa(meta.code, deepcopy(meta.code.code),
                          Int(meta.method.nargs)-1, meta.sparams)
  # return inline_sparams!(ir, meta.sparams)
  return ir
end

end
