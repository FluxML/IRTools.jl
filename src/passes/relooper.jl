function reaching(c::CFG, i::Integer, rs = Int[]; ignore = Int[])
  for j in c.graph[i]
    j in rs && return rs
    j in ignore && continue
    push!(rs, j)
    reaching(c, j, rs, ignore = ignore)
  end
  return rs
end

struct Simple
  block::Int
  next
end

Simple(n) = Simple(n, nothing)

struct Loop
  inner
  next
end

struct Multiple
  inner::Vector{Any}
  next
end

for T in [Simple, Loop, Multiple]
  @eval Base.:(==)(a::$T, b::$T) = $(reduce((a, b) -> :($a&&$b), [:(a.$f == b.$f) for f in fieldnames(T)]))
end

function reaches_unique(rs)
  rs = Dict(b => union(b, cs) for (b, cs) in rs)
  others(b) = union([cs for (c, cs) in rs if b != c]...)
  Dict(b => filter(c -> !any(rs -> c in rs, others(b)), cs) for (b, cs) in rs)
end

function reloop_loop(cfg::CFG, blocks, entry, done)
  rs = Dict(b => reaching(cfg, b, ignore = done) for b in blocks)
  body = filter(b -> any(e -> e in rs[b], entry), blocks)
  next = setdiff(blocks, body)
  Loop(reloop(cfg, blocks = body, entry = entry, done = union(done, entry)),
       reloop(cfg, blocks = next, entry = union([intersect(cfg[b], next) for b in body]...), done = done))
end

function reloop(cfg::CFG; blocks = 1:length(cfg.graph), entry = [1], done = Int[])
  (isempty(blocks) || isempty(entry)) && return
  rs = Dict(b => reaching(cfg, b, ignore = done) for b in entry)
  if length(entry) == 1 && entry[] ∉ rs[entry[]]
    next = setdiff(blocks, entry[])
    Simple(entry[],
           reloop(cfg, blocks = next,
                  entry = intersect(cfg.graph[entry[]], blocks), done = done))
  elseif all(b -> isempty(setdiff(entry, rs[b])), entry)
    reloop_loop(cfg, blocks, entry, done)
  elseif (rsu = reaches_unique(rs); !all(isempty, values(rsu)))
    unique = filter(b -> !isempty(rsu[b]), entry)
    inner = [reloop(cfg, blocks = union(b, rsu[b]), entry = [b], done = done) for b in unique]
    next = setdiff(blocks, union([union(b, rsu[b]) for b in unique]...))
    es = intersect(next, union(entry, reaching.((cfg,), unique)...))
    Multiple(inner, reloop(cfg, blocks = next, entry = es, done = done))
  else
    reloop_loop(cfg, blocks, entry, done)
  end
end

const indent = "  "

printstructure(io::IO, ::Nothing, level) = nothing

function printstructure(io::IO, s::Simple, level)
  println(io, indent^level, s.block)
  printstructure(io, s.next, level)
end

_printstructure(io::IO, s, level) = printstructure(io, s, level)

function _printstructure(io::IO, s::Simple, level)
  println(io, indent^level, "Simple:")
  printstructure(io, s, level+1)
end

function printstructure(io::IO, s::Multiple, level)
  println(io, indent^level, "Multiple:")
  if length(s.inner) == 1
    printstructure(io, s.inner[1], level+1)
  else
    for b in s.inner
      _printstructure(io, b, level+1)
    end
  end
  printstructure(io, s.next, level)
end

function printstructure(io::IO, s::Loop, level)
  println(io, indent^level, "Loop:")
  printstructure(io, s.inner, level+1)
  printstructure(io, s.next, level)
end

function Base.show(io::IO, b::Union{Simple,Multiple,Loop})
  println(io, "Structured CFG:")
  printstructure(io, b, 0)
end

# AST Conversion

rename(env, ex) =
  prewalk(x -> x isa Variable ? env[x] :
          x isa Slot ? Symbol(x.id) :
          x, ex)

entry(s::Simple) = [s.block]
entry(s::Loop) = entry(s.inner)
entry(s::Multiple) = union(entry.(s.inner)...)
entry(s::Nothing) = Int[]

struct ASTCtx
  ir::IR
  args::Dict{Variable,Symbol}
  branches::Dict{Any,Any}
  inline::Bool
end

function ast(cx::ASTCtx, b::Block)
  usages = Dict()
  prewalk(b) do x
    x isa Variable && (usages[x] = get(usages, x, 0)+1)
    x
  end
  exs = []
  env = Dict{Any,Any}(cx.args)
  for v in keys(b)
    ex = b[v].expr
    if cx.inline && get(usages, v, 0) == 1 && !(ex isa Slot) # TODO not correct
      env[v] = rename(env, ex)
    elseif get(usages, v, 0) == 0
      isexpr(ex) && push!(exs, rename(env, ex))
    else
      tmp = gensym("tmp")
      push!(exs, :($tmp = $(rename(env, ex))))
      env[v] = tmp
    end
  end
  return exs, env
end

ast(cx::ASTCtx, ::Nothing) = @q (;)

function ast(cx::ASTCtx, cfg::Simple)
  b = block(cx.ir, cfg.block)
  exs, env = ast(cx, b)
  function nextblock(br)
    if isreturn(br)
      @q (return $(rename(env, returnvalue(br)));)
    elseif haskey(cx.branches, br.block)
      cx.branches[br.block] == :continue && cfg.next == nothing ? @q((;)) :
        @q ($(Expr(cx.branches[br.block]));)
    elseif cfg.next isa Multiple && br.block in entry(cfg.next)
      n = findfirst(i -> br.block in entry(i), cfg.next.inner)
      ast(cx, cfg.next.inner[n])
    elseif cfg.next isa Multiple && br.block in entry(cfg.next.next)
      ast(cx, cfg.next.next)
    elseif br.block in entry(cfg.next)
      ast(cx, cfg.next)
    else
      @q (;)
    end
  end
  @assert length(branches(b)) <= 2 "No more than 2 branches supported, currently."
  ex = if length(branches(b)) == 1
    @q begin
      $(exs...)
      $(nextblock(branches(b)[1]).args...)
    end
  else
    @q begin
      $(exs...)
      if $(rename(env, branches(b)[1].condition))
        $(nextblock(branches(b)[2]).args...)
      else
        $(nextblock(branches(b)[1]).args...)
      end
    end
  end
  cfg.next isa Multiple && (ex = @q ($(ex.args...); $(ast(cx, cfg.next.next).args...)))
  return ex
end

function ast(cx::ASTCtx, cfg::Multiple)
  error("Only structured control flow is currently supported.")
end

function ast(cx::ASTCtx, cfg::Loop)
  for e in entry(cfg)
    cx.branches[e] = :continue
  end
  for e in entry(cfg.next)
    cx.branches[e] = :break
  end
  brs = branches(block(cx.ir, cfg.inner.block))
  if length(brs) == 2 && brs[1].block in entry(cfg.next) && brs[2].block in entry(cfg.inner.next)
    exs, env = ast(cx, block(cx.ir, cfg.inner.block))
    cond = unblock(@q ($(exs...); $(rename(env, brs[1].condition))))
    @q begin
      while $cond
        $(ast(cx, cfg.inner.next).args...)
      end
      $(ast(cx, cfg.next).args...)
    end
  else
    @q begin
      while true
        $(ast(cx, cfg.inner).args...)
      end
      $(ast(cx, cfg.next).args...)
    end
  end
end

function reloop(ir::IR; inline = true)
  ir = explicitbranch!(copy(ir))
  cfg = reloop(CFG(ir))
  args = Dict(v => Symbol(:arg, i) for (i, v) in enumerate(arguments(ir)))
  ast(ASTCtx(ir, args, Dict(), inline), cfg)
end
