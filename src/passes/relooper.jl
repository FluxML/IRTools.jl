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
       reloop(cfg, blocks = next, entry = union([intersect(rs[b], next) for b in body]...), done = done))
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

function ast(b::Block, args, branches, inline)
  usages = Dict()
  prewalk(b) do x
    x isa Variable && (usages[x] = get(usages, x, 0)+1)
    x
  end
  exs = []
  env = Dict{Any,Any}(args)
  for v in keys(b)
    ex = b[v].expr
    if inline && get(usages, v, 0) == 1 && !(ex isa Slot) # TODO not correct
      env[v] = rename(env, ex)
    elseif get(usages, v, 0) == 0
      push!(exs, rename(env, ex))
    else
      tmp = gensym("tmp")
      push!(exs, :($tmp = $(rename(env, ex))))
      env[v] = tmp
    end
  end
  return exs, env
end

ast(ir::IR, ::Nothing; args, branches = Dict(), inline) = nothing

function ast(ir::IR, cfg::Simple; args, branches = Dict(), inline)
  b = block(ir, cfg.block)
  exs, env = ast(b, args, branches, inline)
  x = :nothing
  for br in reverse(IRTools.branches(b))
    y = isreturn(br) ?
      Expr(:return, rename(env, returnvalue(br))) :
      :(__label__ = $(br.block))
    haskey(branches, br.block) && (y = @q ($y; $(Expr(branches[br.block]))))
    isconditional(br) ? (x = :($(rename(env, br.condition)) ? $x : $y)) :
      x = y
  end
  push!(exs, x)
  @q begin
    $(exs...)
    $(ast(ir, cfg.next, args = args, branches = branches, inline = inline))
  end
end

function ast(ir::IR, cfg::Multiple; args, branches = Dict(), inline)
  conds = [:(__label__ == $(s.block)) for s in cfg.inner]
  body = [ast(ir, s, args = args, branches = branches) for s in cfg.inner]
  ex = Expr(:elseif, conds[end], body[end])
  ex = foldr((i, x) -> Expr(:elseif, conds[i], body[i], x), 1:length(conds)-1, init = ex)
  ex.head = :if
  return @q ($ex; $(ast(ir, cfg.next; args = args, branches = branches, inline = inline)))
end

function ast(ir::IR, cfg::Loop; args, branches = Dict())
  for e in entry(cfg)
    branches[e] = :continue
  end
  for e in entry(cfg.next)
    branches[e] = :break
  end
  @q begin
    while true
      $(ast(ir, cfg.inner, args = args, branches = branches, inline = inline))
    end
    $(ast(ir, cfg.next, args = args, branches = branches, inline = inline))
  end
end

function reloop(ir::IR; inline = true)
  cfg = reloop(CFG(ir))
  args = Dict(v => Symbol(:arg, i) for (i, v) in enumerate(arguments(ir)))
  ast(ir, cfg, args = args, inline = inline)
end
