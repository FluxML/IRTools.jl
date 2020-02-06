# Implementation of the amb operator using shift/reset
# http://community.schemewiki.org/?amb

include("continuations.jl");

struct Backtrack end

function require(x)
  x || throw(Backtrack())
  return
end

unwrap(e) = e
unwrap(e::CapturedException) = e.ex

function amb(iter)
  shift() do k
    for x in iter
      try
        return k(x)
      catch e
        unwrap(e) isa Backtrack || rethrow()
      end
    end
    throw(Backtrack())
  end
end

function ambrun(f)
  try
    @reset f()
  catch e
    e isa Backtrack || rethrow()
    error("No possible combination found.")
  end
end

ambrun() do
  x = amb([1, 2, 3])
  y = amb([1, 2, 3])
  require(x^2 + y == 7)
  (x, y)
end

ambrun() do
  N = 20
  i = amb(1:N)
  j = amb(i:N)
  k = amb(j:N)
  require(i*i + j*j == k*k)
  (i, j, k)
end
