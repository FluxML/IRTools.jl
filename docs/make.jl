using Documenter, IRTools

makedocs(
  modules=[IRTools],
  sitename="IRTools",
  pages = [
        "Home" => "index.md",
        "Dynamo" => "dynamo.md",
        "Reference" => "reference.md"],
  # Many docstrings are not yet included in the manual and some cross-references
  # are unresolved; warn instead of erroring so the build still succeeds.
  warnonly = [:missing_docs, :cross_references],
  format = Documenter.HTML(prettyurls = haskey(ENV, "CI")))

deploydocs(
    repo = "github.com/MikeInnes/IRTools.jl.git",
)
