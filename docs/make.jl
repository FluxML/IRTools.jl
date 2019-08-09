using Documenter, IRTools

makedocs(
  modules=[IRTools],
  sitename="IRTools",
  pages = [
        "Home" => "index.md",
        "Dynamo" => "dynamo.md",
        "Reference" => "reference.md"],
  format = Documenter.HTML(prettyurls = haskey(ENV, "CI")))

deploydocs(
    repo = "github.com/MikeInnes/IRTools.jl.git",
)
