using Documenter, IRTools

makedocs(
  sitename="IRTools",
  pages = [
        "Home" => "index.md",
        "Dynamo" => "dynamo.md"],
  format = Documenter.HTML(prettyurls = haskey(ENV, "CI")))

deploydocs(
    repo = "github.com/MikeInnes/IRTools.jl.git",
)
