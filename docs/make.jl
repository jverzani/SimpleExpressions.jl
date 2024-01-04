using Documenter
using SimpleExpressions

makedocs(
    sitename = "SimpleExpressions",
    format = Documenter.HTML(),
    modules = [SimpleExpressions]
)

# Documenter can also automatically deploy documentation to gh-pages.
# See "Hosting Documentation" and deploydocs() in the Documenter manual
# for more information.
deploydocs(
     repo = "github.com/jverzani/SimpleExpressions.jl"
)
