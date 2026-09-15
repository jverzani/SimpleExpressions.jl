#=
$(joinpath(@__DIR__, "..", "README.md") |>
  x -> join(Base.Iterators.drop(readlines(x), 5), "\n")) |>
u -> replace(u, "```julia" => "```jldoctest readme"))
=#
"""
    SimpleExpressions

$(joinpath(@__DIR__, "..", "README.md") |>
  x -> join(Base.Iterators.drop(readlines(x), 5), "\n"))

"""
module SimpleExpressions
import TupleTools
include("CallableExpressions/CallableExpressions.jl")
using .CallableExpressions

using Combinatorics
using CommonEq
using TermInterface

export @symbolic

include("types.jl")
include("constructors.jl")
include("decl.jl")
include("equations.jl")
include("terminterface.jl")
include("ops.jl")
include("combine.jl")
include("show.jl")
include("introspection.jl")
include("call.jl")
include("replace.jl")
include("comparison.jl")
include("generators.jl")
include("scalar-derivative.jl")
include("polynomial-fns.jl")
include("solve.jl")

include("SymbolicIntegration/rule2-mods.jl")
include("SymbolicIntegration/rule2.jl")
include("simplify.jl")
end
