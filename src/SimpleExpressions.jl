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
using CallableExpressions
import TermInterface
import TermInterface: iscall, operation, arguments, sorted_arguments,
    maketerm, is_operation, metadata
using Combinatorics
using CommonEq

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
include("matchpy.jl")
include("replace.jl")
include("comparison.jl")
include("generators.jl")
include("scalar-derivative.jl")
include("solve.jl")

#include("simplify.jl") # wait for Metatheory v3.0 tag


end
