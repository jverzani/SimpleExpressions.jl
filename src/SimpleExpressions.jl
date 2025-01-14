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
# export ≪, ≦, Eq, ⩵, ≶, ≷, ≫, ≧ # \ll, \leqq, \Equal,\lessgtr, \gtrless, \gg,\geqq


export @symbolic

include("types.jl")
include("constructors.jl")
include("equations.jl")
include("terminterface.jl")
#include("metatheory.jl")
include("ops.jl")
include("show.jl")
include("introspection.jl")
include("call.jl")
include("replace.jl")
include("comparison.jl")
include("generators.jl")
include("scalar-derivative.jl")
include("simplify.jl")
include("solve.jl")

end
