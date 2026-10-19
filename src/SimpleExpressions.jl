"""
    SimpleExpressions

$(joinpath(@__DIR__, "..", "README.md") |>
  x -> join(Base.Iterators.drop(readlines(x), 5), "\n"))

"""
module SimpleExpressions
import TupleTools
include("CallableExpressions/CallableExpressions.jl")
using .CallableExpressions
using CommonEq
using TermInterface
import AssociativeCommutativePatternMatching
const ACPM = AssociativeCommutativePatternMatching
import AssociativeCommutativePatternMatching: @rule, MatchDict, unwrap_const

export @symbolic

include("types.jl")
include("terms.jl")
include("constructors.jl")
include("decl.jl")
include("equations.jl")
include("terminterface.jl")
include("ops.jl")
include("show.jl")
include("introspection.jl")
include("call.jl")
include("comparison.jl")
include("generators.jl")
include("scalar-derivative.jl")
include("replace.jl")

include("simplify.jl")
include("polynomial-fns.jl")
include("solve_interface.jl")
include("solve.jl")
include("precompile.jl")
end
