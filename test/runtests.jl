using SimpleExpressions
using Test

import SimpleExpressions: @symbolic_expression

include("basic_tests.jl")

include("test_match.jl")

include("test_simplify.jl")
include("test_limit.jl")
# include("test_aqua.jl") # failing with piracy
