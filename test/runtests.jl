using SimpleExpressions
using Test

import SimpleExpressions: @symbolic_expression

include("basic_tests.jl")
include("test_match.jl")
include("test_aqua.jl")
## too slow right now
#include("extension_tests.jl")
