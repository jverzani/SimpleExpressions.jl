using SimpleExpressions
using Test

import SimpleExpressions: @symbolic_expression

include("basic_tests.jl")
## too slow right now
#include("extension_tests.jl")
