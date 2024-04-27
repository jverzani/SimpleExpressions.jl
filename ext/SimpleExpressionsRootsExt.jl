module SimpleExpressionsRootsExt

using SimpleExpressions
using Roots
import Roots.CommonSolve: solve

# use solve(..., x₀, M)
# or solve(..., (a,b),M) for find_zero
# using solve(..., a, b) for find_zeros
"""
    solve(ex::SymbolicEquation, x, args...; kwargs...)
    solve(ex::SymbolicEquation, a::Real, b::Real; kwargs...)

The first case uses `find_zero`, the second `find_zeros`.

"""
function solve(ex::SimpleExpressions.SymbolicEquation, x₀, args...; kwargs...)
    find_zero(ex, x₀; kwargs...)
end

function solve(ex::SimpleExpressions.SymbolicEquation, a::Real, b::Real; kwargs...)
    find_zeros(ex, (a,b); kwargs...)
end

# what do do with find_zeros (AllZeros?)

end
