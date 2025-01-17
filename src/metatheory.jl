## ----- Interface
"""
    simplify(ex)

Simplify expression using `Metatheory.jl`  and rules on loan from `SymbolicUtils.jl`.
"""
function simplify()
end

"""
    expand(ex)

Expand terms in an expression using `Metatheory.jl`
"""
function expand()
end

# some default definitions
# we extend to SymbolicExpression in the Metatheroy extension
for fn ∈ (:simplify, :expand,
          :canonicalize, :powsimp, :trigsimp, :logcombine,
          :expand_trig, :expand_power_exp, :expand_log)
    @eval begin
        $fn(ex::AbstractSymbolic) = ex
        $fn(eq::SymbolicEquation) = SymbolicEquation($fn.(eq)...)
    end
end

