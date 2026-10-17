## ---- precompile workload
##
## Compiling the pattern matcher and rule machinery dominates the first call
## to `simplify` or `expand`. Expressions encode their structure in their
## type, so a new expression still compiles code for its own shape; this
## workload covers the shared, shape-independent part.

using PrecompileTools

@setup_workload begin
    x, y = SymbolicVariable(:x), SymbolicVariable(:y)
    @compile_workload begin
        simplify(sin(x)^2 + cos(x)^2 + y)
        simplify(2log(x) + 2log(y))
        simplify(x^2 * x^3)
        simplify(exp(x) * exp(y))
        expand(sin(x + y))
    end
end
