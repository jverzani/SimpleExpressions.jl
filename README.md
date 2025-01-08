# SimpleExpressions

[![Docs](https://img.shields.io/badge/docs-dev-blue.svg)](https://jverzani.github.io/SimpleExpressions.jl/dev)

[![Build Status](https://github.com/jverzani/SimpleExpressions.jl/actions/workflows/CI.yml/badge.svg?branch=main)](https://github.com/jverzani/SimpleExpressions.jl/actions/workflows/CI.yml?query=branch%3Amain)

A *very* lightweight means to create callable functions using expressions. This uses  [CallableExpressions](https://juliahub.com/ui/Packages/General/SimpleExpressions) as a backend. See also  [DynamicExpressions](https://juliahub.com/ui/Packages/General/DynamicExpressions) for a performant package with similar abilities.

The [`@symbolic`](@ref) macro, the lone export, can create a symbolic variable and optional symbolic parameter. When expressions are created with these variables, evaluation is deferred until the expression is called like a function. The expressions subtype `Function` so are intended to be useful with `Julia`'s higher-order functions.

The expressions can be evaluated as a univariate function, `u(x)`, a univariate function with parameter, `u(x, p)`, or as a bivariate function, `u(x,y)` (with `y` being a parameter). These are all typical calling patterns when a function is passed to a numeric routine.  For expressions without a symbolic value (as can happen through substitution) `u()` will evaluate the value.

To substitute in for either the variable or the parameter, leaving a symbolic expression, we have the calling patterns `u(:,p)`, `u(x,:)` to substitute in for the parameter and variable respectively. The colon can also be `nothing` or `missing`.

When using positional arguments in a  call, as above, all symbolic variables are treated identically, as are all symbolic parameters.

There are also methods for `replace` that allow more complicated substitutions. For `replace`, symbolic objects are returned. For `replace`, variables are distinct and identified by their symbol. Pairs may be specified to the call notation as a convenience for `replace`.

There are no performance claims, this package is all about convenience.  Similar convenience is available in some form with `SymPy`, `SymEngine`, `Symbolics`, etc. As well, placeholder syntax is available in `Underscores.jl`, `Chain.jl`, `DataPipes.jl` etc., This package only has value in that it is very lightweight and, hopefully, intuitively simple.

Performance is good though, as `CallableExpressions` is performant. A benchmark case of finding a zero of a function runs without allocations in `1.099 μs` with `0` allocations, with a symbolic expression in  `1.231 μs` with `0` allocations, `SymEngine` is two orders of magnitude slower (`302.329 μs` with `1731` allocations), and SymPy is about four orders slower (and with `80k` allocations).

Extensions are provided for `SpecialFunctions`, `AbstractTrees`, `Latexify`, and `RecipesBase`.

# Example

```julia
using SimpleExpressions
@symbolic x       # (x,)
u = sin(x) - (x - x^3/6)
u(0.5)  # 0.000258...
u = u - x^5/120
u(0.5) # -1.544...e-6
```

```julia
map(x^2, (1, 2))  # (1, 4)
```

```julia
using Plots
@symbolic x p     # (x, p)
u = x^5 - x - p   # (x ^ 5) + (-1 * x) + (-1 * p)
plot(u(:, 1), 0, 1.5)
plot!(u(:, 2))    # or plot(u.(:, 1:2), 0, 1.5)
eq = cos(x) ~ 2x
plot(eq, 0, pi/2) # like plot([eq...], 0, pi/2)
```
