# SimpleExpressions

[![Docs](https://img.shields.io/badge/docs-dev-blue.svg)](https://jverzani.github.io/SimpleExpressions.jl/dev)

[![Build Status](https://github.com/jverzani/SimpleExpressions.jl/actions/workflows/CI.yml/badge.svg?branch=main)](https://github.com/jverzani/SimpleExpressions.jl/actions/workflows/CI.yml?query=branch%3Amain)

A *very* lightweight means to create callable functions using expressions. This uses  [CallableExpressions](https://juliahub.com/ui/Packages/General/SimpleExpressions) as a backend. See also  [DynamicExpressions](https://juliahub.com/ui/Packages/General/DynamicExpressions) for a performant package with similar abilities.

The [`@symbolic`](@ref) macro, the lone export, can create a symbolic variable and optional symbolic parameter. When expressions are created with these variables, evaluation is deferred until the expression is called like a function. The expressions subtype `Function` so are intended to be useful with `Julia`'s higher-order functions.

The expressions can be evaluated as a univariate function, `u(x)`, a univariate function with parameter, `u(x, p)`, or as a bivariate function, `u(x,y)` (with `y` being a parameter). These are all typical calling patterns when a function is passed to a numeric routine.  For expressions without a symbolic value (as can happen through substitution) `u()` will evaluate the value.

To substitute in for either the variable or the parameter, leaving a symbolic expression, we have the calling patterns `u(:,p)`, `u(x,:)` to substitute in for the parameter and variable respectively. The colon can also be `nothing` or `missing`. There are also methods for `replace` that allow more complicated substitutions.

There are no performance claims, this package is all about convenience.  Similar convenience is available in some form with `SymPy`, `SymEngine`, `Symbolics`, etc. As well, placeholder syntax is available in `Underscores.jl`, `Chain.jl`, `DataPipes.jl` etc., This package only has value in that it is very lightweight and, hopefully, intuitively simple.

Performance is good though, as `CallableExprssions` is performant. A benchmark case of finding a zero of a function runs without allocations in `0.000003 seconds`, with a symbolic expression in  `0.000036` seconds with 275 allocations (one order of magnitude slower), as compared to a symbolic expression with SymPy which takes `0.067234` seconds with 82.94 k allocations.

An extension is provided for functions in `SpecialFunctions`.

An extension is provided for `TermInterface` which should allow the use of `Metatheory` to rewrite terms. (Once the `3.0` version is tagged.)

An extension is provided for `AbstractTrees`.

An extension for `Latexify` and `RecipesBase` is provided.




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
u = x^5 - x - p   # ((x ^ 5) - x) - p
plot(u(:, 1), 0, 1.5)
plot!(u(:, 2))    # or plot(u.(:, 1:2), 0, 1.5)
```
