# SimpleExpressions

[![Docs](https://img.shields.io/badge/docs-dev-blue.svg)](https://jverzani.github.io/SimpleExpressions.jl/dev)

[![Build Status](https://github.com/jverzani/SimpleExpressions.jl/actions/workflows/CI.yml/badge.svg?branch=main)](https://github.com/jverzani/SimpleExpressions.jl/actions/workflows/CI.yml?query=branch%3Amain)

A *very* lightweight means to create callable functions using expressions.


The [`@symbolic`](@ref) macro, the lone export, can create a symbolic variable and optional symbolic parameter. When expressions are created with these variables, evaluation is deferred until the expression is called like a function.

The expressions subtype `Function` so are intended to be useful with `Julia`'s higher-order functions. The expressions can be called either as `u(x)` or `u(x, p)`, a typical means to pass a function to a numeric routine. These calls substitute in for the symbolic value (and parameter) when not specified as `nothing`. (To substitute in for just the parameter, either `u(nothing, value)` *or* `u(:,value)`.)

There are no performance claims, this package is all about convenience. Similar convenience is available in some form with `SymPy`, `SymEngine`, `Symbolics`, etc. As well, placeholder syntax is available in `Underscores.jl`, `Chain.jl`, `DataPipes.jl` etc., This package only has value in that it is very lightweight and, hopefully, intuitively simple.

An extension is provided for functions in `SpecialFunctions`.

An extension is provided for `TermInterface` which should allow the use of `Metatheory` to rewrite terms.




# Example

```
using SimpleExpressions
@symbolic x
map(x^2, (1, 2)) # (1, 4)
```

```
using Plots
@symbolic x p
u = x^5 - x - p
plot(u(:, 1), 0, 1.5) # substitute in for p
plot!(u(:, 2))
```
