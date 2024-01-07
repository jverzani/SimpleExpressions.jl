# SimpleExpressions

[![Docs](https://img.shields.io/badge/docs-dev-blue.svg)](https://jverzani.github.io/SimpleExpressions.jl/dev)

[![Build Status](https://github.com/jverzani/SimpleExpressions.jl/actions/workflows/CI.yml/badge.svg?branch=main)](https://github.com/jverzani/SimpleSymbolics.jl/actions/workflows/CI.yml?query=branch%3Amain)


A very lightweight package to create expressions involving a symbolic variable and optional parameter. These are convenient with higher-order functions expecting mathematical functions.

Example

```julia
using SimpleExpressions
@symbolic x p
map(x^2, (1,2))  # (1,4)

using Plots
plot(x^5 - x - 1, 0, 1.5)
```
