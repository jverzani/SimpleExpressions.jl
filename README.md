# SimpleSymbolic

[![Build Status](https://github.com/jverzani/SimpleSymbolics.jl/actions/workflows/CI.yml/badge.svg?branch=main)](https://github.com/jverzani/SimpleSymbolics.jl/actions/workflows/CI.yml?query=branch%3Amain)


A very lightweight package to express univariate scalar functions as expression using a symbolic variable and optional parameter.

Example

```
using SimpleSymbolics
@symbolic x p
map(x^2, (1,2))  # (1,4)
```
