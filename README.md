# SimpleSymbolic

[![Build Status](https://github.com/jverzani/SimpleSymbolics.jl/actions/workflows/CI.yml/badge.svg?branch=main)](https://github.com/jverzani/SimpleSymbolics.jl/actions/workflows/CI.yml?query=branch%3Amain)

A very lightweight package to express univariate scalar functions as expression using a symbolic variable and optional parameter.

Example

```
using Roots, SimpleSymbolics
@symbolic x p
find_zero(x^5 - x - 1, 1)
```

This is an alternative to `find_zero(x -> x^5 - x - 1, 1)`.

```
find_zero(x^5 - x ~ p, (1, 2), 2)
```

This is an alternative to `find_zero((x,p) -> x^5 - x - p, (1,2), 2)`. The use of `~` makes using equations a tad easier.

## ? TermInterface, MetaTheory
