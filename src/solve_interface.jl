# Hooks between the generic equation solver (solve.jl) and SimpleExpressions.
#
# solve.jl walks expressions with TermInterface (`iscall`, `operation`,
# `arguments`), builds them with ordinary operators and functions (`+`, `*`,
# `^`, `sqrt`, `log`, `zero(x)`, ...) and compares them with `isequal`.
# Everything else it needs from a symbolic package is defined here.

import CommonSolve
import CommonSolve: solve

## numbers
# no variables or parameters
_isnumeric(u) = is_number(u)
# the value of a numeric expression
_evaluate(u) = u()
# wrap a number as a constant, keeping it exact (e.g. 1//2 stays rational)
_constant(v::Number) = SymbolicNumber(v)
# is u such a wrapped constant (a leaf)?
_isconstantleaf(u) = u isa SymbolicNumber

## symbols
# the free symbols (variables and parameters) of ex, as a tuple
_symbols(ex) = (fs = free_symbols(ex); (fs.x..., fs.p...))
# does ex contain the symbol (or subexpression) x?
_has(ex, x) = contains(ex, x)
# a new variable, distinct from the user's
_fresh(name::Symbol) = SymbolicVariable(name)
# ex with symbols replaced by numbers, given as pairs `x => 2`
_substitute_values(ex, prs...) = ex(prs...)

## building
# the expression op(args...), like ex
_rebuild(ex, op, args) = maketerm(AbstractSymbolic, op, args, nothing)
# the unevaluated comparison op(a, b), e.g. p ≥ 0
_comparison(op, a, b) = SymbolicExpression(op, (a, b))
# the equation l ~ r returned to the user
_equation(l, r) = l ~ r

## polynomials
# polynomial-fns.jl: _poly_coeffs(ex, x) (coefficients in x, or nothing),
# _poly_add, _poly_mul, _poly_pow on coefficient vectors

## entry point
"""
    solve(eq::SymbolicEquation, x; conditions::Bool=false, interval=nothing)

Very *simple* symbolic equations can be solved with the unexported `solve` method.

* `eq::SymbolicEquation`:. equations are specified with `~`, as in `x^2 ~ p`
* `x`: variable to solve for
* `conditions`: set to true to reveal conditions on parameters for which the identified solution(s) hold.
* interval: If specified, through a tuple `(a,b)` all solutions in this interval are found.

# Return value:

* an equation `x ~ r` when there is a single solution;
* a tuple of equations, e.g. `(x ~ r₁, x ~ r₂)`, when there are more
  (quadratics, even powers);
* `()` when numeric values show there are no real solutions;
* `nothing` when `x` does not appear in the equation (or cancels out);
* an equation whose left-hand side is not `x` when `x` could not be isolated.

# Examples

```julia
import SimpleExpressions: solve
SimpleExpressions.@symbolic_variables A a B b # (A, a, B, b)
solve(sin(A)/a ~ sin(B)/b, b) # b ~ (a * sin(B)) / sin(A)
solve(sin(A)/a ~ sin(B)/b, A) # A ~ asin((sin(B) * a) / b)
```

```julia
SimpleExpressions.@symbolic_variables w p h  # (w, p, h)
constraint = p ~ 2w + 2h;
A = w * h;
u = solve(constraint, h)      # h ~ (p / 2) + (-1 * w)
A = A(u)                      # w * ((p / 2) + (-1 * w)), use equation in replacement
v = solve(diff(A, w) ~ 0, w)  # w ~ p / 4
```

# Extended help

The equation is rearranged by moving terms free of `x` to the right-hand side
and applying inverse functions (e.g. `exp(u) ~ r` becomes `u ~ log(r)`,
`abs(u) ~ r` becomes `u ~ ±r`). A product equal to `0` is solved factor by
factor. Polynomials in `x` are solved when they are linear or quadratic, after
factoring out powers of `x`, when they are a polynomial in `xᵏ`
(e.g. `x^4 - 5x^2 + 4`), or when their numeric rational coefficients allow
roots to be found by the rational root theorem.

Answers are kept exact where possible: rational values stay rational and
other values are left as symbolic numbers, e.g. `x^2 ~ 8` gives `x ~ ±2⋅sqrt(2)`,
`exp(x) ~ 2` gives `x ~ log(2)`, `2^x ~ 8` gives `x ~ 3` and `sin(x) ~ 1//2`
gives `x ~ π/6` (standard values of `asin`, `acos` and `atan` are recognized).
Calling a symbolic number, e.g. `solve(x^2 ~ 2, x)[2].rhs()`, gives its
floating point value. When the equation has floating point values, so does
the answer.

Some further rewriting is done:

* an injective function on both sides is removed, e.g. `log(u) ~ log(v)`
  becomes `u ~ v`; exponentials on both sides are compared through their
  logarithms, e.g. `2^x ~ 3^(x+1)`;
* sums of logarithms are combined, `log(u) + log(v) ~ r` becomes `log(u⋅v) ~ r`;
* rational functions, e.g. `1/x + 1/(x+1) ~ 1`, are solved through their
  numerator, discarding roots of the denominator;
* a polynomial in some `g(x)`, e.g. `exp(2x) - 3exp(x) + 2 ~ 0` or
  `sqrt(x) + x ~ 6`, is solved for `g(x)` first;
* logarithms in any base (`log`, `log10`, `log2`, `log(b, u)`) are combined,
  `Σ kᵢ⋅log_b(uᵢ) ~ r` becomes `Π uᵢ^kᵢ ~ b^r`;
* a square root is isolated and both sides squared, e.g. `2x ~ sqrt(x+3)`;
* `abs(u)` is split into the cases `u ≥ 0` and `u ≤ 0`;
* a polynomial factor common to all terms is factored out, e.g.
  `2x - x*exp(6x-1) ~ 0`;
* `a⋅cos(u) + b⋅sin(u) ~ 0` becomes `tan(u) ~ -a/b`;
* a sum of exponentials of one sign equal to `0` has no solutions;
* equations mixing `x` with an exponential or logarithm of a linear
  function of `x`, e.g. `x*exp(x) ~ 1`, `2^x ~ x + 1`, `x + log(x) ~ 2` or
  `x^x ~ 4`, are solved with the Lambert W function: `lambertw(z)` and, for
  `-1/e ≤ z < 0`, the second real branch `lambertw(z, -1)` (from LambertW.jl).
  Simple values are recognized, e.g. `x^x ~ 4` gives `x ~ 2`.

When the equation has no symbolic parameters, numeric solutions are checked in
the original equation and extraneous ones are discarded.

With symbolic parameters (or other variables), each solution is valid only
under some conditions: logarithms of positive values, square roots of
nonnegative ones, nonzero denominators, and conditions from squaring or from
splitting `abs`. These are evaluated at the solution: a solution failing a
numeric condition is discarded, as is one which fails at every sampled value
of the parameters. Conditions holding at every sampled value are dropped (this
is a heuristic, not a proof). Pass `conditions=true` to have each solution
returned as a pair `eq => conds` of the equation and a tuple of the
remaining conditions, e.g.

```julia
solve(sqrt(x) ~ p, x; conditions=true)     # x ~ p^2 => (p >= 0,)
solve(abs(x - p) ~ 2x, x; conditions=true) # (x ~ -p => (p <= 0,), x ~ p/3 => (p >= 0,))
```

The check uses Julia's definitions, so e.g. `x^(1//3)` is not defined for negative `x`
(`(-8.0)^(1//3)` is a `DomainError`); use `cbrt(x)` for the real cube root.


Pass `interval=(a, b)` to get every real solution with `a ≤ x ≤ b` (a closed
interval). Periodic functions then give all their branches, e.g.
`sin(u) ~ r` gives `u = asin(r) + 2πn` and `u = π - asin(r) + 2πn`, and those
in the interval are listed:

```julia
solve(4sin(3x) ~ 2, x; interval=(0, 4π/3))     # (π/18, 5π/18, 13π/18, 17π/18)
solve(x^2 - 5x + 6 ~ 0, x; interval=(0, 2.5))  # x ~ 2; other equations are filtered
```

The equation must not have parameters when `interval` is given. Without
`interval`, inverse trigonometric functions give principal values.

!!! warning
    Inverse functions are applied without regard to domain or range
    restrictions and only the *principal value* is returned. For example,
    `sin(x) ~ a` gives `x ~ asin(a)` and not the infinitely many other
    solutions; `sqrt(u) ~ r` gives `u ~ r^2`, which may introduce extraneous
    solutions when `r` is negative. Symbolic answers are not checked; check them.

!!! note
    The original solve.jl was rewritten and extended with Claude AI.
"""
function CommonSolve.solve(eq::SymbolicEquation, x::𝑉; conditions::Bool = false, interval = nothing)
    solve_equation(eq.lhs, eq.rhs, x; conditions, interval)
end

CommonSolve.init(eq::SymbolicEquation) = throw(ArgumentError("Must specify variable to solve for"))
