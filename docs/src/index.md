# SimpleExpressions.jl

Documentation for `SimpleExpressions` a *very* lightweight means to create callable functions using expressions.

## Rationale

`Julia` has easy-to-use "anonymous" functions defined through the pattern `(args) -> body` using `->`, notation which mirrors common math notation. However, for students the distinction between an expression, such as defines the "`body`" and a function is sometimes not made, whereas in `Julia` or other computer languages, the distinction is forced. The `SymPy` package, as well as other symbolic package like `Symbolics` and `Symengine`, allows symbolic expressions to be created naturally from symbolic variables. This package does just this (and does not provide the many other methods for manipulating symbolic expressions that make using a CAS so powerful). The symbolic expressions subtype `Function`, so can be used where functions are expected.

The envisioned usage is within resource-constrained environments, such as `binder.org`.

To keep things as simple as possible, there are only few types of symbolic values: a symbolic value, a symbolic parameter and symbolic equations. Symbolic numbers may be used internally. Symbolic values and parameters are created with the `@symbolic` macro. For `@symbolic`, the first argument names the symbolic variable, the optional second names the symbolic parameter. Symbolic expressions are built up naturally by using these two types of objects; symbolic equations are specified with the infix `~` operator.

The symbolic expressions are just "thunks" or delayed expressions (akin to [Thunks.jl](https://github.com/tbenst/Thunks.jl)), where the operation and its arguments are kept in a structure and the expression is evaluated when called as a function. It is important to note that when calling the symbolic expression different symbolic variables are treated as a singleton instance; similarly for parameters.


## Usage

A quick example showing how expressions may be called:

```@example expressions
using SimpleExpressions
```

```@example expressions
@symbolic x p
u = exp(-x) * (sin(3x) + sin(101*x))
u(2)
```

This is akin, but different from using a function:

```@example expressions
f(x) = exp(-x) * (sin(3x) + sin(101*x))
f(2)
```

The main difference being, `u` can subsequently be algebraically manipulated.


The parameter can also be used:

```@example expressions
u = cos(x) - p * x
u(pi/4, 4)
```

The variable or parameter can be substituted in for:

```@example expressions
u(pi/4), u(:, 4)
```

The calling pattern for a symbolic expression `ex` is

* `ex(x)` to substitute in for `x`
* `ex(x,p)` to fill in for the variable and the parameter, and
* `ex(:, p)` to substitute in for just the parameter.

Substitution takes a symbolic expression and returns a number or a symbolic expression.

It is worth pointing out, variables are singletons even if they print differently:

```@example expressions
@symbolic y
u = x^2 - y^2
```

Evaluating this `u` will always produce `0`, as both `x` and `y` (both are symbolic variables, not parameters) receive the same value on substitution:

```@example expressions
u(10)
```

The values for `x` or `p` may be containers. For example:

```@example expressions
@symbolic x a
u = sum(aᵢ * x^(i-1) for (i,aᵢ) ∈ enumerate(a))
u(:, (1,2,3,4))
```

(This is relatively untested.)

### Equations

The package grew out of a desire to have a simpler approach to solving `f(x) = g(x)`. While defining `h(x) = f(x) - g(x)` and solving `h(x) = 0` using, say, `Roots` is straightforward, it does cause confusion while learning.

Symbolic equations are specified using `~`, a notation borrowed from `Symbolics` for `SymPy` and now on loan to `SimpleExpressions`. Of course `=` is assignment, and `==` and `===` are used for comparisons, so some other syntax is necessary and `~` plays the role of distinguishing the left- and right-hand sides of an equation.

By default, when calling a symbolic equation the difference of the left- and right-hand sides is used, so, in this case, symbolic equations can be passed directly to `find_zero`:

```@example expressions
using Roots
@symbolic x p
find_zero(cos(x) ~ sin(x), (0, pi/2)) # use bisection
```

```@example expressions
find_zero(cos(x) ~ p*x, (0, pi/2), p=3)
```

For plotting a symbolic equation, `ex`, the values `ex.lhs` and `ex.rhs` may be used separately to produce a pair of traces.

### Derivatives

Symbolic expressions can be easily differentiated, though the operator is not exported. The operator differentiates with respect to the symbolic variable assuming it represents a scalar quantity:

```@example expressions
import SimpleExpressions: D
@symbolic x p
D(cos(x) - x * p)
```

Here the derivative is used to take a step of Newton's method::

```@example expressions
u = x^5 - x - 1
du = D(u)
x0 = 2
x0 - u(x0) / du(x0)
```

Here the product rule is used:

```@example expressions
u = D(exp(x) * (sin(3x) + sin(101x)))
```

No simplification is done so the expressions can quickly become unwieldy. There is`TermInterface` so rewriting of expressions, as is possible with the `Metatheory.jl` package is possible. Though currently this is expecting a development version of Metatheory. For example, with the development version, this pattern can factor out `exp(x)`

```
using Metatheory
r = @rule (~x * ~a + ~x * ~b --> ~x * (~a + ~b))
r(u)
```
