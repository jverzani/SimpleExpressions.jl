# SimpleExpressions.jl

Documentation for `SimpleExpressions` a *very* lightweight means to create callable functions using expressions. This package leverages the `CallableExpressions` package.

## Rationale

`Julia` has easy-to-use "anonymous" functions defined through the pattern `(args) -> body` using `->`, notation which mirrors common math notation. However, for students the distinction between an expression, such as defines the "`body`" and a function is sometimes not made, whereas in `Julia` or other computer languages, the distinction is forced. The `SymPy` package, as well as other symbolic packages in `Julia` like `Symbolics` and `SymEngine`, allows symbolic expressions to be created naturally from symbolic variables. This package does just this (and does not provide the many other methods for manipulating symbolic expressions that make using a CAS so powerful). The symbolic expressions subtype `Function`, so can be used where functions are expected.

The envisioned usage is within resource-constrained environments, such as `binder.org`.

To keep things as simple as possible, there are only a few types of symbolic values:  symbolic numbers, symbolic variables, symbolic parameters, symbolic expressions, and symbolic equations.

Symbolic variables and parameters are created with the `@symbolic` macro. For the `@symbolic` macro, the first argument names the symbolic variable, the optional second names the symbolic parameter.

Symbolic expressions are built up naturally by using these two types of objects.

Symbolic equations are specified with the infix `~` operator.

Symbolic numbers can be produced from substitution.

The symbolic expressions are just "thunks" or delayed expressions (akin to [Thunks.jl](https://github.com/tbenst/Thunks.jl)) but implemented in a more performant manner in `CallableExpressions`, where the operation and its arguments are kept in a structure and the expression is evaluated when called as a function.


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
u(pi/4,:), u(:, 4)
```

### Evaluation

The calling pattern for a symbolic expression `ex` is simple: the first positional argument is for the symbolic value, the second for the symbolic parameter. Leading to:

* `ex(x)` to evaluate the expression of just the variable with the value of  `x`; an error is thrown if the expression has both a variable and a parameter.
* `ex(x,p)` to evaluate an expression of both a  variable and a parameter; if there is no parameter the value of `p` is ignored.
* `ex(*, p)` to evaluate an expression of just a parameter. The `*` in the `x` slot can be any valid identifier (except for `:`, `nothing`, or `missing`, as they are used for substitution), it is just ignored.
* `ex()` to evaluate an expression that involves neither a symbolic variable or a parameter.

### Substitution

Evaluation leaves a non-symbolic value. For substitution, the result is still symbolic. The syntax for substitution is:

* `ex(:, p)` to substitute in for the parameter.
* `ex(x, :)` to substitute in for the variable.
* `replace(ex, args::Pair...)` to substitute in for either a variable or a parameter. The pairs are specified as `variable_name => replacement_value`.
* `ex(args::Pair...)` redirects to `replace(ex, args::Pair...)`

The use of `:` to indicate the remaining value is borrowed from Julia's array syntax; it can also be either `nothing` or `missing`.

The design of `SimpleExpressions` is to only allow one variable and one parameter in a given expression and to assign these variables to positional arguments. This is just to simplify the usage. The underlying `CallableExpressions` package allows greater flexibility.

Two or more variables can be used, as here:

```@example expressions
@symbolic y
u = x^2 - y^2
```

Evaluating `u` with a value in the `x` position will error. This is a deliberate design limitation; it can be worked around via `replace`:

```julia
u(1,2) # ERROR: more than one variable
```

Whereas

```@example expressions
v = replace(u, x=>1, y=>2) # the symbolic value ((1^2)-(2^2))
v()                        # evaluates to -3
```


The values for `x` or `p` may be replaced by containers. For example:

```@example expressions
@symbolic x a
u = sum(aᵢ * x^(i-1) for (i,aᵢ) ∈ enumerate(a))
u(2, (1,2,3,4)) # 49
```

This is relatively untested and almost certainly not fully featured. For example, only evaluation is allowed, not substitution (using `:`):

```
@symbolic x a
u = sum(ai * x^(i-1) for (i,ai) in enumerate(a))
u(2, [1,2,3])
```

## Equations

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

For plotting a symbolic equation, `ex`, the values `ex.lhs` and `ex.rhs` may be used separately to produce a pair of traces. (With `Plots` there is a recipe do plot a symbolic equation as two separate functions; it does not plot the difference of the two functions.)

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
