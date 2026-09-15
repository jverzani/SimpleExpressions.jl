# SimpleExpressions.jl

Documentation for `SimpleExpressions`, a *very* lightweight means to create callable functions using expressions.

This package leverages the [`CallableExpressions`](https://gitlab.com/nsajko/CallableExpressions.jl) package for the heavy lifting.

## Rationale

`Julia` has easy-to-use "anonymous" functions defined through the
pattern `(args) -> body` using `->`, notation which mirrors common
math notation. However, for students the distinction between an
expression, such as defines the "`body`" and a function is sometimes
not made, whereas in `Julia` or other computer languages, the
distinction is forced. The `SymPy` package---as well as other symbolic
packages in `Julia` like `Giac`, `Symbolics` and `SymEngine`---allows
callable symbolic expressions to be created naturally from symbolic
variables by adding methods to many of `Julia`'s mathematical
function. This package does just this (but does not provide the many
other compelling features of a CAS). The symbolic expressions subtype
`Function`, so can be used where functions are expected.

To keep things as simple as possible, there are only a few types of
symbolic values: symbolic numbers, symbolic variables, symbolic
parameters, symbolic expressions, and symbolic equations.

Symbolic variables and parameters are created with the `@symbolic`
macro. For the `@symbolic` macro, the first argument names the
symbolic variable, the optional second argument names the symbolic parameter. (The unexported `@symbolic_variables` macro can define multiple variables with one call.)

Symbolic expressions are built up naturally by using these two types of objects.

Symbolic equations are specified with the infix `~` operator with the left- and right-hand sides being symbolic expressions.

Symbolic numbers can be produced from substitution.

The symbolic expressions are just "thunks" or delayed expressions (akin to [Thunks.jl](https://github.com/tbenst/Thunks.jl)) but implemented in a more performant manner in `CallableExpressions`, where the operation and its arguments are kept in a structure and the expression is evaluated when called as a function.


## Usage

A quick example showing how expressions may be defined and called:

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


The parameter can also be used to form an expression:

```@example expressions
u = cos(x) - p * x
u(pi/4, 4)
```

The variable or parameter can be substituted in for using positional arguments:

```@example expressions
u(pi/4,:), u(:, 4)
```

Or, the expression can be evaluated directly

```@example expressions
u(pi/4, 4)
```


## Evaluation

The basic calling pattern for a symbolic expression `ex` is simple: the first positional argument is for the symbolic value, the second for the symbolic parameter. All symbolic variables in `ex` are evaluated at the same value; all symbolic parameters in `ex` are evaluated at the same value.

This simplification leads to these rules:

* `ex(x)` is used to evaluate the expression of just the variable with the value of  `x`; an error is thrown if the expression has both a variable and a parameter.
* `ex(x, p)` is used to evaluate an expression of both a  variable and a parameter; if there is no parameter the value of the second argument is ignored.
* `ex(*, p)` is used to evaluate an expression of just a parameter. The `*` in the `x` slot can be any valid identifier (except for `:`, `nothing`, or `missing`, as they are used for substitution); the value of the first argument is just ignored.
* `ex()` is used to evaluate an expression that involves neither a symbolic variable or a parameter, such as can be produced through substitution.

## Substitution

Evaluation leaves a non-symbolic value. For substitution, the result is still symbolic.

The basic syntax for substitution is:

* `ex(:, p)` to substitute in for the parameter.
* `ex(x, :)` to substitute in for the variable.

The use of `:` to indicate the remaining value is borrowed from Julia's array syntax; it can also be either `nothing` or `missing`.

For substitution using positional arguments (as with evaluation), all instances of symbolic variables and all instances of symbolic parameters are treated identically.

When a `key=>value` pair is used as the argument, if the key is a symbolic variable or a symbolic parameter, then that object is replaced by the value on the right-hand side.

```@example expressions
@symbolic x p
u = (p + sqrt(x))/2
u(:,1), u(5, :) # symbolic, not numeric
```

```@example expressions
u(5, 1), u(x=>5, p=>1) # golden ratio numerically, symbolically
```



## Symbolic containers

The values for `x` or `p` may be replaced by containers. For example:

```@example expressions
@symbolic x a
u = sum(aᵢ * x^(i-1) for (i,aᵢ) ∈ enumerate(a))
u(2, (1,2,3,4)) # 49
```

This is relatively untested and almost certainly not fully featured. For example, only evaluation is allowed, not substitution (using `:`):

```@example expressions
@symbolic x a
u = sum(ai * x^(i-1) for (i,ai) in enumerate(a))
u(2, [1,2,3])
```

## Broadcasting

The package is intended to support broadcasting of expressions and the construction of broadcasting expressions.

```@example expressions
@symbolic x p
u = x^2 + p
v = @. x^2 + p
f(x,p) = x^2 + p
x0, p0 = (1,2), (3,4)
u.(x0, p0) == v(x0, p0) == f.(x0, p0) == (1^2+3, 2^2+4)
```

## Equations

The package grew out of a desire to have a simpler approach to solving `f(x) = g(x)`. While defining `h(x) = f(x) - g(x)` and solving `h(x) = 0` using, say, `Roots` is straightforward, it does cause confusion while learning.

Symbolic equations are specified using `~`, a notation borrowed from `Symbolics` for `SymPy` and now on loan to `SimpleExpressions`. Of course `=` is assignment, and `==` and `===` are used for comparisons, so some other syntax is necessary and `~` plays the role of distinguishing the left- and right-hand sides of an equation.

By default, when calling a symbolic equation the difference of the left- and right-hand sides is used, so, in this case, symbolic equations can be passed directly to the `find_zero` method from `Roots`:

```@example expressions
using Roots
@symbolic x p
find_zero(cos(x) ~ sin(x), (0, pi/2)) # use bisection
```


The `solve` interface (loaded with `Roots`) is also available for symbolic equations:

```@example expressions
solve(cos(x) ~ p*x, (0, pi/2), p=3)
```

*Linear* symbolic equations can be solved symbolically through this package (though the lack of simplification is annoying). Instead of specifying an interval, a variable to solve for is given.

```@example expressions
@symbolic a A
@symbolic b B
solve(sin(A)/a ~ sin(B)/b, A)  # solve not exported, but is imported with Roots above
```

This example shows "inverse" functions are applied (without concern for domain/range restrictions) when possible.

### Plotting

For plotting a symbolic equation, `eq`, the values `eq.lhs` and `eq.rhs` may be used separately to produce a pair of traces. With `Plots`, where a vector of functions may be plotted, `plot([eq...], a, b)` will plot each side with separate trace. Though with `Plots` there is a recipe to plot a symbolic equation as two separate functions.

### Derivatives

Symbolic expressions can be easily differentiated. A method for `diff` is used, as that name is established in some other languages. A variable to differentiate by should be specified. The operator differentiates with respect to the variable assuming it represents a scalar quantity:

```@example expressions
@symbolic x p
diff(cos(x) - x * p, x)
```

```@example expressions
diff(cos(x) ~ x * p, p)
```

Here the derivative is used to take a step of Newton's method::

```@example expressions
u = x^5 - x - 1
du = diff(u, x)
x0 = 2
x0 - u(x0) / du(x0)
```

Here the application of the product rule can be seen:

```@example expressions
u = diff(exp(x) * (sin(3x) + sin(101x)), x)
```

### `replace`

To work with multiple symbolic parameters or variables, `replace` can be used to substitute in values for a specific variable.

* `replace(ex, args::Pair...)` to substitute in for either a variable, parameter, expression head, or symbolic expression (possibly with a wildcard). The pairs are specified as `variable_name => replacement_value`.
* When a symbolic expression is called with a `key=>value` pair, the `replace` method is used. That is `ex(args::Pair...)` redirects to `replace(ex, args::Pair...)`.

To illustrate, two or more variables can be used, as here:

```@example expressions
@symbolic x
@symbolic y  # or SimpleExpressions.@symbolic_variables x y
u = x^2 - y^2
```

Evaluating `u` with a value in the `x` position will evaluate both `x` and `y` with that value:

```@example expressions
u(1)    # always 0
u(1, 2) # not 1^2 - 2^2, the second argument is ignored here
```

As indicated, this is a deliberate design limitation to simplify usage. It can be worked around via `replace`:

```@example expressions
v = u(x=>1, y=>2)          # the symbolic value ((1^2)-(2^2)), also replace(u, x=>1, y=>2)
v()                        # evaluates to -3
```

The `replace` method is a bit more involved than illustrated. The `key => value` pairs have different dispatches depending on the value of the key. Above, the key is a `SymbolicVariable`, but the key can be:

* A `SymbolicVariable` or `SymbolicParameter` in which case the simple substitution is applied, as just illustrated.

* A function, like `sin`. In this case, a matching operation head is replaced by the replacement head. Eg. `sin => cos` will replace a `sin` call with a `cos` call.

```@example expressions
v = sin(x) + sin(x^2)
replace(v, sin => cos)
```

* A symbolic expression. In this case, the exact match of the expression is replaced by the replacement value.

```@example expressions
v = 1 + (x+1)^1 + 2*(x+1)^2 + 3*(x+1)^3
replace(v, x+1 => x)
```

* A symbolic expression *with* one or more *wildcards*. Wildcards have a naming convention using trailing underscores. One matches one value; two matches one or more values; three match 0, 1, or more values. In addition, the **special** symbol `⋯` (entered with `\cdots[tab]` is wild. (The same pattern can't use, say, `x_` and `x__`.)

```@example expressions
v = log(1 + x) + log(1 + x^2/2)
@symbolic x_
replace(v, log(1 + x_) => log1p(x_)) # log1p(x) + log1p((x ^ 2) / 2)
```

Substitution uses `match(pattern, subject)` for expression matching with wildcards:

```@example expressions
subject, pattern = log(1 + x^2/2), log(1+x_)
σ = match(pattern, subject)
```

The return value is a dictionary of valid substitutions with a special sentinel for no match. The non-exported `rewrite` method can be used to substitute this dictionary into a replacement pattern.

Using the original pattern should result in an expression equivalent to the original subject.

```@example expressions
SimpleExpressions.rewrite(σ, pattern)
```

This is the final step in the last `replace` call above.

```@example expressions
SimpleExpressions.rewrite(σ, log1p(x_))
```


### Simplification

No simplification is done by default so the expressions can quickly become unwieldy. The unexported `combine` functions does light simplification by combining like terms for addition and multiplication.

```@example expressions
ex = 1 + x + 2x + 3x + 4
```

```@example expressions
using SimpleExpressions: combine
combine(ex)
```

Unlike most computer algebra systems---where the basic representation includes specialized storage for additive and multiplicative terms---this package does not. The `combine` function represents terms in this other manner, combines like terms, and then creates a expression again. This is a bit excessive to make a default, as the point here is reasonably fast callable functions.

There is also an unexported `simplify` command that uses pattern matching to perform basic simplification.


```@example expressions
using SimpleExpressions: simplify
ex = 10 * sin(2x)^2 + 10 * cos(2x)^2 - 5
simplify(ex)
```
