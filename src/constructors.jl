## ---- constructors
"""
    @symbolic x [p]

Create a symbolic variable and optional symbolic parameter.

# Expressions and equations

Expressions created using these variables subclass `Function` so may be used where functions are expected.

The  `~` infix operator can be used to create equations, which, by default, are treated as `lhs - rhs` when called as functions.

# Extended help

## Calling or substituting into expressions

To call a symbolic expression regular call notation with positional arguments are used. The first argument maps to *any* symbolic variable; the second -- when given -- to any symbolic parameter. It is an error to call an expression with a parameter using just a single argument; for that substitution is needed.

## Example

```julia
using SimpleExpressions
@symbolic x p
u = x^5 - x - 1
u(2) # 29 call is u(x)

u.((0,1,2)) # (-1, -1, 29)

u = 2x + p
u(1)    # errors!
u(1, 2) # 2(1)+2 or 4

u = sum(x .* p)
u(2, [1,2]) # 6  call is u(x, p)
```

Calling with `nothing`, `missing`, or `:` in a slot *substitutes* in the specified value leaving a symbolic expression, possibly with no variable or parameter.

```julia
@symbolic x p
u = cos(x) - p*x
u(nothing, 2)  # cos(x) - 2 * x
u(:, 2)        #  cos(x) - 2 * x, alternate calling form
u(pi, nothing) # -1.0 - p * π
v = u(1,:)(:,2)    # (cos(1)-(2*1)),
```

The latter can be evaluated using a zero-argument call, e.g. `v()`.

With substitution in this manner, any symbolic variable and any symbolic parameters will receive the same substituted value.

The [`replace`](@ref) generic for symbolic objects takes pairs of values and replaces the left one with the right one working from left to right, leaving a symbolic expression. The `replace` method treats symbolic variables and symbolic parameters with different symbols as unique.

A symbolic equation, defined through `~`, may also be used to specify a left- and right-hand value.

The main use is as an easier-to-type replacement for anonymous functions, though with differences:

```julia
1 |> sin(x) |> x^2  # 0.708… from sin(1)^2
u = cos(x) - p*x
2 |> u(:, 3) # -6.4161…, a alternative to u(2,3)
```

```julia
map(x^2, (1, 2)) # (1,4)
```

Symbolic expressions an be used with other packages, to simplify some function calls at the expense of being non-idiomatic:

```julia
using Roots
@symbolic x p
find_zero(x^5 - x - 1, 1)       # 1.167…
find_zero(x^5 - x ~ p, 1; p=4)  # 1.401…

using ForwardDiff
Base.adjoint(𝑓::Function) = x -> ForwardDiff.derivative(𝑓, x)
u = x^5 - x - 1
find_zero((u,u'), 1, Roots.Newton()) # 1.167…
```

Or

```julia
using Plots
plot(x^5 - x - 1, 0, 1.5)
```

Or using both positions, so that we call as a bivariate function:

```julia
@symbolic x y
xs = ys = range(-5, 5, length=100)
contour(xs, ys, x^2 - y^2 + 2x*y)
```

Symbolic derivatives can be taken with respect to the symbolic value, symbolic parameters are treated as constant.

```julia
@symbolic x p
import SimpleExpressions: D
u = x^5 - p*x - 1
D(u)           # (5 * (x ^ 4)) - p
u = u(:, 1)    # set parameter
a, b = 1, 2
find_zeros(D(u) ~ (u(b)-u(a)) / (b-a), (a,b)) # [1.577…]
```


## Idiosyncrasies

Using this is a convenience for *simple* cases. It is easy to run into idiosyncrasies.

### Expressions are not functions in terms of scope

Unlike functions, expressions are defined with variables at the time of definition, not when called. For example, with a clean environment:

```julia
@symbolic x
u = m*x + b    # errors, `m` not defined
f(x) = m*x + b # ok
m, b = 1, 2
u = m*x + b    # defined using `m` amd `b` at time of assignment
u(3)           # 1 * 3 + 2
f(3)           # 1 * 3 + 2 values of `m` and `b` when called
m, b = 3, 4
u(3)           # still computing 1 * 3 + 2
f(3)           # computing 3 * 3 + 4, using values of `m` and `b` when called
```

### Symbolic values are really singletons when calling by position

Though one can make different symbolic variables, the basic call
notation by position treats them as the same:

```julia
@symbolic x
@symbolic y    # both x, y are `SymbolicVariable` type
u = x + 2y
u(3)           # 9 coming from 3 + 2*(3)
```

However, this is only to simplify the call interface. Using *keyword* arguments allows evaluation with different values:

```julia
u(;x=3, y=2)   # 7
```

Using `replace`, we have:

```julia
u(x=>3, y=>2)  # 3 + (2 * 2); evaluate with u(x=>3, y=>2)()
```

The underlying `CallableExpressions` object is directly called in the above manner; that package does not have the narrowed design of this package.

## Containers

The variables may be used as placeholders for containers, e.g.

```julia
u = sum(xi*pi for (xi, pi) in zip(x,p))
u((1,2),(3,4))  # 11
```


## Broadcasting as a function

Broadcasting a function call works as expected

```julia
@symbolic x
u = x^2
u.((1,2)) # (1, 4)
```

Symbolic expressions can also be constructed that will broadcast the call

```julia
u = x.^2 .+ sin.(p)
u((1,2),3)

u = @. x^2 + sin(p)
u((1,2),(3,4))
```

"""
macro symbolic(x...)
    q=Expr(:block)
    push!(q.args, Expr(:(=), esc(x[1]), Expr(:call, SymbolicVariable, Expr(:quote, x[1]))))
    if length(x) > 1
        push!(q.args, Expr(:(=), esc(x[2]), Expr(:call, SymbolicParameter, Expr(:quote, x[2]))))
    end
    push!(q.args, Expr(:tuple, map(esc, x)...))
    q
end


macro symbolic_expression(expr)
    @assert expr.head === :call
    op = expr.args[1]
    args = expr.args[2:end]
    Expr(:call, SymbolicExpression, esc(op),  Expr(:tuple, map(esc,args)...))
end
