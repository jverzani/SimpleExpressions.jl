var documenterSearchIndex = {"docs":
[{"location":"reference/#SimpleExpressions.jl","page":"Reference/API","title":"SimpleExpressions.jl","text":"","category":"section"},{"location":"reference/","page":"Reference/API","title":"Reference/API","text":"Modules = [SimpleExpressions]","category":"page"},{"location":"reference/#SimpleExpressions.SimpleExpressions","page":"Reference/API","title":"SimpleExpressions.SimpleExpressions","text":"SimpleExpressions\n\nA very lightweight means to create callable functions using expressions.\n\nThe @symbolic macro, the lone export, can create a symbolic variable and optional symbolic parameter. When expressions are created with these variables, evaluation is deferred until the expression is called like a function.\n\nThe expressions subtype Function so are intended to be useful with Julia's higher-order functions. The expressions can be called either as u(x) or u(x, p), a typical means to pass a function to a numeric routine. These calls substitute in for the symbolic value (and parameter) when not specified as nothing. (To substitute in for just the parameter, either u(nothing, value) or u(:,value).)\n\nThere are no performance claims, this package is all about convenience. Similar convenience is available in some form with SymPy, SymEngine, Symbolics, etc. As well, placeholder syntax is available in Underscores.jl, Chain.jl, DataPipes.jl etc., This package only has value in that it is very lightweight and, hopefully, intuitively simple.\n\nAn extension is provided for functions in SpecialFunctions.\n\nAn extension is provided for TermInterface which should allow the use of Metatheory to rewrite terms.\n\nExample\n\nusing SimpleExpressions\n@symbolic x\nmap(x^2, (1, 2)) # (1, 4)\n\nusing Plots\n@symbolic x p\nu = x^5 - x - p\nplot(u(:, 1), 0, 1.5) # substitute in for p\nplot!(u(:, 2))\n\n\n\n\n\n","category":"module"},{"location":"reference/#SimpleExpressions.D-Tuple{SimpleExpressions.SymbolicExpression}","page":"Reference/API","title":"SimpleExpressions.D","text":"D(::AbstractSymbolic)\n\nFinds derivative of a symbolic expression.\n\nassumes a symbolic value is a scalar and takes derivative with respect to that; symbolic parameters are assumed to be constants\nThere is no simplification, so the output is not necessarily friendly\nlimited to a select set of functions\n\nExample\n\njulia> @symbolic x p\n(x, p)\n\njulia> D(exp(sin(x)))\n(1 * cos(x)) * exp(sin(x))\n\njulia> D(D(sin(x))) + sin(x) # no simplification!\n(-(sin(x))) + sin(x)\n\n```\n\n\n\n\n\n","category":"method"},{"location":"reference/#SimpleExpressions.@symbolic-Tuple","page":"Reference/API","title":"SimpleExpressions.@symbolic","text":"@symbolic x [p]\n\nCreate a symbolic variable and optional symbolic parameter.\n\nExpressions and equations\n\nExpressions created using these variables subclass Function so may be used where functions are expected.\n\nThe  ~ infix operator can be used to create equations, which, by default, are treated as lhs - rhs when used as functions.\n\nExtended help\n\nExample\n\nusing SimpleExpressions\n@symbolics x\nu = x^5 - x - 1\nu(2) # 29 call is u(x)\n\n@symbolic x p\nu = sum(x .* p)\nu(2, [1,2]) # 6  call is u(x, p)\n\nCalling with nothing in a slot leaves the variable\n\n@symbolic x p\nu = cos(x) - p*x\nu(nothing, 2)  # cos(x) - 2 * x\nu(:, 2)        #  cos(x) - 2 * x, alternate calling form\nu(pi, nothing) # -1.0 - p * π\n\nThe main use is as an easier-to-type replacement for anonymous functions, though with differences:\n\n1 |> sin(x) |> x^2  # sin(1)^2\nu = cos(x) - p*x\n2 |> u(:, 3) # u(2,3) alternative\n\nmap(x^2, (1, 2)) # (1,4)\n\nCan be used with other packages, to simplify some function calls at the expense of being non-idiomatic:\n\nusing Roots\n@symbolic x p\nfind_zero(x^5 - x - 1, 1)     # 1.167...\nfind_zero(x^5 - x ~ p, 1, 4)  # 1.401...\n\nusing ForwardDiff\nBase.adjoint(𝑓::Function) = x -> ForwardDiff.derivative(𝑓, x)\nu = x^5 - x - 1\nfind_zero((u,u'), 1, Roots.Newton()) # 1.167...\n\nOr\n\nusing Plots\nplot(x^5 - x - 1, 0, 1.5)\n\nSymbolic derivatives can be taken with respect to the symbolic value, symbolic parameters are treated as constant.\n\n@symbolic x p\nD = SimpleExpressions.D  # not exported\nu = x^5 - p*x - 1\nD(u)           # (5 * (x ^ 4)) - p\nu = u(:, 1)    # set parameter\na, b = 1, 2\nfind_zeros(D(u) ~ (u(b)-u(a)) / (b-a), (a,b)) # [1.577…]\n\nExtended help\n\nUsing this is a convenience for simple cases. It is easy to run into idiosyncrasies.\n\nExpressions are not functions in terms of scope\n\nUnlike functions, expressions are defined with variables at the time of definition, not when called. For example, with a clean environment:\n\n@symbolic x\nu = m*x + b    # errors, `m` not defined\nf(x) = m*x + b # ok\nm, b = 1, 2\nu = m*x + b    # defined using `m` amd `b` at time of assignment\nu(3)           # 1 * 3 + 2\nf(3)           # 1 * 3 + 2 values of `m` and `b` when called\nm, b = 3, 4\nu(3)           # still computing 1 * 3 + 2\nf(3)           # computing 3 * 3 + 4, using values of `m` and `b` when called\n\nSymbolic values are really singletons\n\nThough one can make different symbolic variables, they are all indistinguishable for purposes of evaluation:\n\n@symbolic x\n@symbolic y    # both x, y are `Symbolic` type\nu = x + 2y\nu(3)           # 9 or 3 + 2*3\n\nSimilarly for symbolic parameters. The variables may be used as containers though, e.g. u=sum(xi*pi for (xi, pi) in zip(x,p)); u((1,2),(3,4)).\n\nBroadcasting as a function\n\nThere is a difference – which needs to be corrected – where it is best to wrap the expression in a container for broadcasting. We can see it here in this artificial example:\n\n@symbolic x\nmap(x^2, [1,2])    # [1, 4]\nmap.(x^2, [1,2])   # map.(x^2, [1, 2]) ... not desirable\nmap.([x^2], [1,2]) # [1, 4]\n\n\n\n\n\n","category":"macro"},{"location":"reference/#SimpleExpressions.@symbolic_expression-Tuple{Any}","page":"Reference/API","title":"SimpleExpressions.@symbolic_expression","text":"@symbolic_expression expr\n\nTake a function call and return a symbolic (delayed) expression.\n\nExample\n\n@symbolic x\nu = @symbolic_expression quadgk(sin, 0, x)\n\n# from ?foldl\nu = @symbolic_expression foldl(=>, @symbolic_expression(1:x))\nu(4) # ((1 => 2) => 3) => 4\n\nNot exported\n\n\n\n\n\n","category":"macro"},{"location":"#SimpleExpressions.jl","page":"Home","title":"SimpleExpressions.jl","text":"","category":"section"},{"location":"","page":"Home","title":"Home","text":"Documentation for SimpleExpressions a very lightweight means to create callable functions using expressions.","category":"page"},{"location":"#Rationale","page":"Home","title":"Rationale","text":"","category":"section"},{"location":"","page":"Home","title":"Home","text":"Julia has easy-to-use \"anonymous\" functions defined through the pattern (args) -> body using ->, notation which mirrors common math notation. However, for students the distinction between an expression, such as defines the \"body\" and a function is sometimes not made, whereas in Julia or other computer languages, the distinction is forced. The SymPy package, as well as other symbolic package like Symbolics and Symengine, allows symbolic expressions to be created naturally from symbolic variables. This package does just this (and does not provide the many other methods for manipulating symbolic expressions that make using a CAS so powerful). The envisioned usage is with resource-constrained environments, such as binder.org. The symbolic expressions subtype Function, so can be used where functions are expected.","category":"page"},{"location":"","page":"Home","title":"Home","text":"To keep things as simple as possible, there are only few types of symbolic values: a symbolic value, a symbolic parameter, symbolic numbers, and symbolic equations. The first two are created with the @symbolic macro, the latter with the ~ infix operator. For @symbolic, the first argument names the symbolic variable, the optional second names the symbolic parameter. It is important to note that when calling the symbolic expression different symbolic variables are treated as a singleton instance; similarly for parameters.","category":"page"},{"location":"#Usage","page":"Home","title":"Usage","text":"","category":"section"},{"location":"","page":"Home","title":"Home","text":"A quick example showing how expressions may be called:","category":"page"},{"location":"","page":"Home","title":"Home","text":"using SimpleExpressions","category":"page"},{"location":"","page":"Home","title":"Home","text":"@symbolic x p\nu = exp(-x) * (sin(3x) + sin(101*x))\nu(2)","category":"page"},{"location":"","page":"Home","title":"Home","text":"Or using a parameter:","category":"page"},{"location":"","page":"Home","title":"Home","text":"u = cos(x) - p * x\nu(pi/4, 4)","category":"page"},{"location":"","page":"Home","title":"Home","text":"Or leaving the parameter or variable unevaluated:","category":"page"},{"location":"","page":"Home","title":"Home","text":"u(pi/4), u(:, 4)","category":"page"},{"location":"","page":"Home","title":"Home","text":"The calling pattern is ex(x) to substitute in for x, ex(x,p) to fill in for the variable and the parameter, and ex(:, p) to substitute in for just the parameter. Substitution takes a symbolic expression and returns a number or a symbolic expression.","category":"page"},{"location":"","page":"Home","title":"Home","text":"It is worth pointing out, variables are singletons even if they print differently:","category":"page"},{"location":"","page":"Home","title":"Home","text":"@symbolic y\nu = x^2 - y^2","category":"page"},{"location":"","page":"Home","title":"Home","text":"Evaluating will always produce 0, as both x and y (both are symbolic variables, not parameters) receive the same value on substitution:","category":"page"},{"location":"","page":"Home","title":"Home","text":"u(10)","category":"page"},{"location":"","page":"Home","title":"Home","text":"The values for x or p may be containers. For example:","category":"page"},{"location":"","page":"Home","title":"Home","text":"@symbolic x a\nu = sum(aᵢ * x^(i-1) for (i,aᵢ) ∈ enumerate(a))\nu(:, (1,2,3,4))","category":"page"},{"location":"","page":"Home","title":"Home","text":"(This is relatively untested.)","category":"page"},{"location":"#Equations","page":"Home","title":"Equations","text":"","category":"section"},{"location":"","page":"Home","title":"Home","text":"The package grew out of a desire to have a simpler approach to solve f(x) = g(x). While defining h(x) = f(x) - g(x) and solving h(x) = 0 using, say, Roots is straightforward, it does cause confusion while learning.","category":"page"},{"location":"","page":"Home","title":"Home","text":"Symbolic equations are specified using ~, a notation borrowed from Symbolics for SymPy and now on loan to SimpleExpressions. Of course = is assignment, and == and === are used for comparisons, so some over syntax is necessary and ~ plays the role of distinguishing the left- and right-hand sides of an equation.","category":"page"},{"location":"","page":"Home","title":"Home","text":"The MTH229Lite package defines the following method for solve:","category":"page"},{"location":"","page":"Home","title":"Home","text":"import Roots\nsolve(ex::SimpleExpressions.SymbolicEquation, x0, args...; kwargs...) =\n    Roots.find_zero(ex, x0, args...; kwargs...)","category":"page"},{"location":"","page":"Home","title":"Home","text":"@symbolic x\nsolve(sin(x) ~ cos(x), (0, pi/2))","category":"page"},{"location":"","page":"Home","title":"Home","text":"(Another dispatch is used to call find_zeros).","category":"page"},{"location":"","page":"Home","title":"Home","text":"By default, when evaluating a symbolic equation the difference of the left- and right-hand sides is used, so no special use of find_zero is needed. The solve verb is introduced to parallel its use in SymPy for symbolic solutions to equations.","category":"page"},{"location":"","page":"Home","title":"Home","text":"The MTH229Lite package also defines a plot method for symbolic equations that plots both the left-hand side (ex.lhs) and the right-hand side (ex.rhs); basically just plot([eq.lhs, eq.rhs], a, b).","category":"page"},{"location":"#Derivatives","page":"Home","title":"Derivatives","text":"","category":"section"},{"location":"","page":"Home","title":"Home","text":"Symbolic expressions can be easily differentiated, though the operator is not exported. The operator differentiates with respect to the symbolic variable assuming it represents a scalar quantity:","category":"page"},{"location":"","page":"Home","title":"Home","text":"import SimpleExpressions: D\n@symbolic x p\nD(cos(x) - x * p)","category":"page"},{"location":"","page":"Home","title":"Home","text":"Here is a step of Newton's method::","category":"page"},{"location":"","page":"Home","title":"Home","text":"u = x^5 - x - 1\ndu = D(u)\nx0 = 2\nx0 - u(x0) / du(x0)","category":"page"},{"location":"","page":"Home","title":"Home","text":"Here the product rule is used:","category":"page"},{"location":"","page":"Home","title":"Home","text":"u = D(exp(x) * (sin(3x) + sin(101x)))","category":"page"},{"location":"","page":"Home","title":"Home","text":"No simplification is done so the expressions can quickly become unwieldy. There is an extension for TermInterface so rewriting of expressions, as is possible with the Metatheory.jl package is possible. For example, this pattern can factor out exp(x):","category":"page"},{"location":"","page":"Home","title":"Home","text":"using Metatheory\nr = @rule (~x * ~a + ~x * ~b --> ~x * (~a + ~b))\nr(u)","category":"page"}]
}
