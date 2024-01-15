var documenterSearchIndex = {"docs":
[{"location":"#SimpleExpressions.jl","page":"SimpleExpressions.jl","title":"SimpleExpressions.jl","text":"","category":"section"},{"location":"","page":"SimpleExpressions.jl","title":"SimpleExpressions.jl","text":"Modules = [SimpleExpressions]","category":"page"},{"location":"#SimpleExpressions.SimpleExpressions","page":"SimpleExpressions.jl","title":"SimpleExpressions.SimpleExpressions","text":"SimpleExpressions\n\nA very lightweight means to create callable functions using expressions.\n\nThe @symbolic macro, the lone export, can create a symbolic variable and optional symbolic parameter. When expressions are created with these variables, evaluation is deferred until the expression is called like a function.\n\nThe expressions subtype Function so are intended to be useful with Julia's higher-order functions. The expressions can be called either as u(x) or u(x, p), a typical means to pass a function to a numeric routine. These calls substitute in for the symbolic value (and parameter) when not specified as nothing. (To substitute in for just the parameter, either u(nothing, value) or u(:,value).)\n\nThere are no performance claims, this package is all about convenience. Similar convenience is available in some form with SymPy, SymEngine, Symbolics, etc. As well, placeholder syntax is available in Underscores.jl, Chain.jl, DataPipes.jl etc., This package only has value in that it is very lightweight and, hopefully, intuitively simple.\n\nAn extension is provided for functions in SpecialFunctions.\n\nAn extension is provided for TermInterface which should allow the use of Metatheory to rewrite terms.\n\nExample\n\nusing SimpleExpressions\n@symbolic x\nmap(x^2, (1, 2)) # (1, 4)\n\nusing Plots\n@symbolic x p\nu = x^5 - x - p\nplot(u(:, 1), 0, 1.5) # substitute in for p\nplot!(u(:, 2))\n\n\n\n\n\n","category":"module"},{"location":"#SimpleExpressions.D-Tuple{SimpleExpressions.SymbolicExpression}","page":"SimpleExpressions.jl","title":"SimpleExpressions.D","text":"D(::AbstractSymbolic)\n\nFinds derivative of a symbolic expression.\n\nassumes a symbolic value is a scalar and takes derivative with respect to that; symbolic parameters are assumed to be constants\nThere is no simplification, so the output is not necessarily friendly\nlimited to a select set of functions\n\nExample\n\njulia> @symbolic x p\n(x, p)\n\njulia> D(exp(sin(x)))\n(1 * cos(x)) * exp(sin(x))\n\njulia> D(D(sin(x))) + sin(x) # no simplification!\n(-(sin(x))) + sin(x)\n\n```\n\n\n\n\n\n","category":"method"},{"location":"#SimpleExpressions.@symbolic-Tuple","page":"SimpleExpressions.jl","title":"SimpleExpressions.@symbolic","text":"@symbolic x [p]\n\nCreate a symbolic variable and optional symbolic parameter.\n\nExpressions and equations\n\nExpressions created using these variables subclass Function so may be used where functions are expected.\n\nThe  ~ infix operator can be used to create equations, which, by default, are treated as lhs - rhs when used as functions.\n\nExtended help\n\nExample\n\nusing SimpleExpressions\n@symbolics x\nu = x^5 - x - 1\nu(2) # 29 call is u(x)\n\n@symbolic x p\nu = sum(x .* p)\nu(2, [1,2]) # 6  call is u(x, p)\n\nCalling with nothing in a slot leaves the variable\n\n@symbolic x p\nu = cos(x) - p*x\nu(nothing, 2)  # cos(x) - 2 * x\nu(:, 2)        #  cos(x) - 2 * x, alternate calling form\nu(pi, nothing) # -1.0 - p * π\n\nThe main use is as an easier-to-type replacement for anonymous functions, though with differences:\n\n1 |> sin(x) |> x^2  # sin(1)^2\nu = cos(x) - p*x\n2 |> u(:, 3) # u(2,3) alternative\n\nmap(x^2, (1, 2)) # (1,4)\n\nCan be used with other packages, to simplify some function calls at the expense of being non-idiomatic:\n\nusing Roots\n@symbolic x p\nfind_zero(x^5 - x - 1, 1)     # 1.167...\nfind_zero(x^5 - x ~ p, 1, 4)  # 1.401...\n\nusing ForwardDiff\nBase.adjoint(𝑓::Function) = x -> ForwardDiff.derivative(𝑓, x)\nu = x^5 - x - 1\nfind_zero((u,u'), 1, Roots.Newton()) # 1.167...\n\nOr\n\nusing Plots\nplot(x^5 - x - 1, 0, 1.5)\n\nSymbolic derivatives can be taken with respect to the symbolic value, symbolic parameters are treated as constant.\n\n@symbolic x p\nD = SimpleExpressions.D  # not exported\nu = x^5 - p*x - 1\nD(u)           # (5 * (x ^ 4)) - p\nu = u(:, 1)    # set parameter\na, b = 1, 2\nfind_zeros(D(u) ~ (u(b)-u(a)) / (b-a), (a,b)) # [1.577…]\n\nExtended help\n\nUsing this is a convenience for simple cases. It is easy to run into idiosyncrasies.\n\nExpressions are not functions in terms of scope\n\nUnlike functions, expressions are defined with variables at the time of definition, not when called. For example, with a clean environment:\n\n@symbolic x\nu = m*x + b    # errors, `m` not defined\nf(x) = m*x + b # ok\nm, b = 1, 2\nu = m*x + b    # defined using `m` amd `b` at time of assignment\nu(3)           # 1 * 3 + 2\nf(3)           # 1 * 3 + 2 values of `m` and `b` when called\nm, b = 3, 4\nu(3)           # still computing 1 * 3 + 2\nf(3)           # computing 3 * 3 + 4, using values of `m` and `b` when called\n\nSymbolic values are really singletons\n\nThough one can make different symbolic variables, they are all indistinguishable for purposes of evaluation:\n\n@symbolic x\n@symbolic y    # both x, y are `Symbolic` type\nu = x + 2y\nu(3)           # 9 or 3 + 2*3\n\nSimilarly for symbolic parameters. The variables may be used as containers though, e.g. u=sum(xi*pi for (xi, pi) in zip(x,p)); u((1,2),(3,4)).\n\nBroadcasting as a function\n\nThere is a difference – which needs to be corrected – where it is best to wrap the expression in a container for broadcasting. We can see it here in this artificial example:\n\n@symbolic x\nmap(x^2, [1,2])    # [1, 4]\nmap.(x^2, [1,2])   # map.(x^2, [1, 2]) ... not desirable\nmap.([x^2], [1,2]) # [1, 4]\n\n\n\n\n\n","category":"macro"}]
}
