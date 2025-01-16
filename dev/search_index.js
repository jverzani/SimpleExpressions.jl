var documenterSearchIndex = {"docs":
[{"location":"reference/#SimpleExpressions.jl","page":"Reference/API","title":"SimpleExpressions.jl","text":"","category":"section"},{"location":"reference/","page":"Reference/API","title":"Reference/API","text":"Modules = [SimpleExpressions]","category":"page"},{"location":"reference/#SimpleExpressions.SimpleExpressions","page":"Reference/API","title":"SimpleExpressions.SimpleExpressions","text":"SimpleExpressions\n\nA very lightweight means to create callable functions using expressions. This uses  CallableExpressions as a backend. See also  DynamicExpressions for a performant package with similar abilities.\n\nThe @symbolic macro, the lone export, can create a symbolic variable and optional symbolic parameter. When expressions are created with these variables, evaluation is deferred until the expression is called like a function. The expressions subtype Function so are intended to be useful with Julia's higher-order functions.\n\nThe expressions can be evaluated as a univariate function, u(x), a univariate function with parameter, u(x, p), or as a bivariate function, u(x,y) (with y being a parameter). These are all typical calling patterns when a function is passed to a numeric routine.  For expressions without a symbolic value (as can happen through substitution) u() will evaluate the value.\n\nTo substitute in for either the variable or the parameter, leaving a symbolic expression, we have the calling patterns u(:,p), u(x,:) to substitute in for the parameter and variable respectively. The colon can also be nothing or missing.\n\nWhen using positional arguments in a  call, as above, all symbolic variables are treated identically, as are all symbolic parameters.\n\nThere are also methods for replace that allow more complicated substitutions. For replace, symbolic objects are returned. For replace, variables are distinct and identified by their symbol. Pairs may be specified to the call notation as a convenience for replace.\n\nThere are no performance claims, this package is all about convenience.  Similar convenience is available in some form with SymPy, SymEngine, Symbolics, etc. As well, placeholder syntax is available in Underscores.jl, Chain.jl, DataPipes.jl etc., This package only has value in that it is very lightweight and, hopefully, intuitively simple.\n\nPerformance is good though, as CallableExpressions is performant. A benchmark case of finding a zero of a function runs without allocations in 1.099 μs with 0 allocations, with a symbolic expression in  1.231 μs with 0 allocations, SymEngine is two orders of magnitude slower (302.329 μs with 1731 allocations), and SymPy is about four orders slower (and with 80k allocations).\n\nExtensions are provided for SpecialFunctions, AbstractTrees, Latexify, and RecipesBase.\n\nExample\n\nusing SimpleExpressions\n@symbolic x       # (x,)\nu = sin(x) - (x - x^3/6)\nu(0.5)  # 0.000258...\nu = u - x^5/120\nu(0.5) # -1.544...e-6\n\nmap(x^2, (1, 2))  # (1, 4)\n\nusing Plots\n@symbolic x p     # (x, p)\nu = x^5 - x - p   # (x ^ 5) + (-1 * x) + (-1 * p)\nplot(u(:, 1), 0, 1.5)\nplot!(u(:, 2))    # or plot(u.(:, 1:2), 0, 1.5)\neq = cos(x) ~ 2x\nplot(eq, 0, pi/2) # like plot([eq...], 0, pi/2)\n\n\n\n\n\n","category":"module"},{"location":"reference/#SimpleExpressions.SymbolicEquation","page":"Reference/API","title":"SimpleExpressions.SymbolicEquation","text":"a ~ b\n\nCreate a SymbolicEquation.\n\nThe equation has a left and right-hand side, which can be found by tuple destructing; calling first and last; by index; or field access using .lhs and .rhs.\n\nSymbolic equations can be evaluated, in which case the value of a-b is returned.\n\nWhen a symbolic equation is passed as an argument to a symbolic expression, the pair a => b is passed to replace.\n\nThe D function differentiates both sides. The solve function tries to move x terms to the left-hand side; and non-x terms to the right-hand side.\n\n\n\n\n\n","category":"type"},{"location":"reference/#Base.match-Tuple{SimpleExpressions.AbstractSymbolic, SimpleExpressions.AbstractSymbolic}","page":"Reference/API","title":"Base.match","text":"match(pattern, expression)\n\nMatch expression using a pattern with possible wildcards. Uses a partial implementation of Non-linear Associative-Commutative Many-to-One Pattern Matching with Sequence Variables by Manuel Krebber.\n\nIf there is no match: returns nothing.\n\nIf there is a match: returns a collection of substitutions (σ₁, σ₂, …) – possibly empty – with the property pattern(σ...) == expression is true.\n\nWildcards are just symbolic variables with a naming convention: use one trailing underscore to indicate a single match, two trailing underscores for a match of one or more, and three trailing underscores for a match on 0, 1, or more.\n\nExamples\n\njulia> using SimpleExpressions\n\njulia> SimpleExpressions.@symbolic_variables a b x_ x__ x___\n(a, b, x_, x__, x___)\n\njulia> p, s= x_*cos(x__), a*cos(2 + b)\n(x_ * cos(x__), a * cos(2 + b))\n\njulia> Θ = match(p, s)\n((x__ => 2 + b, x_ => a),)\n\njulia> σ = only(Θ)\n(x__ => 2 + b, x_ => a)\n\njulia> p(σ...) == s\ntrue\n\njulia> p, s =  p = x_ + x__ + x___,  a + b + a + b + a\n(x_ + x__ + x___, a + b + a + b + a)\n\njulia> Θ = match(p, s);\n\njulia> length(Θ)   # 37 matches\n37\n\njulia> σ = last(Θ)\n(x_ => b, x__ => b, x___ => a + a + a)\n\njulia> p(σ...) # a + a + (a + b + b)\nb + b + (a + a + a)\n\n\n\n\n\n","category":"method"},{"location":"reference/#Base.replace-Tuple{SimpleExpressions.AbstractSymbolic, Vararg{Pair}}","page":"Reference/API","title":"Base.replace","text":"replace(ex::SymbolicExpression, args::Pair...)\n\nReplace parts of the expression with something else.\n\nReturns a symbolic object.\n\nThe replacement is specified using variable => value; these are processed left to right.\n\nThere are different methods depending on the type of key in the the key => value pairs specified:\n\nA symbolic variable is replaced by the right-hand side, like ex(val,:), though the latter is more performant\nA symbolic parameter is replaced by the right-hand side, like ex(:,val)\nA function is replaced by the corresponding specified function, as the head of the sub-expression\nA sub-expression is replaced by the new expression.\nA sub-expression containing a wildcard is replaced by the new expression, possibly containing a wildcard, in which the arguments are called.\n\nThe first two are straightforward.\n\njulia> using SimpleExpressions\n\njulia> @symbolic x p\n(x, p)\n\njulia> ex = cos(x) - x*p\ncos(x) + (-1 * x * p)\n\njulia> replace(ex, x => 2) == ex(2, :)\ntrue\n\njulia> replace(ex, p => 2) == ex(:, 2)\ntrue\n\nThe third, is illustrated by:\n\njulia> replace(sin(x + sin(x + sin(x))), sin => cos)\ncos(x + cos(x + cos(x)))\n\nThe fourth is similar to the third, only an entire expression (not just its head) is replaced\n\njulia> ex = cos(x)^2 + cos(x) + 1\n(cos(x) ^ 2) + cos(x) + 1\n\njulia> @symbolic u\n(u,)\n\njulia> replace(ex, cos(x) => u)\n(u ^ 2) + u + 1\n\nReplacements occur only if an entire node in the expression tree is matched:\n\njulia> u = 1 + x\n1 + x\n\njulia> replace(u + exp(-u), u => x^2)\n1 + x + exp(-1 * (x ^ 2))\n\n(As this addition has three terms, 1+x is not a subtree in the expression tree.)\n\nThe fifth needs more explanation, as there can be wildcards in the expression.\n\nWildcards have a naming convention using trailing underscores. One matches one value; two matches one or more values; three match 0, 1, or more values. In addition, the special symbol ⋯ (entered with \\cdots[tab] is wild.\n\njulia> @symbolic x p; @symbolic x_\n(x_,)\n\njulia> replace(cos(pi + x^2), cos(pi + x_) => -cos(x_))\n-1 * cos(x ^ 2)\n\n\njulia> ex = log(sin(x)) + tan(sin(x^2))\nlog(sin(x)) + tan(sin(x ^ 2))\n\njulia> replace(ex, sin(x_) => tan((x_) / 2))\nlog(tan(x / 2)) + tan(tan((x ^ 2) / 2))\n\njulia> replace(ex, sin(x_) => x_)\nlog(x) + tan(x ^ 2)\n\njulia> replace(x*p, (x_) * x => x_)\np\n\nPicture\n\nThe AbstractTrees package can print this tree-representation of the expression ex = sin(x + x*log(x) + cos(x + p + x^2)):\n\njulia> print_tree(ex;maxdepth=10)\nsin\n└─ +\n   ├─ x\n   ├─ *\n   │  ├─ x\n   │  └─ log\n   │     └─ x\n   └─ cos              <--\n      └─ +             ...\n         ├─ x          <--\n         ├─ p          ...\n         └─ ^          ...\n            ├─ x       ...\n            └─ 2       ...\n\nThe command wildcard expression cos(x + ...) looks at the part of the tree that has cos as a node, and the lone child is an expression with node + and child x. The ⋯ then matches p + x^2.\n\n\n\n\n\n","category":"method"},{"location":"reference/#CommonSolve.solve-Tuple{SimpleExpressions.SymbolicEquation, Union{SimpleExpressions.SymbolicParameter, SimpleExpressions.SymbolicVariable}}","page":"Reference/API","title":"CommonSolve.solve","text":"solve(eq::SymboliclEquation, x)\n\nVery simple symbolic equations can be solved with the unexported solve method. This example shows a usage.\n\n@symbolic w p; @symbolic h  # two variables, one parameter\nimport SimpleExpressions: solve, D\nconstraint = p ~ 2w + 2h\nA = w * h\n\nu = solve(constraint, h)\nA = A(u) # use equation in replacement\nv = solve(D(A, w) ~ 0, w)\n\n\n\n\n\n","category":"method"},{"location":"reference/#SimpleExpressions.D-Tuple{SimpleExpressions.SymbolicNumber, Any}","page":"Reference/API","title":"SimpleExpressions.D","text":"D(::AbstractSymbolic, [x])\n\nFinds derivative of a symbolic expression with respect to a symbolic variable or parameter.\n\nSpecify a variable to differentiate by, otherwise the lone symbolic variable (if present) will be used\nThere is scant simplification, so the output is not necessarily friendly\nlimited to a select set of functions\n\nExample\n\njulia> @symbolic x p\n(x, p)\n\njulia> D(exp(sin(x)), x)\n(1 * cos(x)) * exp(sin(x))\n\njulia> D(D(sin(x))) + sin(x) # no simplification!\n(-(sin(x))) + sin(x)\n\nNot exported.\n\n\n\n\n\n","category":"method"},{"location":"reference/#SimpleExpressions.coefficients-Tuple{SimpleExpressions.SymbolicEquation, Any}","page":"Reference/API","title":"SimpleExpressions.coefficients","text":"coefficients(ex, x)\n\nIf expression or equation is a polynomial in x, return the coefficients. Otherwise return nothing.\n\nExample\n\njulia> @symbolic x p;\n\njulia> eq = x*(x+2)*(x-p) ~ 2;\n\njulia> a0, as... = cs = SimpleExpressions.coefficients(eq, x)\n(a₀ = -2, a₁ = -2 * p, a₂ = 2 + (-1 * p), a₃ = 1)\n\njulia> a0 + sum(aᵢ*x^i for (i,aᵢ) ∈ enumerate(Iterators.rest(cs,2)) if !iszero(aᵢ))\n-2 + (-2 * p * (x ^ 1)) + ((2 + (-1 * p)) * (x ^ 2)) + (1 * (x ^ 3))\n\nNot exported.\n\n\n\n\n\n","category":"method"},{"location":"reference/#SimpleExpressions.map_matched-Tuple{Any, Any, Any}","page":"Reference/API","title":"SimpleExpressions.map_matched","text":"map_matched(ex, is_match, f)\n\nTraverse expression. If is_match is true, apply f to that part of expression tree and reassemble.\n\nBasically CallableExpressions.expression_map_matched.\n\nNot exported.\n\n\n\n\n\n","category":"method"},{"location":"reference/#SimpleExpressions.@symbolic-Tuple","page":"Reference/API","title":"SimpleExpressions.@symbolic","text":"@symbolic x [p]\n\nCreate a symbolic variable and optional symbolic parameter.\n\nExpressions and equations\n\nExpressions created using these variables subclass Function so may be used where functions are expected.\n\nThe  ~ infix operator can be used to create equations, which, by default, are treated as lhs - rhs when called as functions.\n\nExtended help\n\nCalling or substituting into expressions\n\nTo call a symbolic expression regular call notation with positional arguments are used. The first argument maps to any symbolic variable; the second – when given – to any symbolic parameter. It is an error to call an expression with a parameter using just a single argument; for that substitution is needed.\n\nExample\n\nusing SimpleExpressions\n@symbolic x p\nu = x^5 - x - 1\nu(2) # 29 call is u(x)\n\nu.((0,1,2)) # (-1, -1, 29)\n\nu = 2x + p\nu(1)    # errors!\nu(1, 2) # 2(1)+2 or 4\n\nu = sum(x .* p)\nu(2, [1,2]) # 6  call is u(x, p)\n\nCalling with nothing, missing, or : in a slot substitutes in the specified value leaving a symbolic expression, possibly with no variable or parameter.\n\n@symbolic x p\nu = cos(x) - p*x\nu(nothing, 2)  # cos(x) - 2 * x\nu(:, 2)        #  cos(x) - 2 * x, alternate calling form\nu(pi, nothing) # -1.0 - p * π\nv = u(1,:)(:,2)    # (cos(1)-(2*1)),\n\nThe latter can be evaluated using a zero-argument call, e.g. v().\n\nWith substitution in this manner, any symbolic variable and any symbolic parameters will receive the same substituted value.\n\nThe replace generic for symbolic objects takes pairs of values and replaces the left one with the right one working from left to right, leaving a symbolic expression. The replace method treats symbolic variables and symbolic parameters with different symbols as unique.\n\nA symbolic equation, defined through ~, may also be used to specify a left- and right-hand value.\n\nThe main use is as an easier-to-type replacement for anonymous functions, though with differences:\n\n1 |> sin(x) |> x^2  # 0.708… from sin(1)^2\nu = cos(x) - p*x\n2 |> u(:, 3) # -6.4161…, a alternative to u(2,3)\n\nmap(x^2, (1, 2)) # (1,4)\n\nSymbolic expressions an be used with other packages, to simplify some function calls at the expense of being non-idiomatic:\n\nusing Roots\n@symbolic x p\nfind_zero(x^5 - x - 1, 1)       # 1.167…\nfind_zero(x^5 - x ~ p, 1; p=4)  # 1.401…\n\nusing ForwardDiff\nBase.adjoint(𝑓::Function) = x -> ForwardDiff.derivative(𝑓, x)\nu = x^5 - x - 1\nfind_zero((u,u'), 1, Roots.Newton()) # 1.167…\n\nOr\n\nusing Plots\nplot(x^5 - x - 1, 0, 1.5)\n\nOr using both positions, so that we call as a bivariate function:\n\n@symbolic x y\nxs = ys = range(-5, 5, length=100)\ncontour(xs, ys, x^2 - y^2 + 2x*y)\n\nSymbolic derivatives can be taken with respect to the symbolic value, symbolic parameters are treated as constant.\n\n@symbolic x p\nimport SimpleExpressions: D\nu = x^5 - p*x - 1\nD(u)           # (5 * (x ^ 4)) - p\nu = u(:, 1)    # set parameter\na, b = 1, 2\nfind_zeros(D(u) ~ (u(b)-u(a)) / (b-a), (a,b)) # [1.577…]\n\nIdiosyncrasies\n\nUsing this is a convenience for simple cases. It is easy to run into idiosyncrasies.\n\nExpressions are not functions in terms of scope\n\nUnlike functions, expressions are defined with variables at the time of definition, not when called. For example, with a clean environment:\n\n@symbolic x\nu = m*x + b    # errors, `m` not defined\nf(x) = m*x + b # ok\nm, b = 1, 2\nu = m*x + b    # defined using `m` amd `b` at time of assignment\nu(3)           # 1 * 3 + 2\nf(3)           # 1 * 3 + 2 values of `m` and `b` when called\nm, b = 3, 4\nu(3)           # still computing 1 * 3 + 2\nf(3)           # computing 3 * 3 + 4, using values of `m` and `b` when called\n\nSymbolic values are really singletons when calling by position\n\nThough one can make different symbolic variables, the basic call notation by position treats them as the same:\n\n@symbolic x\n@symbolic y    # both x, y are `SymbolicVariable` type\nu = x + 2y\nu(3)           # 9 coming from 3 + 2*(3)\n\nHowever, this is only to simplify the call interface. Using keyword arguments allows evaluation with different values:\n\nu(;x=3, y=2)   # 7\n\nUsing replace, we have:\n\nu(x=>3, y=>2)  # 3 + (2 * 2); evaluate with u(x=>3, y=>2)()\n\nThe underlying CallableExpressions object is directly called in the above manner; that package does not have the narrowed design of this package.\n\nContainers\n\nThe variables may be used as placeholders for containers, e.g.\n\nu = sum(xi*pi for (xi, pi) in zip(x,p))\nu((1,2),(3,4))  # 11\n\nBroadcasting as a function\n\nBroadcasting a function call works as expected\n\n@symbolic x\nu = x^2\nu.((1,2)) # (1, 4)\n\nSymbolic expressions can also be constructed that will broadcast the call\n\nu = x.^2 .+ sin.(p)\nu((1,2),3)\n\nu = @. x^2 + sin(p)\nu((1,2),(3,4))\n\n\n\n\n\n","category":"macro"},{"location":"reference/#SimpleExpressions.@symbolic_variables-Tuple","page":"Reference/API","title":"SimpleExpressions.@symbolic_variables","text":"@symbolic_variables w x[1:3] y() z=>\"𝑧\" Ω::isinteger\n\nDefine multiple symbolic variables or symbolic functions. Guards are ignored.\n\nNot exported.\n\n\n\n\n\n","category":"macro"},{"location":"#SimpleExpressions.jl","page":"Home","title":"SimpleExpressions.jl","text":"","category":"section"},{"location":"","page":"Home","title":"Home","text":"Documentation for SimpleExpressions a very lightweight means to create callable functions using expressions.","category":"page"},{"location":"","page":"Home","title":"Home","text":"This package leverages the CallableExpressions package for the heavy lifting.","category":"page"},{"location":"#Rationale","page":"Home","title":"Rationale","text":"","category":"section"},{"location":"","page":"Home","title":"Home","text":"Julia has easy-to-use \"anonymous\" functions defined through the pattern (args) -> body using ->, notation which mirrors common math notation. However, for students the distinction between an expression, such as defines the \"body\" and a function is sometimes not made, whereas in Julia or other computer languages, the distinction is forced. The SymPy package, as well as other symbolic packages in Julia like Symbolics and SymEngine, allows callable symbolic expressions to be created naturally from symbolic variables. This package does just this (but does not provide the many other compelling features of a CAS). The symbolic expressions subtype Function, so can be used where functions are expected.","category":"page"},{"location":"","page":"Home","title":"Home","text":"The envisioned usage is within resource-constrained environments, such as binder.org.","category":"page"},{"location":"","page":"Home","title":"Home","text":"To keep things as simple as possible, there are only a few types of symbolic values: symbolic numbers, symbolic variables, symbolic parameters, symbolic expressions, and symbolic equations.","category":"page"},{"location":"","page":"Home","title":"Home","text":"Symbolic variables and parameters are created with the @symbolic macro. For the @symbolic macro, the first argument names the symbolic variable, the optional second argument names the symbolic parameter.","category":"page"},{"location":"","page":"Home","title":"Home","text":"Symbolic expressions are built up naturally by using these two types of objects.","category":"page"},{"location":"","page":"Home","title":"Home","text":"Symbolic equations are specified with the infix ~ operator.","category":"page"},{"location":"","page":"Home","title":"Home","text":"Symbolic numbers can be produced from substitution.","category":"page"},{"location":"","page":"Home","title":"Home","text":"The symbolic expressions are just \"thunks\" or delayed expressions (akin to Thunks.jl) but implemented in a more performant manner in CallableExpressions, where the operation and its arguments are kept in a structure and the expression is evaluated when called as a function.","category":"page"},{"location":"#Usage","page":"Home","title":"Usage","text":"","category":"section"},{"location":"","page":"Home","title":"Home","text":"A quick example showing how expressions may be called:","category":"page"},{"location":"","page":"Home","title":"Home","text":"using SimpleExpressions","category":"page"},{"location":"","page":"Home","title":"Home","text":"@symbolic x p\nu = exp(-x) * (sin(3x) + sin(101*x))\nu(2)","category":"page"},{"location":"","page":"Home","title":"Home","text":"This is akin, but different from using a function:","category":"page"},{"location":"","page":"Home","title":"Home","text":"f(x) = exp(-x) * (sin(3x) + sin(101*x))\nf(2)","category":"page"},{"location":"","page":"Home","title":"Home","text":"The main difference being, u can subsequently be algebraically manipulated.","category":"page"},{"location":"","page":"Home","title":"Home","text":"The parameter can also be used to form an expression:","category":"page"},{"location":"","page":"Home","title":"Home","text":"u = cos(x) - p * x\nu(pi/4, 4)","category":"page"},{"location":"","page":"Home","title":"Home","text":"The variable or parameter can be substituted in for:","category":"page"},{"location":"","page":"Home","title":"Home","text":"u(pi/4,:), u(:, 4)","category":"page"},{"location":"","page":"Home","title":"Home","text":"Or, the expression can be evaluated directly","category":"page"},{"location":"","page":"Home","title":"Home","text":"u(pi/4, 4)","category":"page"},{"location":"#Evaluation","page":"Home","title":"Evaluation","text":"","category":"section"},{"location":"","page":"Home","title":"Home","text":"The basic calling pattern for a symbolic expression ex is simple: the first positional argument is for the symbolic value, the second for the symbolic parameter.","category":"page"},{"location":"","page":"Home","title":"Home","text":"Leading to these rules:","category":"page"},{"location":"","page":"Home","title":"Home","text":"ex(x) to evaluate the expression of just the variable with the value of  x; an error is thrown if the expression has both a variable and a parameter.\nex(x, p) to evaluate an expression of both a  variable and a parameter; if there is no parameter the value of the second argument is ignored.\nex(*, p) to evaluate an expression of just a parameter. The * in the x slot can be any valid identifier (except for :, nothing, or missing, as they are used for substitution); the value of the first argument is just ignored.\nex() to evaluate an expression that involves neither a symbolic variable or a parameter.","category":"page"},{"location":"#Substitution","page":"Home","title":"Substitution","text":"","category":"section"},{"location":"","page":"Home","title":"Home","text":"Evaluation leaves a non-symbolic value. For substitution, the result is still symbolic.","category":"page"},{"location":"","page":"Home","title":"Home","text":"The basic syntax for substitution is:","category":"page"},{"location":"","page":"Home","title":"Home","text":"ex(:, p) to substitute in for the parameter.\nex(x, :) to substitute in for the variable.","category":"page"},{"location":"","page":"Home","title":"Home","text":"The use of : to indicate the remaining value is borrowed from Julia's array syntax; it can also be either nothing or missing.","category":"page"},{"location":"","page":"Home","title":"Home","text":"For evaluation and substitution using positional arguments, all instances of symbolic variables and all instances of symbolic parameters are treated identically.","category":"page"},{"location":"","page":"Home","title":"Home","text":"To work with multiple symbolic parameters or variables, replace can be used to substitute in values for a specific variable.","category":"page"},{"location":"","page":"Home","title":"Home","text":"replace(ex, args::Pair...) to substitute in for either a variable, parameter, expression head, or symbolic expression (possibly with a wildcard). The pairs are specified as variable_name => replacement_value.\nex(args::Pair...) redirects to replace(ex, args::Pair...)","category":"page"},{"location":"","page":"Home","title":"Home","text":"To illustrate, two or more variables can be used, as here:","category":"page"},{"location":"","page":"Home","title":"Home","text":"@symbolic x\n@symbolic y  # or SimpleExpressions.@symbolic_variables x y\nu = x^2 - y^2","category":"page"},{"location":"","page":"Home","title":"Home","text":"Evaluating u with a value in the x position will evaluate both x and y with that value:","category":"page"},{"location":"","page":"Home","title":"Home","text":"u(1) # always 0\nu(1,2) # not 1^2 - 2^2, the second argument is ignored here","category":"page"},{"location":"","page":"Home","title":"Home","text":"As indicated, this is a deliberate design limitation to simplify usage. It can be worked around via replace:","category":"page"},{"location":"","page":"Home","title":"Home","text":"v = replace(u, x=>1, y=>2) # the symbolic value ((1^2)-(2^2))\nv()                        # evaluates to -3","category":"page"},{"location":"","page":"Home","title":"Home","text":"The replace method is a bit more involved than illustrated. The key => value pairs have different dispatches depending on the value of the key. Above, the key is a SymbolicVariable, but the key can be:","category":"page"},{"location":"","page":"Home","title":"Home","text":"A SymbolicVariable or SymbolicParameter in which case the simple substitution is applied, as just illustrated.\nA function, like sin. In this case, a matching operation head is replaced by the replacement head. Eg. sin => cos will replace a sin call with a cos call.","category":"page"},{"location":"","page":"Home","title":"Home","text":"v = sin(x) + sin(x^2)\nreplace(v, sin => cos)","category":"page"},{"location":"","page":"Home","title":"Home","text":"A symbolic expression. In this case, the exact match of the expression is replaced by the replacement value.","category":"page"},{"location":"","page":"Home","title":"Home","text":"v = 1 + (x+1)^1 + 2*(x+1)^2 + 3*(x+1)^3\nreplace(v, x+1 => x)","category":"page"},{"location":"","page":"Home","title":"Home","text":"A symbolic expression with a wildcard. Wildcards have a naming convention using trailing underscores. One matches one value; two matches one or more values; three match 0, 1, or more values. In addition, the special symbol ⋯ (entered with \\cdots[tab] is wild.","category":"page"},{"location":"","page":"Home","title":"Home","text":"v = log(1 + x) + log(1 + x^2/2)\n@symbolic x_\nreplace(v, log(1 + x_) => log1p(x_)) # log1p(x) + log1p((x ^ 2) / 2)","category":"page"},{"location":"","page":"Home","title":"Home","text":"Substitution uses match(pattern, subject) for expression matching with wildcards:","category":"page"},{"location":"","page":"Home","title":"Home","text":"subject, pattern = log(1 + x^2/2), log(1+x_)\nms = match(pattern, subject)","category":"page"},{"location":"","page":"Home","title":"Home","text":"The return value is nothing (for no match) or a collection of valid substitutions. Substituting one into the pattern should return the subject:","category":"page"},{"location":"","page":"Home","title":"Home","text":"σ = first(ms)\npattern(σ...)","category":"page"},{"location":"#Symbolic-containers","page":"Home","title":"Symbolic containers","text":"","category":"section"},{"location":"","page":"Home","title":"Home","text":"The values for x or p may be replaced by containers. For example:","category":"page"},{"location":"","page":"Home","title":"Home","text":"@symbolic x a\nu = sum(aᵢ * x^(i-1) for (i,aᵢ) ∈ enumerate(a))\nu(2, (1,2,3,4)) # 49","category":"page"},{"location":"","page":"Home","title":"Home","text":"This is relatively untested and almost certainly not fully featured. For example, only evaluation is allowed, not substitution (using :):","category":"page"},{"location":"","page":"Home","title":"Home","text":"@symbolic x a\nu = sum(ai * x^(i-1) for (i,ai) in enumerate(a))\nu(2, [1,2,3])","category":"page"},{"location":"#Broadcasting","page":"Home","title":"Broadcasting","text":"","category":"section"},{"location":"","page":"Home","title":"Home","text":"The package is intended to support broadcasting of expressions and the construction of broadcasting expressions.","category":"page"},{"location":"","page":"Home","title":"Home","text":"@symbolic x p\nu = x^2 + p\nv = @. x^2 + p\nf(x,p) = x^2 + p\nx0, p0 = (1,2), (3,4)\nu.(x0, p0) == v(x0, p0) == f.(x0, p0) == (1^2+3, 2^2+4)","category":"page"},{"location":"#Equations","page":"Home","title":"Equations","text":"","category":"section"},{"location":"","page":"Home","title":"Home","text":"The package grew out of a desire to have a simpler approach to solving f(x) = g(x). While defining h(x) = f(x) - g(x) and solving h(x) = 0 using, say, Roots is straightforward, it does cause confusion while learning.","category":"page"},{"location":"","page":"Home","title":"Home","text":"Symbolic equations are specified using ~, a notation borrowed from Symbolics for SymPy and now on loan to SimpleExpressions. Of course = is assignment, and == and === are used for comparisons, so some other syntax is necessary and ~ plays the role of distinguishing the left- and right-hand sides of an equation.","category":"page"},{"location":"","page":"Home","title":"Home","text":"By default, when calling a symbolic equation the difference of the left- and right-hand sides is used, so, in this case, symbolic equations can be passed directly to the find_zero method from Roots:","category":"page"},{"location":"","page":"Home","title":"Home","text":"using Roots\n@symbolic x p\nfind_zero(cos(x) ~ sin(x), (0, pi/2)) # use bisection","category":"page"},{"location":"","page":"Home","title":"Home","text":"The solve interface (loaded with Roots) is also available for symbolic equations:","category":"page"},{"location":"","page":"Home","title":"Home","text":"solve(cos(x) ~ p*x, (0, pi/2), p=3)","category":"page"},{"location":"","page":"Home","title":"Home","text":"Linear symbolic equations can be solved symbolically through this package (though the lack of simplification is annoying). Instead of specifying an interval, a variable to solve for is given.","category":"page"},{"location":"","page":"Home","title":"Home","text":"@symbolic a A\n@symbolic b B\nsolve(sin(A)/a ~ sin(B)/b, A)  # solve not exported, but is imported with Roots above","category":"page"},{"location":"","page":"Home","title":"Home","text":"This example shows \"inverse\" functions are applied (without concern for domain/range restrictions) when possible.","category":"page"},{"location":"#Plotting","page":"Home","title":"Plotting","text":"","category":"section"},{"location":"","page":"Home","title":"Home","text":"For plotting a symbolic equation, eq, the values eq.lhs and eq.rhs may be used separately to produce a pair of traces. With Plots, where a vector of functions may be plotted, plot([eq...], a, b) will plot each side with separate trace. Though with Plots there is a recipe to plot a symbolic equation as two separate functions.","category":"page"},{"location":"#Derivatives","page":"Home","title":"Derivatives","text":"","category":"section"},{"location":"","page":"Home","title":"Home","text":"Symbolic expressions can be easily differentiated, though the operator is not exported. A variable to differentiate by should be specified, though when missing it is assumed there is a lone symbolic variable to differentiate by. The operator differentiates with respect to the variable assuming it represents a scalar quantity:","category":"page"},{"location":"","page":"Home","title":"Home","text":"import SimpleExpressions: D\n@symbolic x p\nD(cos(x) - x * p)  # uses x","category":"page"},{"location":"","page":"Home","title":"Home","text":"D(cos(x) ~ x * p, p)","category":"page"},{"location":"","page":"Home","title":"Home","text":"Here the derivative is used to take a step of Newton's method::","category":"page"},{"location":"","page":"Home","title":"Home","text":"u = x^5 - x - 1\ndu = D(u, x)\nx0 = 2\nx0 - u(x0) / du(x0)","category":"page"},{"location":"","page":"Home","title":"Home","text":"Here the application of the product rule can be seen:","category":"page"},{"location":"","page":"Home","title":"Home","text":"u = D(exp(x) * (sin(3x) + sin(101x)), x)","category":"page"},{"location":"#Simplification","page":"Home","title":"Simplification","text":"","category":"section"},{"location":"","page":"Home","title":"Home","text":"No simplification is done so the expressions can quickly become unwieldy. There is TermInterface support, so–in theory–rewriting of expressions, as is possible with the Metatheory.jl package, is supported. The scaffolding is in place, but waits for the development version to be tagged.","category":"page"}]
}
