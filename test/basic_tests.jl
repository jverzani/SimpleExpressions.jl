# basics
import SimpleExpressions: arguments, sorted_arguments
import SimpleExpressions: D, solve, ispolynomial, polynomial_coefficients, polynomial_degree
import SimpleExpressions: map_matched
import SimpleExpressions: is_number, isconstant, isvariable
@testset "SimpleExpressions.jl" begin

    @symbolic x p

    # basic arithmetic
    x₀ = 3
    @test (x + 2)(x₀)  == x₀ + 2
    @test (-x)(x₀)  == -x₀
    @test (x * 2)(x₀)  == x₀ * 2
    @test (x / 2)(x₀) == x₀ / 2
    @test (x // 2)(x₀) == x₀ // 2
    @test (x ^ 2)(x₀)  == x₀ ^ 2

    # in a pipeline
    @test 1 |> x^2 |> x - 3 == 1^2 - 3

    # simple map
    @test map(x^2, (1,2)) == (1^2, 2^2)

    # using a parameter
    u = cos(x) - p*x
    @test u(1,2) == cos(1) - 2*1
    x₀ = rand()
    @test (u(nothing,2)(x₀) == u(:,2)(x₀) == cos(x₀) - 2*x₀) # replace parameter

    # function algebra
    f = sin(x)
    g = cos(x)
    @test (f∘g)(x₀) == f(g(x₀)) == sin(cos(x₀))
    for op ∈ (+, -, *, /, ^)
        @test op(f,g)(x₀) ≈ op(f(x₀), g(x₀)) atol = eps(x₀)
    end

    # +,* nary
    @test length(arguments(x + 2x + 6sin(x))) == 2
    @test length(arguments(x * 2x * 6sin(x))) == 3

    # sort
    @test sorted_arguments(6*sin(x)*x*p*2) == (12,p,x,sin(x))

    # isless + isequal: exactly one of those three yields true.
    xs = (2, 3, x, 2x, p, 2p, x^2,sin(x),x+x^2+x^3)
    for a in xs
        for b in xs
            @test sum(isless(a,b) + isequal(a,b) + isless(b,a)) == 1
        end
    end

    # inv
    @test inv(inv(x)) == x
    @test inv(x^p) == x^(-p)
    x₀ = rand() # integers special cased, so harder to test
    @test inv(x^2)(x₀) ≈ (x^(-2))(x₀)
end

@testset "predicates" begin
    a,b,c = SimpleExpressions.SymbolicNumber.((1,2,3))
    @test isinteger(a)
    @test isinteger(a + b + c)
    @test ispow2(b)
    @test !ispow2(c)
    @test !iszero(a)
    @test iszero(a-a)
    @test isone(a)
    @test isone(b-a)
    @test !isone(b)
    @test iseven(b)
    @test iseven(a + c)
    @test !iseven(a)
    @test isodd(a)
    @test isodd(a + b)
    @test !isodd(b)
    @test isfinite(a)
    @test !isfinite(a*Inf)
    @test !isinf(a)
    @test isinf(a*Inf)
    @test isnan(a*Inf - a*Inf)
    @test !isnan(a)

    @symbolic x p
    u = (x + sqrt(x))(2,:) # symbolic number
    @test is_number(2)
    @test is_number(u)
    @test !is_number(p)
    @test !is_number(x)

    @test isnumeric(u)
    @test !isnumeric(p)
    @test !isnumeric(x)

    @test isconstant(2)
    @test isconstant(u)
    @test isconstant(p)
    @test !isconstant(x)

    @test !isvariable(2)
    @test !isvariable(u)
    @test isvariable(p)
    @test isvariable(x)
    @test !isvariable(sin(x))

end

@testset "evaluation/substitution" begin
    @symbolic x p
    f = (x,p)  -> cos(x) - x*p
    uxp = f(x, p)
    x₀,p₀ = 1, 2

    @test uxp(x₀,p₀)         == f(x₀, p₀)
    @test_throws "type NamedTuple has no field" uxp(x₀)          # no p
    @test uxp(x₀, :)(:,p₀)() == f(x₀, p₀)
    @test uxp(:,p₀)(x₀)      == f(x₀, p₀)
    @test uxp(:,p₀)(x₀,:)()  == f(x₀, p₀)

    @test uxp(;x=x₀, p=p₀) == f(x₀, p₀)   # direct to CallableExpressions

    f = (x,p) -> x^2
    ux_ = f(x,p)
    @test ux_(x₀)            == f(x₀, p₀)
    @test ux_(x₀,p₀)         == f(x₀, p₀)
    @test ux_(:,p₀)(x₀)      == f(x₀, p₀)
    @test ux_(:,p₀)(x₀,:)()  == f(x₀, p₀)

    @test ux_(;x=x₀, p=p₀)   == f(x₀, p₀) # direct to CallableExpressions

    f = (x,p) -> p^2
    u_p = f(x,p)
    @test u_p(:, p₀)()       == f(x₀, p₀)
    @test u_p(x₀,p₀)         == f(x₀, p₀)
    @test_throws "type NamedTuple has no field" u_p(x₀)          # no p
    @test u_p(x₀, :)(:,p₀)() == f(x₀, p₀)

    @test u_p(;x=x₀, p=p₀)   == f(x₀, p₀) # direct to CallableExpressions


    u__ = replace(u_p, p=>p₀)
    @test is_number(u__)
    @test u__()       == f(x₀,p₀)
    @test u__(x₀)     == f(x₀,p₀)
    @test u__(x₀, p₀) == f(x₀,p₀) # u is numeric, but can be called these ways

    u = cos(x)*sin(p*x)
    # : = nothing = missing
    import SimpleExpressions: ↓
    @test ↓(u(:,p₀)) == ↓(u(missing, p₀)) == ↓(u(nothing, p₀))
    @test ↓(u(x₀,:)) == ↓(u(x₀, missing)) == ↓(u(x₀, nothing))

    # Number or symbolic output
    @test u(x₀, p₀)                isa Number
    @test u(:, p₀)(x₀)             isa Number
    @test u(x₀, :)(*, p₀)          isa Number
    @test u(:, p₀)                 isa SimpleExpressions.AbstractSymbolic
    @test u(x₀, :)                 isa SimpleExpressions.AbstractSymbolic
    @test replace(u, x=>x₀, p=>p₀) isa SimpleExpressions.AbstractSymbolic
    @test u(x=>x₀, p=>p₀)          isa SimpleExpressions.AbstractSymbolic
    @test u(x=>x₀)                 isa SimpleExpressions.AbstractSymbolic
    @test u(p=>p₀)                 isa SimpleExpressions.AbstractSymbolic

    # unwrap_const
    @test SimpleExpressions.unwrap_const(x) == x
    @test x(2,:) !== SimpleExpressions.unwrap_const(x(2,:)) === 2
end

@testset "map_matched" begin
    # map_matched
    @symbolic x p
    @test SimpleExpressions.map_matched(x*tanh(exp(x)), ==(exp(x)), x -> x^2) == x * tanh(exp(x)^2)


    # occursin/contains
    @test occursin(x, sin(cos(x)))
    @test contains(sin(cos(x)), x)

    # contains operation
    @test SimpleExpressions.contains_operation(x*tanh(exp(x)), exp)
    @test !SimpleExpressions.contains_operation(x*tanh(exp(x)), sin)
end



@testset "show" begin
    # test show
    # note *,+ do **not** do light simplification and sort arguments
    # though they had
    @symbolic x p

    @test repr(2x) == "2 * x"
    @test repr(x*2) == "2 * x"

    @test repr(x / 2) == "x / 2"
    @test repr((x+2) / 2) == "1 + (x / 2)"
    @test repr(x / (x+2)) == "x / (2 + x)"
    @test repr(x .- sum(x)/length(x)) == "x .- (sum(identity, x) / length(x))"

    @test repr((1+x)^2) == "(1 + x) ^ 2"

    # non-atomic numbers are wrapped so the output parses back correctly
    @test repr(x^(1//2)) == "x ^ (1//2)"
    @test repr(x^(-1//2)) == "1 / (x ^ (1//2))"
    @test repr(x^(1+2im)) == "x ^ (1 + 2im)"
    @test repr((-2)^x) == "(-2) ^ x"
    @test repr((1//2)^x) == "(1//2) ^ x"
    @test repr(x + 1//2) == "1//2 + x"
    @test repr((1+2im)*x) == "(1 + 2im) * x"
    for ex in (x^(1//2), x^(-1//2), (-2)^x, (2//3)*x^(1//2), x^(2//3)*p^(-1//2), x - (1+2im))
        ex′ = eval(:(let x = $x, p = $p; $(Meta.parse(repr(ex))) end))
        @test ex′(2, 3) ≈ ex(2, 3)
    end

end

@testset "combine" begin
    # simplish simplification
    # uses term by default, so these are noops now and test use
    # of add and mul terms
    combine = identity
    @symbolic x p

    ex = 2x + x
    @test combine(ex) == 3x
    @test combine(ex + 2x) == 5x

    ex = x * x^2 * x^3
    @test combine(ex) == x^6

    ex = x * cos(x) * x^2 * x^3
    u = combine(ex)
    @test u ∈ (x^6 * cos(x), cos(x)*x^6)

    ex = x + 2x + x*x*x
    u = combine(ex)
    @test u ∈ (3x + x^3, x^3 + 3x)

    ex = sin(x + 2x) + sin(3x)
    u = combine(ex)
    @test u == 2 * sin(3x)

    ex = 2x + p*x + (2+p)*x
    u = combine(ex)
    @test u == (4 + 2p)*x

    ex = (x + 2x) / 3x
    u = combine(ex)
    @test u == 1

    ex = (2x) / (3x)
    u = combine(ex)
    @test u == 2//3

    ex = (2x) * ((3x)^(-1))
    u = combine(ex)
    @test u == 2//3

    ex = sin(x+x) / cos(x + 2x)
    u = combine(ex)
    @test u == sin(2x)/cos(3x)

    ex = sum(n + n*x + n^2*x^2 for n in 1:5)
    u = combine(ex)
    @test polynomial_coefficients(u, x) == (a₀ = 15, a₁ = 15, a₂ = 55)

    ex = sum(n + n*x + (n*x)^2 for n in 1:5)
    u = combine(ex)
    @test polynomial_coefficients(u, x) == (a₀ = 15, a₁ = 15, a₂ = 55)

end

@testset "polynomial" begin
    @symbolic x p

    @test ispolynomial(2x + 3x^4 + 5x^6, x)
    @test ispolynomial(p*x + p^2 * x^2 + sin(p)*x^3, x)
    @test !ispolynomial(1/x + 1/x^2, x)

    @test values(polynomial_coefficients(x^5 - x - 1, x)) == (-1,-1,0,0,0,1)

    a,b...,c = polynomial_coefficients(x^20 - 1,x)
    @test isone(-a) && isone(c)
    @test all(iszero, b)

    @test polynomial_degree(1 + x + x^5, x) == 5
    @test polynomial_degree((1+x)^5, x) == 5
    @test polynomial_degree((1+p*x)^5, x) == 5
    @test polynomial_degree((1+sin(p)*x)^5, x) == 5
    @test polynomial_degree((1+p*sin(x))^5, x) == nothing

end



@testset "broadcast/generators" begin
    @symbolic x p

    x₀ = [1,2,3]
    ## Need to address Base.broadcasted method
    @test x₀ |> (x .- sum(x)/length(x)) |> x .* x  == (x₀ .- sum(x₀)/length(x₀)).^2

    @test x₀ .|> x^2 == x₀.^2
    @test x₀ |> x.^2 == x₀.^2
    @test x₀ |> x.^2.0 == x₀.^2.0

    @test_throws MethodError x₀ |> x^2

    # basic generators
    x₀, p₀ = 2, (1,2,3)

    # eachindex
    u = sum(x[i] for i ∈ eachindex(x))
    @test u(x₀) == sum(x₀[i] for i ∈ eachindex(x₀))

    x₀′ = (1,2)
    u = sum(p[i] for i ∈ eachindex(x))
    @test u(x₀′, p₀) == sum(p₀[i] for i in eachindex(x₀′))



    # enumerate
    u = sum(pᵢ*x^i for (i,pᵢ) ∈ enumerate(p))
    @test u(x₀, p₀) == sum(pᵢ*x₀^i for (i, pᵢ) ∈ enumerate(p₀))

    # zip
    x₀ = (3,2,3)
    u = prod(xᵢ*pᵢ for (xᵢ, pᵢ) ∈ zip(x, p))
    @test u(x₀, p₀) == prod(xᵢ*pᵢ for (xᵢ, pᵢ) ∈ zip(x₀, p₀))

    # make new symbolic expressions
    u = @symbolic_expression foldl(=>, @symbolic_expression(1:x))
    @test u(4) == (((1 => 2) => 3) => 4)


    # convert Expr type
    u = sin(x) * (cos(x) - x^2)
    ex = convert(Expr, u)

    ## goal with broadcasting
    ## we want to be able to broadcast function calls
    @symbolic x p
    u = x + p
    @test u.([1,2],3) == [4,5]
    @test u.([1,2], [3,4]) == [1+3, 2+4]
    @test u.([1,2],[3 4]) == [1+3 2+3; 1+4 2+4]

    ## we want to be able to create symbolic expressions that will broadcast arguments
    u = x.^2  # literal_pow
    v = x^2
    @test u((1,2)) == v.((1,2)) == (1,2) .^ 2

    u = sin.(x)
    v = sin(x)
    @test u((1,2)) == v.((1,2)) == sin.((1,2))

    u = x .^ 2.0
    v = x ^ 2.0
    @test u((1,2)) == v.((1,2)) == (1,2) .^ 2

    u = x .+ p
    v = x + p
    @test u((1,2), (3,4)) == v.((1,2), (3,4)) == (1+3, 2+4)

    # u  maps like f
    @symbolic x
    u = x^2
    f(x) = x^2
    xs = [1,2]
    @test map(u, xs)    == map(f, xs)
    @test map.(u, xs)   == map(f, xs)
    @test map.([u], xs) == map.([f], xs)
end

@testset "derivatives" begin

    ∂(u,x) = (h = 1e-6; (u(x+h)-u(x))/h)

    @symbolic x
    x₀ = 2

    ex = cos(x)*sin(x^2+x)
    @test D(ex, x)(x₀) ≈ ∂(ex, x₀) atol=1e-4
    @test diff(ex, x)(x₀) ≈ ∂(ex, x₀) atol=1e-4

    ex = exp(x^2 - 2) * log(x + sin(x))
    @test D(ex,x)(x₀) ≈ ∂(ex, x₀) atol=1e-4
    @test diff(ex, x)(x₀) ≈ ∂(ex, x₀) atol=1e-4

    ex = log1p(x^2) * sqrt(1 + sin(x)^2)
    @test D(ex,x)(x₀) ≈ ∂(ex, x₀) atol=1e-4
    @test diff(ex, x)(x₀) ≈ ∂(ex, x₀) atol=1e-4

    ex = (x^2 + 1) / (x^2 - x)
    @test D(ex,x)(x₀) ≈ ∂(ex, x₀) atol=1e-4
    @test diff(ex, x)(x₀) ≈ ∂(ex, x₀) atol=1e-4

    ex = abs(inv(x))
    @test D(ex,x)(x₀) ≈ ∂(ex, x₀) atol=1e-4
    @test diff(ex, x)(x₀) ≈ ∂(ex, x₀) atol=1e-4

    ex = log(x)
    u = D(D(ex,x),x) - D(1/x,x)
    @test u(x₀) == 0
    @test isnan(u(-x₀)) # Fix indicator ones

    # sum rule (vararg)
    ex = sin(x) + cos(x) + tan(x)
    @test D(ex, x)(x₀) ≈ ∂(ex, x₀) atol=1e-4
        @test diff(ex, x)(x₀) ≈ ∂(ex, x₀) atol=1e-4

    # different variables
    @symbolic w p
    ex = cos(w) - p*w
    @test D(ex, w) == D(ex) # finds w from expression
    @test D(ex, p) == -w

    # diff
    @test_throws MethodError diff(x) # must specify variable
    @test diff(sin(x),x) == cos(x)
    @test diff(sin(x),x, x) == -sin(x)
    @test diff(sin(x),x,x,x,x) == sin(x)

end

@testset "solve" begin
    # solve is really rudimentary
    # and experimental to see if it has any value
    @symbolic a A
    @symbolic b B

    eq = solve(sin(A)/a ~ sin(B)/b, b)
    @test eq.lhs == b
    @test !contains(eq.rhs, b)

    eq = solve(a*A + b ~ B, A)
    @test eq.lhs == A
    @test !contains(eq.rhs, A)

    constraint = B ~ 2a + 2b
    A = a * b
    u = solve(constraint, b)
    A = A(u) # use equation in replacement
    v = solve(D(A, a) ~ 0, a) # lack of simplification masks answer
    l,r = v
    @test l == a # solved
    @test !contains(r, a)
    @test contains(r, B) # parameterized still

    @symbolic x p
    # linear, either side
    @test solve(2x + 3 ~ 7, x).rhs == 2
    @test Tuple(solve(5 ~ x - p, x)) == Tuple(solve(x - p ~ 5, x))
    @test solve(p ~ 2x + 2a, x).lhs == x
    # powers and quadratics return tuples of roots
    @test map(last, solve(x^2 ~ 4, x)) == (-2, 2)
    @test map(last, solve(x^2 - 5x + 6 ~ 0, x)) == (2, 3)
    @test map(last, solve((x+1)^2 ~ 4, x)) == (-3, 1)
    @test solve(x^3 ~ 8, x).rhs() ≈ 2
    s1, s2 = solve(a*x^2 + b*x ~ p, x)
    @test s1.lhs == x && !contains(s1.rhs, x)
    # no real solutions
    @test solve(x^2 + 1 ~ 0, x) == ()
    @test solve(exp(x) ~ -1, x) == ()
    @test solve(sqrt(x) ~ -1, x) == ()
    # inverse functions (principal values)
    @test solve(exp(2x) ~ p, x).rhs(p => 2)() ≈ log(2)/2
    @test solve(2^x ~ p, x).rhs(p => 8)() ≈ 3
    @test solve(sqrt(x + 1) ~ p, x).rhs(p => 3)() ≈ 8
    # no x, or x cancels
    @test isnothing(solve(a ~ b, x))
    @test isnothing(solve(x ~ x, x))
    # can't isolate
    @test solve(x * exp(x) ~ 1, x).lhs == x # Lambert W

    # zero-product rule
    @test map(last, solve((x-1)*(x-2)*(x-3) ~ 0, x)) == (1, 2, 3)
    @test solve(x * sin(x) ~ 0, x).rhs == 0
    @test map(last, solve(exp(x)*(x^2 - 4) ~ 0, x)) == (-2, 2)
    @test map(last, solve((x-1)^2*(x+2) ~ 0, x)) == (-2, 1)
    # two branches
    @test map(last, solve(abs(x - 1) ~ 2, x)) == (-1, 3)
    @test solve(abs(x) ~ -1, x) == ()
    @test solve(cosh(x) ~ 1//2, x) == ()
    s1, s2 = solve(cosh(x) ~ 2, x)
    @test s1.rhs() ≈ -acosh(2) && s2.rhs() ≈ acosh(2)
    @test solve(x^(2//3) ~ 4, x).rhs == 8 # (-8.0)^(2//3) is a DomainError
    # higher degree polynomials
    @test map(last, solve(x^3 - 2x^2 ~ 0, x)) == (0, 2)
    @test map(last, solve(x^3 - 6x^2 + 11x - 6 ~ 0, x)) == (1, 2, 3)
    @test map(last, solve(2x^3 - 3x^2 + 1 ~ 0, x)) == (-1//2, 1)
    @test map(last, solve(x^4 - 5x^2 + 4 ~ 0, x)) == (-2, -1, 1, 2)
    @test map(last, solve(x^6 - 9x^3 + 8 ~ 0, x)) == (1, 2)
    @test solve(x^4 + 1 ~ 0, x) == ()
    @test length(solve(x^3 - x^2 - 2x + 2 ~ 0, x)) == 3 # 1, ±√2
    @test solve(x^3 + x ~ 1, x).lhs == x # no rational roots; Cardano
    # exact values
    @test map(last, solve(x^2 ~ 4//9, x)) == (-2//3, 2//3)
    @test solve(x^3 ~ -8, x).rhs() === -2
    @test solve(2x + 3 ~ 7, x).rhs() === 2

    # same function on both sides
    @test solve(2^x ~ 2^(3x - 4), x).rhs == 2
    @test solve(exp(x) ~ exp(2x + 1), x).rhs == -1
    @test solve(log(x) ~ log(2x - 3), x).rhs == 3
    @test solve(sqrt(x + 5) ~ sqrt(2x), x).rhs == 5
    @test solve(sqrt(x) ~ sqrt(p), x).rhs == p
    @test solve(2^x ~ 3^(x + 1), x).rhs() ≈ log(3) / (log(2) - log(3))
    @test solve(exp(x) ~ 2exp(-x), x).rhs() ≈ log(2) / 2
    # sums of logs; extraneous roots dropped
    @test solve(log(x) + log(x - 1) ~ log(6), x).rhs == 3
    @test solve(2log(x) ~ log(4), x).rhs() ≈ 2
    # rational functions; roots of the denominator dropped
    @test solve((x^2 - 1)/(x - 1) ~ 0, x).rhs == -1
    @test solve(x/(x - 2) ~ 2/(x - 2), x) == ()
    @test solve(1/x ~ 0, x) == ()
    @test all(s -> s.rhs()^2 - s.rhs() - 1 ≈ 0, solve(1/x + 1/(x + 1) ~ 1, x))
    u = solve(x/(x + 1) ~ p, x)
    @test u.lhs == x && !contains(u.rhs, x)
    # substitution
    @test all(≈(0, atol=1e-12), map(s -> exp(2s.rhs()) - 3exp(s.rhs()) + 2,
                                     solve(exp(2x) - 3exp(x) + 2 ~ 0, x)))
    @test solve(sqrt(x) + x ~ 6, x).rhs() ≈ 4
    @test solve(sqrt(x) ~ x - 2, x).rhs() ≈ 4   # x = 1 is extraneous
    @test map(s -> s.rhs(), solve(cbrt(x)^2 - cbrt(x) ~ 2, x)) == (-1, 8)
    @test length(solve(log(x)^2 - log(x) ~ 2, x)) == 2

    # exact (symbolic) values rather than floats
    S = SimpleExpressions.SymbolicNumber
    @test map(last, solve(x^2 ~ 2, x)) == (-sqrt(S(2)), sqrt(S(2)))
    @test map(last, solve(x^2 - 2x - 1 ~ 0, x)) == (1 - sqrt(S(2)), 1 + sqrt(S(2)))
    @test map(last, solve(x^2 ~ 8, x)) == (-2sqrt(S(2)), 2sqrt(S(2)))
    @test solve(x^3 ~ 16, x).rhs == 2cbrt(S(2))
    @test solve(sin(x) ~ 1//2, x).rhs == S(π) / 6
    @test solve(cos(2x) ~ 0, x).rhs == S(π) / 4
    @test solve(tan(x) ~ -1, x).rhs == -S(π) / 4
    @test solve(acos(x) ~ 0, x).rhs == 1
    @test solve(exp(x) ~ 2, x).rhs == log(S(2))
    @test solve(2^x ~ 8, x).rhs == 3
    @test solve(2^x ~ 1//4, x).rhs == -2
    @test map(last, solve(exp(2x) - 3exp(x) + 2 ~ 0, x)) == (0, log(S(2)))
    @test map(last, solve(sin(x)^2 - sin(x) ~ 0, x)) == (0, S(π) / 2)
    @test solve(x^2 ~ 0, x).rhs == 0
    # floats in, floats out
    @test solve(x^2 ~ 2.0, x)[2].rhs() ≈ sqrt(2)

    # logarithms in other bases
    @test map(last, solve(log(4, x^2 - 2x) ~ log(4, 5x - 12), x)) == (3, 4)
    @test map(last, solve(log(3, 25 - x^2) ~ 2, x)) == (-4, 4)
    @test solve(log(2, x + 1) - log(2, 2 - x) ~ 3, x).rhs == 5//3
    @test solve(log10(x) + log10(x - 21) ~ 2, x).rhs == 25
    @test solve(log(4, -x) + log(4, 6 - x) ~ 2, x).rhs == -2
    # radicals: isolate and square, extraneous roots dropped
    @test solve(2x ~ sqrt(x + 3), x).rhs == 1
    @test solve(7 ~ sqrt(39 + 3x) - x, x).rhs == -1
    @test map(last, solve(x ~ 1 + sqrt(2x - 2), x)) == (1, 3)
    @test solve(1 + sqrt(1 - x) ~ sqrt(2x + 4), x).rhs == 0
    # abs split into cases
    @test solve(6x ~ abs(1 + 3x), x).rhs == 1//3
    @test map(last, solve(abs(2x - 3) ~ 4 - x, x)) == (-1, 7//3)
    @test map(last, solve(abs(x/2 + 4) ~ abs(4x - 6), x)) == (4//9, 20//7)
    # common polynomial factor
    s1, s2 = solve(2x - x*exp(6x - 1) ~ 0, x)
    @test s1.rhs == 0 && s2.rhs() ≈ (1 + log(2))/6
    @test length(solve(4x + 1 ~ (12x + 3)*exp(x^2 - 2), x)) == 3
    # exponentials of one sign; a⋅cos + b⋅sin
    @test solve(14exp(6 - x) + exp(12x - 7) ~ 0, x) == ()
    @test solve(9cos(4x/9) + 21sin(4x/9) ~ 0, x).rhs() ≈ 9atan(-3/7)/4
    # x^(1//3) is undefined for negative x in Julia; use cbrt
    @test solve(4x^(2//3) + 21x^(1//3) + 27 ~ 0, x) == ()
    @test map(last, solve(4cbrt(x)^2 + 21cbrt(x) + 27 ~ 0, x)) == (-27, -729//64)

    # parameters: never-valid solutions dropped; conditions on request
    @test solve(log(x) + log(x + 1) ~ p, x).lhs == x # only one root
    @test solve(sqrt(x - 1) ~ p - x, x).lhs == x
    @test solve((x^2 - p^2)/(x - p) ~ 0, x).rhs == -p
    @test solve(log(x) ~ log(p), x).rhs == p
    eq, cs = solve(sqrt(x) ~ p, x; conditions=true)
    @test eq.rhs == p^2 && length(cs) == 1 && string(only(cs)) == "p >= 0"
    (e1, c1), (e2, c2) = solve(abs(x - p) ~ 2x, x; conditions=true)
    @test string.((c1..., c2...)) == ("p <= 0", "p >= 0")
    @test string(only(last(solve(log(x - p) ~ log(2x), x; conditions=true)))) == "p < 0"
    @test string(only(last(solve(x/(x + 1) ~ p, x; conditions=true)))) == "p != 1"
    @test string(only(last(solve(sqrt(x - 1) ~ p - x, x; conditions=true)))) == "p >= 3//4"
    @test last(solve(x^2 - 5x + 6 ~ 0, x; conditions=true)[1]) == ()
    # expanded discriminant
    @test contains(string(solve(sqrt(x - 1) ~ p - x, x).rhs), "sqrt(-3 + (4 * p))")

    # no infinite recursion when g(x) can't be isolated
    @test solve(x^x ~ 4, x).rhs == 2 # Lambert W
    @test solve(floor(x) ~ 2, x).lhs != x
    # a branch where x cancels is dropped
    @test map(last, solve(abs(x) + abs(x - 2) ~ 4, x)) == (-1, 3)
    # exp(log(2)) = 2
    @test solve(log(x + 1) - log(x) ~ log(S(2)), x).rhs == 1
    # powers of a common base
    @test map(last, solve(4^x - 3*2^x + 2 ~ 0, x)) == (0, 1)
    @test map(last, solve(9^x - 4*3^x + 3 ~ 0, x)) == (0, 1)
    # a⋅cos + b⋅sin ~ c
    @test solve(3cos(x) + 4sin(x) ~ 5, x).rhs == atan(S(4//3))
    v = solve(cos(x) + sin(x) ~ 1, x).rhs() # π/2; principal branch
    @test cos(v) + sin(v) ≈ 1
    # cube roots
    @test solve(cbrt(x + 6) ~ x, x).rhs == 2
    @test solve(cbrt(x) ~ x - 6, x).rhs == 8
    # radical in a denominator
    @test solve(x/sqrt(x^2 + 1) ~ 1//2, x).rhs == sqrt(S(3))/3
    # three square roots
    @test solve(sqrt(x + 1) + sqrt(x + 4) + sqrt(x + 9) ~ 6, x).rhs == 0
    @test map(last, solve(sqrt(2x + 1) - sqrt(x) ~ 1, x)) == (0, 4)

    # cubics and quartics without rational roots
    resid(eq, sols) = maximum(s -> abs(eq(s.rhs())), sols isa Tuple ? sols : (sols,))
    for eq ∈ (x^3 + x ~ 1, x^3 - 3x + 1 ~ 0, 2x^3 + 3x^2 - x + 5 ~ 0,
              x^4 + x - 2 ~ 0, x^4 - 10x^2 + 1 ~ 0, x^4 + 4x - 1 ~ 0, x^4 + 1 ~ 3x)
        sols = solve(eq, x)
        @test all(s -> isequal(s.lhs, x), sols isa Tuple ? sols : (sols,))
        @test resid(eq, sols) < 1e-12
    end
    @test length(solve(x^3 - 3x + 1 ~ 0, x)) == 3        # three real roots, trig form
    @test solve(x^3 - 2 ~ 0, x).rhs == cbrt(S(2))
    @test map(last, solve(x^3 - 6x - 4 ~ 0, x)) == (1 - sqrt(S(3)), -2, 1 + sqrt(S(3)))[[2, 1, 3]]
    @test length(solve(x^4 - 10x^2 + 1 ~ 0, x)) == 4

    # trig identities: rewritten as polynomials in sin(u), cos(u)
    π′ = S(π)
    @test map(last, solve(sin(2x) ~ cos(x), x)) == (π′/6, π′/2)
    @test map(last, solve(sin(x)^2 + cos(x) ~ 1, x)) == (0, π′/2)
    @test map(last, solve(cos(2x) + cos(x) ~ 0, x)) == (π′/3, π′)
    @test map(last, solve(2cos(x)^2 + 3sin(x) ~ 3, x)) == (π′/6, π′/2)
    @test map(last, solve(tan(x) ~ 2sin(x), x)) == (0, π′/3)
    @test map(last, solve(cos(2x) ~ sin(x), x)) == (-π′/2, π′/6)
    @test map(last, solve(sec(x)^2 - 2tan(x) ~ 0, x)) == (-3π′/4, π′/4)
    @test map(last, solve(sin(3x) ~ sin(x), x)) == (-π′/4, 0, π′/4)
    @test map(last, solve(sin(4x) ~ cos(2x), x)) == (π′/12, π′/4)
    @test map(last, solve(sin(x)*cos(x) ~ 1//4, x)) == (-11π′/12, -7π′/12, π′/12, 5π′/12)
    @test solve(3sin(x) + 2cos(x)^2 ~ 0, x).rhs == -π′/6
    eq = cos(x) - sin(x) ~ sqrt(S(2))*sin(x)*cos(x)
    @test all(s -> abs(eq(s.rhs())) < 1e-12, solve(eq, x))

    # all solutions in a closed interval
    @test map(last, solve(4sin(3x) ~ 2, x; interval = (0, 4π′/3))) ==
        (π′/18, 5π′/18, 13π′/18, 17π′/18)
    @test map(last, solve(sin(2x) ~ cos(x), x; interval = (0, 2π′))) ==
        (π′/6, π′/2, 5π′/6, 3π′/2)
    @test map(last, solve(sin(x) ~ 0, x; interval = (0, 2π′))) == (0, π′, 2π′) # closed
    @test map(last, solve(tan(x) ~ 1, x; interval = (0, 2π′))) == (π′/4, 5π′/4)
    @test map(last, solve(sind(x) ~ 1//2, x; interval = (0, 360))) == (30, 150)
    @test solve(cos(x) ~ 2, x; interval = (0, 10)) == ()
    @test length(solve(sin(x^2) ~ 1//2, x; interval = (0, 4))) == 6
    @test solve(x^2 - 5x + 6 ~ 0, x; interval = (0, 2.5)).rhs == 2 # filters
    @test solve(sin(x) ~ 1, x; interval = (0, 1)) == ()
    @test last(solve(sin(x) ~ 1//2, x; interval = (0, π′), conditions = true)[1]) == ()
    @test_throws ArgumentError solve(sin(x) ~ p, x; interval = (0, 1))
    @test_throws ArgumentError solve(sin(x) ~ 1//2, x; interval = (1, 0))

    # Lambert W
    W = SimpleExpressions.lambertw
    @test solve(x*exp(x) ~ 1, x).rhs == W(S(1))
    @test solve(exp(-x) ~ x, x).rhs == W(S(1))
    @test map(last, solve(x*exp(x) ~ -1//4, x)) == (W(S(-1//4), -1), W(S(-1//4)))
    @test solve(x*exp(x) ~ -1, x) == () # below -1/e
    @test solve(x^x ~ 4, x).rhs == 2
    @test solve(x^x ~ 27, x).rhs == 3
    @test map(last, solve(2^x ~ x + 1, x)) == (0, 1)
    @test solve(x + exp(x) ~ 1, x).rhs == 0
    @test solve(x + exp(x) ~ 2, x).rhs == 2 - W(exp(S(2)))
    @test solve(x + log(x) ~ 1, x).rhs == 1
    @test solve(x*log(x) ~ 2log(S(2)), x).rhs == 2
    @test solve(x*2^x ~ 8, x).rhs == 2
    @test solve((x + 1)*exp(x) ~ 1, x).rhs == 0
    for eq ∈ (x*log(x) ~ 1, 3^x ~ 3x, x - log10(x) ~ 2, 4x - exp(x) ~ 0, x^x ~ 0.8, x*exp(x) ~ 2.5)
        sols = solve(eq, x)
        @test all(s -> abs(eq(s.rhs())) < 1e-12, sols isa Tuple ? sols : (sols,))
    end
    (e₀, c₀), (e₁, c₁) = solve(x*exp(x) ~ p, x; conditions = true)
    @test e₀.rhs == W(p) && length(c₀) == 1 && length(c₁) == 2
    @test string(W(x)) == "lambertw(x)" && string(W(x, -1)) == "lambertw(x, -1)"
    @test W(x)(1.0) ≈ 0.5671432904097838
end

@testset "polynomial_coefficients" begin
    @symbolic x p

    @test length(polynomial_coefficients(x^2 - x ~ 1, x)) == 2 + 1
    @test length(polynomial_coefficients(x^2*(x+1)*(x+p) ~ 0, x)) == 4 + 1
    @test isnothing(polynomial_coefficients(x + 1/x ~ 1, x))
    @test isnothing(polynomial_coefficients(x + sin(x)*x^2 ~ 1, x))
end

@testset "sympify" begin
    ex = SimpleExpressions.sympify("x^2 + 2x - a")
    fs = SimpleExpressions.free_symbols(ex)
    @test isempty(fs.p)
    xs = fs.x
    @test length(xs) == 2 && all(uᵢ ∈ (:a, :x) for uᵢ in nameof.(xs))


    ex = SimpleExpressions.sympify(:(x^5 - x - 1))
    @test ex(1) == -1
end
