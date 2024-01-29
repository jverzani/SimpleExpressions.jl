using Metatheory
using Metatheory.Rewriters

export simplify

simplify(ex::Any) = ex
function simplify(ex::SymbolicEquation)
    simplify(ex.lhs) ~ simplify(ex.rhs)
end


function simplify(expr::SymbolicExpression, theory=simple_theory)
    @show :hi
    #𝐹(expr)
    𝐹 = Metatheory.Rewriters.FixpointNoCycle(Prewalk(Fixpoint(RestartedChain(theory))), UInt64[])
    𝐹 = (Prewalk(RestartedChain(theory)))
    #Fixpoint(Prewalk(Fixpoint(RestartedChain(theory))))(expr)
    𝐹(expr)
    #rewrite(expr, theory)
end

simple_theory = @theory a b c m n x begin
    0 + x --> x

    1 * x --> x

    0 / x --> 0
    x / 1 --> x
    1 / x --> x^(-1)
    x / x --> 1

    x^0 --> 1
    0^x --> 0
    1^x --> 1
    x^1 --> x

    -(-x) --> x
    x - x --> 0
    x + (-x) --> 0
    a - b --> a + (-1*b)
    x + x --> 2x
    x + n*x --> (1 + n) * x
    x - n*x --> (1 - n) * x
    m*x + n*x --> (m + n) * x
    m*x - n*x --> (m - n) * x
    m*x + x*n --> (m + n) * x
    m*x - x*n --> (m - n) * x
    x*m + x*n --> (m + n) * x
    x*m - x*n --> (m - n) * x

    x / x^n   --> x^(1 - n)
    x^m / x   --> x^(m - 1)
    x^m / x^n --> x^(m - n)

    (a^n) * a --> a^(n + 1)
    a^n * a^m --> a^(n + m) # was ==
    (a * b)^n --> a^n * b^n # was ==
    (x^a)^b --> x^(a * b)  # was ==
    inv(x) --> x^(-1) # was ==

    cos(-x) --> cos(x)
    sin(-x) --> -sin(x)
    tan(-x) --> -tan(x)

    log(x^a) --> a * log(x)
    exp(log(x)) --> x

    a + (b + c) --> (a + b) + c
    (a + b) + c --> a + (b + c)
    a * (b * c) --> (a * b) * c
    (a * b) * c --> a * (b * c)
end

const 𝐹 = Fixpoint(Prewalk(Fixpoint(RestartedChain(simple_theory))))
