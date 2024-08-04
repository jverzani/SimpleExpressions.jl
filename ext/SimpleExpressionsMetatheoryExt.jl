module SimpleExpressionsMetatheoryExt

import SimpleExpressions
import SimpleExpressions: SymbolicNumber

using Metatheory
using Metatheory.Library

function SimpleExpressions.simplify(ex::SimpleExpressions.SymbolicExpression)

    mult_t = @commutative_monoid (*) 1
    plus_t = @commutative_monoid (+) 0

    add_t = @theory a n m begin
        a + a == 2a
        n::Number * a + a == (n+1) * a
        a + m::Number*a == (1 + m) * a
        n::Number * a + m::Number * a == (n + m) * a
    end

    minus_t = @theory a b begin
        a - a --> 0
        a + (-b) == a - b
    end

    mulplus_t = @theory a b c begin
        0 * a --> 0
        a * 0 --> 0
        a * (b + c) == ((a * b) + (a * c))
        a + (b * a) == ((b + 1) * a)
    end

    pow_t = @theory x y z n m p q begin
        (y^n) * y == y^(n + 1)
        x^n * x^m == x^(n + m)
        (x * y)^z == x^z * y^z
        (x^p)^q == x^(p * q)
        x^0 --> 1
        0^x --> 0
        1^x --> 1
        x^1 --> x
        inv(x) == x^(-1)
    end
    maths_theory = mult_t ∪ plus_t ∪ add_t ∪ minus_t ∪ mulplus_t ∪ pow_t

    g = EGraph(ex)
    saturate!(g, maths_theory)
    extract!(g, astsize)
end

end
