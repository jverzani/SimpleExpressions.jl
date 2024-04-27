module SimpleExpressionsBinderPlotsExt

import SimpleExpressions
import BinderPlots

# plot method for equations
function BinderPlots.plot!(t::Val{:scatter}, p::BinderPlots.Plot,
               f::SimpleExpressions.SymbolicEquation, y, z;
               seriestype::Symbol=:lines,
               kwargs...)
    BinderPlots.plot!(p, [f.lhs, f.rhs], y, z; seriestype, kwargs...)
end

end
