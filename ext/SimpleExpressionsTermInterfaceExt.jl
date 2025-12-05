module SimpleExpressionsTermInterfaceExt

using SimpleExpressions
import SimpleExpressions: AbstractSymbolic
import TermInterface

# will error on older TermInterface. Might want to do both?
TermInterface.is_operation(x::AbstractSymbolic) = SimpleExpressions.is_operation(x)
TermInterface.operation(x::AbstractSymbolic) = SimpleExpressions.operation(x)
TermInterface.arguments(x::AbstractSymbolic) = SimpleExpressions.arguments(x)
TermInterface.sorted_arguments(x::AbstractSymbolic) =
    SimpleExpressions.sorted_arguments(x)
TermInterface.head(x::AbstractSymbolic) = SimpleExpressions.head(x)
TermInterface.children(x::AbstractSymbolic) = SimpleExpressions.children(x)
TermInterface.iscall(x::AbstractSymbolic) = SimpleExpressions.iscall(x)
TermInterface.isexpr(x::AbstractSymbolic) = SimpleExpressions.isexpr(x)
TermInterface.maketerm(x::AbstractSymbolic) = SimpleExpressions.maketerm(x)
TermInterface.metadata(x::AbstractSymbolic) = SimpleExpressions.metadata(x)
TermInterface.metadata(x::AbstractSymbolic, md) = SimpleExpressions.metadata(x, md)

end
