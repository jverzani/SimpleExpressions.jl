# What? Why?

CallableExpression has a TermInterface 0.4 requirement which is a problem here. So until that gets bumped, we just vendor in the package code from v1.1.1 and install it here. Not ideal, but the current released version of Metatheory.jl uses an older TermInterface version.
