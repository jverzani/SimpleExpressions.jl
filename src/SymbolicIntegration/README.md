## README

rule2.jl is lightly modified from https://github.com/JuliaSymbolics/SymbolicIntegration.jl/blob/main/src/methods/rule_based/rule2.jl

## LICENSE

SymbolicIntegration.jl is licensed under the MIT License:

Copyright (c) 2022 Harald Hofstätter, Mattia Micheletta Merlin, Chris Rackauckas, and other contributors

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.

## Diffs

>> diff rule2.jl rule2-orig.jl
#
41c41
< #XXX const SymsType = SymbolicUtils.BasicSymbolic{SymbolicUtils.SymReal}
---
> const SymsType = SymbolicUtils.BasicSymbolic{SymbolicUtils.SymReal}
156,162c156
<         # XXXreturn MatchDict(matches, rule.args[2].args[2].args[2], data)::MatchDict
<         args = arguments(data)
<         if length(args) == 1
<             return MatchDict(matches, rule.args[2].args[2].args[2], only(args))::MatchDict
<         else
<             return MatchDict(matches, rule.args[2].args[2].args[2], data)::MatchDict
<         end
---
>         return MatchDict(matches, rule.args[2].args[2].args[2], data)::MatchDict
166,167c160,161
<     if (rule.args[1] == ://) && isa(unwrap_const(data), Rational)
<         r = unwrap_const(data)
---
>     if (rule.args[1] == ://) && isa(SymbolicUtils.unwrap_const(data), Rational)
>         r = SymbolicUtils.unwrap_const(data)
252c246
<         elseif unwrap_const(n)!==1
---
>         elseif SymbolicUtils.unwrap_const(n)!==1
315c309
<         if has_predicate(rule_symbol) #isa(rule_symbol, Expr)
---
>         if isa(rule_symbol, Expr)
317c311
<             pred = get_predicate(rule_symbol) # XXXrule_symbol.args[2]
---
>             pred = rule_symbol.args[2]
319c313
<             !Base.invokelatest(eval(pred),unwrap_const(value_matched)) && return FAIL_DICT
---
>             !Base.invokelatest(eval(pred),SymbolicUtils.unwrap_const(value_matched)) && return FAIL_DICT
325,326c319
<         #XXX return MatchDict(current_dict, rule_symbol, value_matched)::MatchDict
<         return MatchDict(current_dict, varname(rule_symbol), value_matched)::MatchDict
---
>         return MatchDict(current_dict, rule_symbol, value_matched)::MatchDict
332,337c325
<     #    printdb(3,"Checking $data against ℯ, with matches: $(matches...)")
<
<     data′ = has_𝑋(data) ? Symbol(data) : data
<     eq_expr(data′, rule) && return matches # XXX <-- changed this
<     return FAIL_DICT::MatchDict
<
---
> #    printdb(3,"Checking $data against ℯ, with matches: $(matches...)")
339c327
<         unwrap_const(data)===ℯ && return matches::MatchDict
---
>         SymbolicUtils.unwrap_const(data)===ℯ && return matches::MatchDict
357d344
< #=
373c360
<             return unwrap_const(matches[var_name])
---
>             return SymbolicUtils.unwrap_const(matches[var_name])
442d428
< =#
