# This file is a part of Julia. License is MIT: https://julialang.org/license

module Fuzzer

using Supposition

# Copied from `../src/styledmarkup.jl`
const VALID_WEIGHTS = ("thin", "extralight", "light", "semilight", "normal",
                       "medium", "semibold", "bold", "extrabold", "black")
const VALID_SLANTS = ("italic", "oblique", "normal")
const VALID_UNDERLINE_STYLES = ("straight", "double", "curly", "dotted", "dashed")

# Utility syntax

const emptystr = Data.Just("")
const dnothing = Data.Just("nothing")
const bool = Data.SampledFrom(["true", "false"])
const alphanum = ['a':'z'..., 'A':'Z'..., '0':'9'..., '_']
const identifier =
    filter(Base.isidentifier,
           Data.Text(Data.SampledFrom(alphanum), min_len=1, max_len=20))
const hexcolor =
    @composed (bytes = Data.Text(Data.SampledFrom(['0':'9'..., 'a':'f'...]), min_len=6, max_len=6)) ->
    '#' * bytes
const namecolor = identifier
const whitespace = Data.Text(Data.SampledFrom(collect(" \t\n")), max_len=3)

const integer = Data.Text(Data.SampledFrom('0':'9'), min_len=1, max_len=5)
const decimalND = @composed (n=emptystr | integer, d=emptystr | integer) ->
    n * '.' * d
const decimalN = @composed (n=integer) -> n * '.'
const decimalD = @composed (d=integer) -> '.' * d
const decimal = decimalND | decimalN | decimalD
const pmsign = Data.SampledFrom(["+", "-"])
const exponent = @composed (s1=emptystr | pmsign, frac=decimal, s2=emptystr | pmsign, exp=integer) ->
    s1 * frac * 'e' * s2 * exp
const number = integer | decimal | exponent

# Inline face syntax (from EBNF)

const simplecolor = hexcolor | namecolor | dnothing

const underlineline = Data.SampledFrom(VALID_UNDERLINE_STYLES)
const underlinestyled =
    @composed (ws1=whitespace, v1=emptystr | dnothing | simplecolor,
               ws2=whitespace, ws3=whitespace, v2=underlineline, ws4=whitespace) ->
    '(' * ws1 * v1 * ws2 * ',' * ws3 * v2 * ws4 * ')'
const underline = dnothing | bool | simplecolor | underlineline | underlinestyled

const inheritval = @composed (ws1=whitespace, colonp=Data.Booleans(), symb=identifier) ->
    ws1 * ifelse(colonp, ":", "") * symb
const inherit = inheritval | map(Data.Vectors(inheritval, min_size=1, max_size=5)) do v
    '[' * join(v, ',') * ']'
end

const faceprop_font =
    @composed (ws0=whitespace, ws1=whitespace, ws2=whitespace, val=identifier, ws3=whitespace) ->
    ws0 * "font" * ws1 * '=' * ws2 * val * ws3
const faceprop_height =
    @composed (ws0=whitespace, ws1=whitespace, ws2=whitespace, val=integer|decimal, ws3=whitespace) ->
    ws0 * "height" * ws1 * '=' * ws2 * val * ws3
const faceprop_weightvals = Data.SampledFrom(VALID_WEIGHTS)
const faceprop_weight =
    @composed (ws0=whitespace, ws1=whitespace, ws2=whitespace, val=faceprop_weightvals, ws3=whitespace) ->
    ws0 * "weight" * ws1 * '=' * ws2 * val * ws3
const faceprop_slantvals = Data.SampledFrom(VALID_SLANTS)
const faceprop_slant =
    @composed (ws0=whitespace, ws1=whitespace, ws2=whitespace, val=faceprop_slantvals, ws3=whitespace) ->
    ws0 * "slant" * ws1 * '=' * ws2 * val * ws3
const faceprop_fg =
    @composed (key=Data.SampledFrom(["foreground", "fg"]),
               ws0=whitespace, ws1=whitespace, ws2=whitespace, val=simplecolor, ws3=whitespace) ->
    ws0 * key * ws1 * '=' * ws2 * val * ws3
const faceprop_bg =
    @composed (key=Data.SampledFrom(["background", "bg"]),
               ws0=whitespace, ws1=whitespace, ws2=whitespace, val=simplecolor, ws3=whitespace) ->
    ws0 * key * ws1 * '=' * ws2 * val * ws3
const faceprop_ul =
    @composed (ws0=whitespace, ws1=whitespace, ws2=whitespace, val=underline, ws3=whitespace) ->
    ws0 * "underline" * ws1 * '=' * ws2 * val * ws3
const faceprop_inv =
    @composed (ws0=whitespace, ws1=whitespace, ws2=whitespace, val=bool, ws3=whitespace) ->
    ws0 * "inverse" * ws1 * '=' * ws2 * val * ws3
const faceprop_inh =
    @composed (ws0=whitespace, ws1=whitespace, ws2=whitespace, val=inherit, ws3=whitespace) ->
    ws0 * "inherit" * ws1 * '=' * ws2 * val * ws3

const faceprop_pos = reduce(|, (
    Data.Pairs(Data.Just(:face), faceprop_font),
    Data.Pairs(Data.Just(:height), faceprop_height),
    Data.Pairs(Data.Just(:weight), faceprop_weight),
    Data.Pairs(Data.Just(:slant), faceprop_slant),
    Data.Pairs(Data.Just(:fg), faceprop_fg),
    Data.Pairs(Data.Just(:bg), faceprop_bg),
    Data.Pairs(Data.Just(:ul), faceprop_ul),
    Data.Pairs(Data.Just(:inv), faceprop_inv),
    Data.Pairs(Data.Just(:inh), faceprop_inh)
))

const faceprop_vec = map(v -> map(last, unique(first, v)), Data.Vectors(faceprop_pos; max_size=8))

const inlineface =
    @composed (ws1=whitespace, vals=faceprop_vec, ws2=whitespace)  ->
    '(' * ws1 * join(vals, ',') * ws2 * ')'

# General syntax (from EBNF)

const anychar = Data.Characters()
const specialchar = Data.SampledFrom(collect("{}\$\""))
const plain =
    Data.Text(filter(c -> c ∉ "{}\$\"\\", anychar); max_len=20)
const escaped = map(c -> '\\' * c, specialchar)

const interpolated = Data.OneOf(
    map(s -> '\$'*s, identifier),
    map(s -> "\$("*s*")", identifier)
)

const facename = Data.Text(Data.SampledFrom(alphanum); min_len=1, max_len=20)
const face = facename #| interpolated

const key = (@composed function k(
                 start=filter(c -> !(c in "\0\${},:"), anychar),
                 next=Data.Text(filter(c -> !(c in "\0=,:"), anychar); max_len=20))
                 start*next
             end) #| interpolated

const curlybraced = map(Data.Vectors(escaped | plain; max_size=10)) do str
    join('{', str, '}')
end

const simplevalue =
    @composed (pre=filter(c -> c ∉ "\${},:", anychar),
               post=Data.Text(filter(c -> !(c in ",:"), anychar); max_len=20)) ->
    pre*post

const value = simplevalue #| curlybraced #| interpolated

const keyvalue =
    @composed (k=key,ws1=whitespace,ws2=whitespace,v=value) ->
    k*ws1*'='*ws2*v

const annotation = face | inlineface | keyvalue

const padded_annotation =
    @composed (ws1=whitespace, ann=annotation, ws2=whitespace) ->
    ws1 * ann * ws2

const annotations =
    @composed (anns=Data.Vectors(padded_annotation, max_size=3)) ->
    join(anns, ',')

const styledpos =
    @composed (ws1=whitespace, anns=annotations, c=content) ->
    '{'*ws1*anns*':'*c*'}'

const content = map(join, Data.Vectors(#=interpolated | =# plain | escaped | styledpos; max_size=10))

end
