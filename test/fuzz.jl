# This file is a part of Julia. License is MIT: https://julialang.org/license

using StyledStrings: styled
using Supposition

# Utility syntax

const emptystr = Data.Just("")
const dnothing = Data.Just("nothing")
const bool = Data.SampledFrom(["true", "false"])
const alphanum = ['a':'z'..., 'A':'Z'..., '0':'9'..., '_']
const identifier =
    filter(Base.isidentifier,
           Data.Text(Data.SampledFrom(alphanum), min_len=1, max_len=20))
const hexcolor = @composed function hashprefix(
    bytes = Data.Text(Data.SampledFrom(['0':'9'..., 'a':'f'...]), min_len=6, max_len=6))
    '#' * bytes
end
const namecolor = identifier # Data.SampledFrom(StyledStrings.StyledMarkup.VALID_COLORNAMES)
const whitespace = Data.Text(Data.SampledFrom(collect(" \t\n")), max_len=3)

const integer = Data.Text(Data.SampledFrom('0':'9'), min_len=1, max_len=5)
const decimalND = @composed function dec(n=emptystr | integer, d=emptystr | integer)
    n * '.' * d
end
const decimalN = @composed function dec(n=integer) n * '.' end
const decimalD = @composed function dec(d=integer) '.' * d end
const decimal = decimalND | decimalN | decimalD
const pmsign = Data.SampledFrom(["+", "-"])
const exponent = @composed function dexp(
    s1=emptystr | pmsign, frac=decimal, s2=emptystr | pmsign, exp=integer)
    s1 * frac * 'e' * s2 * exp
end
const number = integer | decimal | exponent

# Inline face syntax (from EBNF)

const simplecolor = hexcolor | namecolor | dnothing

const underlineline = Data.SampledFrom(
    StyledStrings.StyledMarkup.VALID_UNDERLINE_STYLES)
const underlinestyled = @composed function ulsty(
    ws1=whitespace, v1=emptystr | dnothing | simplecolor,
    ws2=whitespace, ws3=whitespace, v2=underlineline, ws4=whitespace)
    '(' * ws1 * v1 * ws2 * ',' * ws3 * v2 * ws4 * ')'
end
const underline = dnothing | bool | simplecolor | underlineline | underlinestyled

const inheritval = @composed function ival(
    ws1=whitespace, colonp=Data.Booleans(), symb=identifier)
    ws1 * ifelse(colonp, ":", "") * symb
end
const inherit = inheritval | map(Data.Vectors(inheritval, min_size=1, max_size=5)) do v
    '[' * join(v, ',') * ']'
end

const faceprop_face = @composed function fpface(
    ws0=whitespace, ws1=whitespace, ws2=whitespace, val=identifier, ws3=whitespace)
    ws0 * "face" * ws1 * '=' * ws2 * val * ws3
end
const faceprop_height = @composed function fpheight(
    ws0=whitespace, ws1=whitespace, ws2=whitespace, val=integer|decimal, ws3=whitespace)
    ws0 * "height" * ws1 * '=' * ws2 * val * ws3
end
const faceprop_weightvals = Data.SampledFrom(
    StyledStrings.StyledMarkup.VALID_WEIGHTS)
const faceprop_weight = @composed function fpweight(
    ws0=whitespace, ws1=whitespace, ws2=whitespace, val=faceprop_weightvals, ws3=whitespace)
    ws0 * "weight" * ws1 * '=' * ws2 * val * ws3
end
const faceprop_slantvals = Data.SampledFrom(
    StyledStrings.StyledMarkup.VALID_SLANTS)
const faceprop_slant = @composed function fpslant(
    ws0=whitespace, ws1=whitespace, ws2=whitespace, val=faceprop_slantvals, ws3=whitespace)
    ws0 * "slant" * ws1 * '=' * ws2 * val * ws3
end
const faceprop_fg = @composed function fpfg(
    key=Data.SampledFrom(["foreground", "fg"]),
    ws0=whitespace, ws1=whitespace, ws2=whitespace, val=simplecolor, ws3=whitespace)
    ws0 * key * ws1 * '=' * ws2 * val * ws3
end
const faceprop_bg = @composed function fpbg(
    key=Data.SampledFrom(["background", "bg"]),
    ws0=whitespace, ws1=whitespace, ws2=whitespace, val=simplecolor, ws3=whitespace)
    ws0 * key * ws1 * '=' * ws2 * val * ws3
end
const faceprop_ul = @composed function fpul(
    ws0=whitespace, ws1=whitespace, ws2=whitespace, val=underline, ws3=whitespace)
    ws0 * "underline" * ws1 * '=' * ws2 * val * ws3
end
const faceprop_inv = @composed function fpinv(
    ws0=whitespace, ws1=whitespace, ws2=whitespace, val=bool, ws3=whitespace)
    ws0 * "inverse" * ws1 * '=' * ws2 * val * ws3
end
const faceprop_inh = @composed function fpinh(
    ws0=whitespace, ws1=whitespace, ws2=whitespace, val=inherit, ws3=whitespace)
    ws0 * "inherit" * ws1 * '=' * ws2 * val * ws3
end

const faceprop_pos = reduce(|, (
    Data.Pairs(Data.Just(:face), faceprop_face),
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

const inlineface = @composed function inlf(
    ws1=whitespace, vals=faceprop_vec, ws2=whitespace)
    '(' * ws1 * join(vals, ',') * ws2 * ')'
end

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
                 start=filter(c -> !(c in "\${},:"), anychar),
                 next=Data.Text(filter(c -> !(c in "=,:"), anychar); max_len=20))
                 start*next
             end) #| interpolated

const curlybraced = map(Data.Vectors(escaped | plain; max_size=10)) do str
    join('{', str, '}')
end

const simplevalue = @composed function simpvalue(
    pre=filter(c -> c ∉ "\${},:", anychar),
    post=Data.Text(filter(c -> !(c in ",:"), anychar); max_len=20))
    pre*post
end

const value = simplevalue #| curlybraced #| interpolated

const keyvalue = @composed function kv(k=key,ws1=whitespace,ws2=whitespace,v=value)
    k*ws1*'='*ws2*v
end

const annotation = face | inlineface | keyvalue

const padded_annotation = @composed function pannot(
    ws1=whitespace, ann=annotation, ws2=whitespace)
    ws1 * ann * ws2
end

const annotations = @composed function anj(
    anns=Data.Vectors(padded_annotation, max_size=3))
    join(anns, ',')
end

const styledpos = @composed function stld(ws1=whitespace, anns=annotations, c=content)
    '{'*ws1*anns*':'*c*'}'
end

const content = map(join, Data.Vectors(#=interpolated | =# plain | escaped | styledpos; max_size=10))

isstyled(s) = styled(s) isa Base.AnnotatedString

# Run this to check:
 @__MODULE__() === Main &&
     (print("Run fuzzer? [y/N]: "); read(stdin, Char)) ∈ ('y', 'Y')
     @check isstyled(content)
