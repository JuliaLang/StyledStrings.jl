# This file is a part of Julia. License is MIT: https://julialang.org/license

const RGBTuple = NamedTuple{(:r, :g, :b), NTuple{3, UInt8}}

"""
    struct SimpleColor

A basic representation of a color, intended for string styling purposes.
It can either contain a named color (like `:red`), or an `RGBTuple` which
is a NamedTuple specifying an `r`, `g`, `b` color with a bit-depth of 8.

# Constructors

```julia
SimpleColor(name::Symbol)  # e.g. :red
SimpleColor(rgb::RGBTuple) # e.g. (r=1, b=2, g=3)
SimpleColor(r::Integer, b::Integer, b::Integer)
SimpleColor(rgb::UInt32)   # e.g. 0x123456
```

Also see `tryparse(SimpleColor, rgb::String)`.
"""
struct SimpleColor
    value::Union{Symbol, RGBTuple}
end

SimpleColor(r::Integer, g::Integer, b::Integer) = SimpleColor((; r=UInt8(r), g=UInt8(g), b=UInt8(b)))

function SimpleColor(rgb::UInt32)
    b, g, r, _ = reinterpret(NTuple{4, UInt8}, htol(rgb))
    SimpleColor(r, g, b)
end

Base.convert(::Type{SimpleColor}, (; r, g, b)::RGBTuple) = SimpleColor((; r, g, b))
Base.convert(::Type{SimpleColor}, namedcolor::Symbol) = SimpleColor(namedcolor)
Base.convert(::Type{SimpleColor}, rgb::UInt32) = SimpleColor(rgb)

"""
    tryparse(::Type{SimpleColor}, rgb::String)

Attempt to parse `rgb` as a `SimpleColor`. If `rgb` starts with
`#` and has a length of 7, it is converted into a `RGBTuple`-backed `SimpleColor`.
If `rgb` starts with `a`-`z`, `rgb` is interpreted as a color name
and converted to a `Symbol`-backed `SimpleColor`.

Otherwise, `nothing` is returned.

# Examples

```jldoctest; setup = :(import StyledStrings.SimpleColor)
julia> tryparse(SimpleColor, "blue")
SimpleColor(blue)

julia> tryparse(SimpleColor, "#9558b2")
SimpleColor(#9558b2)

julia> tryparse(SimpleColor, "#nocolor")
```
"""
function Base.tryparse(::Type{SimpleColor}, rgb::String)
    if ncodeunits(rgb) == 7 && first(rgb) == '#' &&
        all(∈(('#',) ∪ ('0':'9') ∪ ('a':'f') ∪ ('A':'F')), rgb)
        SimpleColor(parse(UInt8, rgb[2:3], base=16),
                    parse(UInt8, rgb[4:5], base=16),
                    parse(UInt8, rgb[6:7], base=16))
    elseif startswith(rgb, 'a':'z') || startswith(rgb, 'A':'Z')
        SimpleColor(Symbol(rgb))
    else
        nothing
    end
end

"""
    parse(::Type{SimpleColor}, rgb::String)

An analogue of `tryparse(SimpleColor, rgb::String)` (which see),
that raises an error instead of returning `nothing`.
"""
function Base.parse(::Type{SimpleColor}, rgb::String)
    color = tryparse(SimpleColor, rgb)
    !isnothing(color) ||
        throw(ArgumentError("invalid color \"$rgb\""))
    color
end

struct _Face
    font::String
    height::UInt64
    weight::Symbol
    slant::Symbol
    foreground::SimpleColor
    background::SimpleColor
    underline::SimpleColor
    underline_style::Symbol
    strikethrough::UInt8
    inverse::UInt8
    inherit::Memory{Symbol}
end

"""
A [`Face`](@ref) is a collection of graphical attributes for displaying text.
Faces control how text is displayed in the terminal, and possibly other
places too.

Most of the time, a [`Face`](@ref) will be stored in the global faces dicts as a
unique association with a *face name* Symbol, and will be most often referred to
by this name instead of the [`Face`](@ref) object itself.

# Attributes

All attributes can be set via the keyword constructor, and default to `nothing`.

- `height` (an `Int` or `Float64`): The height in either deci-pt (when an `Int`),
  or as a factor of the base size (when a `Float64`).
- `weight` (a `Symbol`): One of the symbols (from faintest to densest)
  `:thin`, `:extralight`, `:light`, `:semilight`, `:normal`,
  `:medium`, `:semibold`, `:bold`, `:extrabold`, or `:black`.
  In terminals any weight greater than `:normal` is displayed as bold,
  and in terminals that support variable-brightness text, any weight
  less than `:normal` is displayed as faint.
- `slant` (a `Symbol`): One of the symbols `:italic`, `:oblique`, or `:normal`.
- `foreground` (a `SimpleColor`): The text foreground color.
- `background` (a `SimpleColor`): The text background color.
- `underline`, the text underline, which takes one of the following forms:
  - a `Bool`: Whether the text should be underlined or not.\\
  - a `SimpleColor`: The text should be underlined with this color.\\
  - a `Tuple{Nothing, Symbol}`: The text should be underlined using the style
    set by the Symbol, one of `:straight`, `:double`, `:curly`, `:dotted`,
    or `:dashed`.\\
  - a `Tuple{SimpleColor, Symbol}`: The text should be underlined in the specified
    SimpleColor, and using the style specified by the Symbol, as before.
- `strikethrough` (a `Bool`): Whether the text should be struck through.
- `inverse` (a `Bool`): Whether the foreground and background colors should be
  inverted.
- `inherit` (a `Vector{Symbol}`): Names of faces to inherit from,
  with earlier faces taking priority. All faces inherit from the `:default` face.
"""
mutable struct Face # We want to force reference semantics here
    const f::_Face
end

# NOTE: We use shenanigans instead of `Union{Nothing, X}` in the
# face fields for two reasons:
# 1. To improve the memory layout of `Face`, so there's less indirection
# 2. To allow for having two flavours of `nothing`, "weak" and "strong".
#    This allows for us to distinguish between unset attributes (weak)
#    and attributes that should replace a set value with weak nothing (strong).
# We can only do this because we have full knowledge and dominion
# over the semantics of `Face`.

const WEAK_NOTHING_SYMB = gensym("weak_nothing")
const WEAK_NOTHING_STR = String(WEAK_NOTHING_SYMB)

weaknothing(::Type{Symbol}) = WEAK_NOTHING_SYMB
weaknothing(::Type{String}) = WEAK_NOTHING_STR
weaknothing(::Type{N}) where {N<:Number} = -one(N)
weaknothing(::Type{SimpleColor}) = SimpleColor(WEAK_NOTHING_SYMB)
weaknothing(::Type{Bool}) = strongnothing(UInt8)
weaknothing(::Type) = nothing
weaknothing(x) = weaknothing(typeof(x))

isweaknothing(x) = x === weaknothing(typeof(x))
isweaknothing(s::String) = pointer(s) == pointer(WEAK_NOTHING_STR)
isweaknothing(c::SimpleColor) = c.value === WEAK_NOTHING_SYMB

const STRONG_NOTHING_SYMB = gensym("strong_nothing")
const STRONG_NOTHING_STR = String(STRONG_NOTHING_SYMB)

strongnothing(::Type{Symbol}) = STRONG_NOTHING_SYMB
strongnothing(::Type{String}) = STRONG_NOTHING_STR
strongnothing(::Type{N}) where {N<:Number} = - (0x2 * one(N))
strongnothing(::Type{SimpleColor}) = SimpleColor(STRONG_NOTHING_SYMB)
strongnothing(::Type{Bool}) = strongnothing(UInt8)
strongnothing(::Type) = nothing
strongnothing(x) = strongnothing(typeof(x))

isstrongnothing(x) = x === strongnothing(typeof(x))
isstrongnothing(s::String) = pointer(s) == pointer(STRONG_NOTHING_STR)
isstrongnothing(c::SimpleColor) = c.value === STRONG_NOTHING_SYMB

isnothinglike(x) = isweaknothing(x) || isstrongnothing(x)

# With our flavours of nothing defined, we can now define the Face constructor.

function Face(; font::Union{Nothing, String} = nothing,
              height::Union{Nothing, Int, Float64} = nothing,
              weight::Union{Nothing, Symbol} = nothing,
              slant::Union{Nothing, Symbol} = nothing,
              foreground = nothing, # nothing, or SimpleColor-able value
              background = nothing, # nothing, or SimpleColor-able value
              underline::Union{Nothing, Bool, SimpleColor,
                               Symbol, RGBTuple, UInt32,
                               Tuple{<:Any, Symbol}} = nothing,
              strikethrough::Union{Nothing, Bool} = nothing,
              inverse::Union{Nothing, Bool} = nothing,
              inherit::Union{Symbol, Vector{Symbol}} = Symbol[],
              _...) # Simply ignore unrecognised keyword arguments.
    inheritlist = if inherit isa Vector
        inherit.ref.mem
    else
        mem = Memory{Symbol}(undef, 1)
        mem[1] = inherit
        mem
    end
    ascolor(::Nothing) = SimpleColor(WEAK_NOTHING_SYMB)
    ascolor(c::AbstractString) = parse(SimpleColor, c)
    ascolor(c::Any) = convert(SimpleColor, c)
    ul, ulstyle = if underline isa Tuple{Any, Symbol}
        ascolor(underline[1]), underline[2]
    elseif underline in (:straight, :double, :curly, :dotted, :dashed)
        weaknothing(SimpleColor), underline
    elseif underline isa Bool
        SimpleColor(ifelse(underline, :foreground, :background)), :straight
    else
        ascolor(underline), :straight
    end
    f = _Face(something(font, weaknothing(String)),
              if isnothing(height)
                  weaknothing(UInt64)
              elseif height isa Float64
                  reinterpret(UInt64, height) & ~(typemax(UInt64) >> 1)
              else
                  UInt64(height)
              end,
              something(weight, weaknothing(Symbol)),
              something(slant, weaknothing(Symbol)),
              ascolor(foreground),
              ascolor(background),
              ul, ulstyle,
              something(strikethrough, weaknothing(Bool)),
              something(inverse, weaknothing(Bool)),
              inheritlist)
    Face(f)
end

function Base.getproperty(face::Face, attr::Symbol)
    val = getfield(getfield(face, :f), attr)
    if isnothinglike(val)
    elseif attr ∈ (:strikethrough, :inverse)
        if attr != 0x3
            attr == 0x1
        end
    elseif attr == :underline
        style = getfield(getfield(face, :f), :underline_style)
        val, style
    else
        val
    end
end

Base.propertynames(::Face) = setdiff(fieldnames(_Face), (:underline_style,))

function Base.:(==)(a::Face, b::Face)
    af, bf = getfield(a, :f), getfield(b, :f)
    af.font          == bf.font &&
    af.height        === bf.height &&
    af.weight        == bf.weight &&
    af.slant         == bf.slant &&
    af.foreground    == bf.foreground &&
    af.background    == bf.background &&
    af.underline     == bf.underline &&
    af.strikethrough == bf.strikethrough &&
    af.inverse       == bf.inverse &&
    af.inherit       == bf.inherit
end

Base.hash(f::Face, h::UInt) =
    mapfoldr(Base.Fix1(getfield, getfield(f, :f)), hash, fieldnames(Face), init=hash(Face, h))

function Base.copy(f::Face)
    ff = getfield(f, :f)
    Face(_Face(ff.font, ff.height, ff.weight, ff.slant,
               ff.foreground, ff.background, ff.underline,
               ff.strikethrough, ff.inverse, copy(ff.inherit)))
end

"""
    merge(initial::StyledStrings.Face, others::StyledStrings.Face...)

Merge the properties of the `initial` face and `others`, with later faces taking priority.

This is used to combine the styles of multiple faces, and to resolve inheritance.
"""
function Base.merge(a::Face, b::Face)
    af, bf = getfield(a, :f), getfield(b, :f)
    function mergeattr(af′, bf′, attr::Symbol)
        a′ = getfield(af′, attr)
        b′ = getfield(bf′, attr)
        if isweaknothing(b′)
            a′
        elseif isstrongnothing(b′)
            weaknothing(b′)
        else
            b′
        end
    end
    if isempty(bf.inherit)
        abheight = if isstrongnothing(bf.height)
            weaknothing(bf.height)
        elseif isweaknothing(bf.height)
            af.height
        elseif isnothinglike(af.height)
            bf.height
        elseif iszero(bf.height & ~(typemax(UInt64) >> 1)) # bf.height::Int
            bf.height
        elseif iszero(af.height & ~(typemax(UInt64) >> 1)) # af.height::Int
            aint = reinterpret(Int64, af.height) % UInt
            bfloat = reinterpret(Float64, bf.height & (typemax(UInt64) >> 1))
            aint * bfloat
        else # af.height::Float64, bf.height::Float64
            afloat = reinterpret(Float64, af.height & (typemax(UInt64) >> 1))
            bfloat = reinterpret(Float64, bf.height & (typemax(UInt64) >> 1))
            afloat * bfloat
        end
        Face(_Face(
            mergeattr(af, bf, :font),
            abheight,
            mergeattr(af, bf, :weight),
            mergeattr(af, bf, :slant),
            mergeattr(af, bf, :foreground),
            mergeattr(af, bf, :background),
            mergeattr(af, bf, :underline),
            if isweaknothing(bf.underline_style)
                af.underline_style
            else
                bf.underline_style
            end,
            mergeattr(af, bf, :strikethrough),
            mergeattr(af, bf, :inverse),
            af.inherit))
    else
        b_noinherit = Face(_Face(
            bf.font, bf.height, bf.weight, bf.slant, bf.foreground, bf.background,
            bf.underline, bf.strikethrough, bf.inverse, Symbol[]))
        b_inheritance = map(fname -> get(Face, FACES.current[], fname), Iterators.reverse(bf.inherit))
        b_resolved = merge(foldl(merge, b_inheritance), b_noinherit)
        merge(a, b_resolved)
    end
end

Base.merge(a::Face, b::Face, others::Face...) = merge(merge(a, b), others...)

function Base.show(io::IO, ::MIME"text/plain", color::SimpleColor)
    skiptype = get(io, :typeinfo, nothing) === SimpleColor
    skiptype || show(io, SimpleColor)
    skiptype || print(io, '(')
    if get(io, :color, false)::Bool
        print(io, AnnotatedString("■", [(region=1:1, label=:face, value=Face(foreground=color))]), ' ')
    end
    if color.value isa RGBTuple
        (; r, g, b) = color.value
        print(io, '#',
              rpad(string(r, base=16), 2, '0'),
              rpad(string(g, base=16), 2, '0'),
              rpad(string(b, base=16), 2, '0'))
    else
        print(io, color.value)
    end
    skiptype || print(io, ')')
    nothing
end

function Base.show(io::IO, color::SimpleColor)
    show(io, SimpleColor)
    print(io, '(')
    if color.value isa RGBTuple
        (; r, g, b) = color.value
        print(io, "0x",
              rpad(string(r, base=16), 2, '0'),
              rpad(string(g, base=16), 2, '0'),
              rpad(string(b, base=16), 2, '0'))
    else
        show(io, color.value)
    end
    print(io, ')')
end

function Base.show(io::IO, ::MIME"text/plain", face::Face)
    if get(io, :compact, false)::Bool
        show(io, Face)
        if get(io, :color, false)::Bool
            # Could do styled"({$face:sample})", but S_str isn't defined yet
            print(io, AnnotatedString("(sample)", [(region=2:7, label=:face, value=face)]))
        # elseif get(io, :limit, false)::Bool
        #     print(io, "(…)")
        else
            print(io, '(')
            isfirst = true
            for field in setdiff(fieldnames(Face), (:inherit,))
                if !isnothing(getfield(face, field))
                    if isfirst; isfirst = false else print(io, ", ") end
                    print(io, field, '=')
                    show(io, getfield(face, field))
                end
            end
            if !isempty(face.inherit)
                if isfirst; isfirst = false else print(io, ", ") end
                print(io, "inherit=")
                show(IOContext(io, :typeinfo => Vector{Symbol}), face.inherit)
            end
            print(io, ')')
        end
    else
        show(io, Face)
        print(io, AnnotatedString(" (sample)", [(region=3:8, label=:face, value=face)]))
        showcolor(io, color) = show(IOContext(io, :typeinfo => SimpleColor),
                                    MIME("text/plain"), color)
        setfields = Pair{Symbol, Any}[]
        isempty(setfields) || print(io, ":")
        fieldnamepad = 14
        for field in (:font, :height, :weight, :slant)
            if !isnothing(getfield(face, field))
                print(io, '\n', lpad(String(field), fieldnamepad, ' '), ": ",
                      getfield(face, field))
            end
        end
        for field in (:foreground, :background)
            if !isnothing(getfield(face, field))
                print(io, '\n', lpad(String(field), fieldnamepad, ' '), ": ")
                showcolor(io, getfield(face, field))
            end
        end
        if !isnothing(face.underline)
            print(io, '\n', lpad("underline", fieldnamepad, ' '), ": ")
            if face.underline isa Bool
                print(io, face.underline)
            elseif face.underline isa SimpleColor
                showcolor(io, face.underline)
            elseif face.underline isa Tuple{Nothing, Symbol}
                print(io, last(face.underline))
            elseif face.underline isa Tuple{SimpleColor, Symbol}
                showcolor(io, first(face.underline))
                print(io, ", ", last(face.underline))
            end
        end
        for field in (:strikethrough, :inverse)
            if !isnothing(getfield(face, field))
                print(io, '\n', lpad(String(field), fieldnamepad, ' '), ": ",
                      getfield(face, field))
            end
        end
        if !isempty(face.inherit)
            print(io, '\n', lpad("inherit", fieldnamepad, ' '), ": ")
            isfirst = true
            for iface in face.inherit
                if isfirst; isfirst = false else print(io, ", ") end
                print(io, iface, '(', AnnotatedString("*", [(region=1:1, label=:face, value=iface)]), ')')
            end
        end
    end
end

function Base.show(io::IO, face::Face)
    show(IOContext(io, :compact => true), MIME("text/plain"), face)
end
