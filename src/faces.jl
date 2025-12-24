# This file is a part of Julia. License is MIT: https://julialang.org/license

const RGBTuple = NamedTuple{(:r, :g, :b), NTuple{3, UInt8}}

struct _SimpleColor{_F}
    value::Union{_F, RGBTuple}
end

struct _FaceDef{_F}
    font::String
    height::UInt32
    strikethrough::UInt8
    inverse::UInt8
    weight::Symbol
    slant::Symbol
    foreground::_SimpleColor{_F}
    background::_SimpleColor{_F}
    underline::_SimpleColor{_F}
    underline_style::Symbol
    inherit::Memory{_F}
end

 # We want to force reference semantics for `Face`.
 # The sole field would be a `const`, if there were
 # any way to safely do that and have `new_recursive_fg_face`.
mutable struct Face
    f::_FaceDef{Face}
    Face(f::_FaceDef{Face}) = new(f)
    global uninitialised_face() = new()
end

Base.setproperty!(::Face, ::Symbol, ::Any) = throw(ArgumentError("Faces are immutable"))

function new_recursive_fg_face()
    f = uninitialised_face()
    setfield!(f, :f, _FaceDef{Face}(
        weaknothing(String), weaknothing(UInt32),
        weaknothing(UInt8), weaknothing(UInt8),
        weaknothing(Symbol), weaknothing(Symbol),
        SimpleColor(f), weaknothing(SimpleColor),
        weaknothing(SimpleColor), weaknothing(Symbol),
        Memory{Face}()))
    f
end

const FaceDef = _FaceDef{Face}

const SimpleColor = _SimpleColor{Face}

@doc """
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
""" Face

@doc """
    struct SimpleColor

A basic representation of a color, intended for string styling purposes.
It can either contain a named color (like `:red`), or an `RGBTuple` which
is a NamedTuple specifying an `r`, `g`, `b` color with a bit-depth of 8.

# Constructors

```julia
SimpleColor(face::Face)    # e.g. face"red"
SimpleColor(r::Integer, b::Integer, b::Integer) # 0-255
SimpleColor(rgb::RGBTuple) # e.g. (r=0x12, b=0x34, g=0x56)
SimpleColor(rgb::UInt32)   # e.g. 0x123456
```

Also see `tryparse(SimpleColor, rgb::String)`.
""" SimpleColor

SimpleColor(r::Integer, g::Integer, b::Integer) = SimpleColor((; r=UInt8(r), g=UInt8(g), b=UInt8(b)))

function SimpleColor(rgb::UInt32)
    b, g, r, _ = reinterpret(NTuple{4, UInt8}, htol(rgb))
    SimpleColor(r, g, b)
end

Base.convert(::Type{SimpleColor}, (; r, g, b)::RGBTuple) = SimpleColor((; r, g, b))
Base.convert(::Type{SimpleColor}, face::Face) = SimpleColor(face)
Base.convert(::Type{SimpleColor}, rgb::UInt32) = SimpleColor(rgb)

function Base.convert(::Type{SimpleColor}, namedcolor::Symbol)
    Base.depwarn("Creating a SimpleColor from a face name Symbol is deprecated as of v1.14. Use faces directly instead, such as from `face\"colourname\"`", :convert)
    SimpleColor(lookmakeface(namedcolor, false))
end

"""
    tryparse(::Type{SimpleColor}, rgb::String)

Attempt to parse `rgb` as a `SimpleColor`. If `rgb` starts with
`#` and has a length of 7, it is converted into a `RGBTuple`-backed `SimpleColor`.
If `rgb` starts with `a`-`z`, `rgb` is interpreted as a color name
and converted to a [`Face`](@ref)-backed `SimpleColor`.

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
        SimpleColor(lookmakeface(Symbol(rgb), false))
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

# NOTE: We use shenanigans instead of `Union{Nothing, X}` in the
# face fields for two reasons:
# 1. To improve the memory layout of `Face`, so there's less indirection
# 2. To allow for having two flavours of `nothing`, "weak" and "strong".
#    This allows for us to distinguish between unset attributes (weak)
#    and attributes that should replace a set value with weak nothing (strong).
# We can only do this because we have full knowledge and dominion
# over the semantics of `Face`.
#
# This is slightly funky. Don't look too closely.

const WEAK_NOTHING_SYMB = gensym("weak_nothing")
const WEAK_NOTHING_STR = String(WEAK_NOTHING_SYMB)
const WEAK_NOTHING_FACE = Face(FaceDef(
    WEAK_NOTHING_STR, typemax(UInt32) >> 2, UInt8(0), UInt8(0), WEAK_NOTHING_SYMB, WEAK_NOTHING_SYMB,
    SimpleColor((r = 0x00, g = 0x00, b = 0x00)), SimpleColor((r = 0x00, g = 0x00, b = 0x00)),
    SimpleColor((r = 0x00, g = 0x00, b = 0x00)), WEAK_NOTHING_SYMB, Memory{Face}()))

weaknothing(::Type{Symbol}) = WEAK_NOTHING_SYMB
weaknothing(::Type{String}) = WEAK_NOTHING_STR
weaknothing(::Type{N}) where {N<:Number} = - (0x2 * one(N))
weaknothing(::Type{Face}) = WEAK_NOTHING_FACE
weaknothing(::Type{SimpleColor}) = SimpleColor(weaknothing(Face))
weaknothing(::Type{Bool}) = weaknothing(UInt8)
weaknothing(::Type) = nothing
weaknothing(x) = weaknothing(typeof(x))

isweaknothing(x) = x === weaknothing(typeof(x))
isweaknothing(u::UInt32) = isnothinglike(u) && !isstrongnothing(u)
isweaknothing(s::String) = pointer(s) == pointer(WEAK_NOTHING_STR)
isweaknothing(c::SimpleColor) = c.value isa Face && pointer_from_objref(c.value) == pointer_from_objref(WEAK_NOTHING_FACE)

const STRONG_NOTHING_SYMB = gensym("strong_nothing")
const STRONG_NOTHING_STR = String(STRONG_NOTHING_SYMB)
const STRONG_NOTHING_FACE = Face(FaceDef(
    WEAK_NOTHING_STR, typemax(UInt32) >> 1, UInt8(0), UInt8(0), WEAK_NOTHING_SYMB, WEAK_NOTHING_SYMB,
    SimpleColor((r = 0x00, g = 0x00, b = 0x00)), SimpleColor((r = 0x00, g = 0x00, b = 0x00)),
    SimpleColor((r = 0x00, g = 0x00, b = 0x00)), WEAK_NOTHING_SYMB, Memory{Face}()))

strongnothing(::Type{Symbol}) = STRONG_NOTHING_SYMB
strongnothing(::Type{String}) = STRONG_NOTHING_STR
strongnothing(::Type{N}) where {N<:Number} = -one(N)
strongnothing(::Type{SimpleColor}) = SimpleColor(strongnothing(Face))
strongnothing(::Type{Bool}) = strongnothing(UInt8)
strongnothing(::Type) = nothing
strongnothing(x) = strongnothing(typeof(x))

isstrongnothing(x) = x === strongnothing(typeof(x))
isstrongnothing(s::String) = pointer(s) == pointer(STRONG_NOTHING_STR)
isstrongnothing(c::SimpleColor) = c.value isa Face && pointer_from_objref(c.value) == pointer_from_objref(STRONG_NOTHING_FACE)

isnothinglike(x) = isweaknothing(x) || isstrongnothing(x)
isnothinglike(u::UInt32) = u & 0xff800000 == 0xff800000

# With our flavours of nothing defined, we can now define the Face constructor.

const BASE_FACES =
    (foreground     = new_recursive_fg_face(),
     background     = new_recursive_fg_face(),
     black          = new_recursive_fg_face(),
     red            = new_recursive_fg_face(),
     green          = new_recursive_fg_face(),
     yellow         = new_recursive_fg_face(),
     blue           = new_recursive_fg_face(),
     magenta        = new_recursive_fg_face(),
     cyan           = new_recursive_fg_face(),
     white          = new_recursive_fg_face(),
     bright_black   = new_recursive_fg_face(),
     bright_red     = new_recursive_fg_face(),
     bright_green   = new_recursive_fg_face(),
     bright_yellow  = new_recursive_fg_face(),
     bright_blue    = new_recursive_fg_face(),
     bright_magenta = new_recursive_fg_face(),
     bright_cyan    = new_recursive_fg_face(),
     bright_white   = new_recursive_fg_face())

const EMPTY_FACE = Face(FaceDef(
        weaknothing(String), weaknothing(UInt32), # font, height
        weaknothing(Bool), weaknothing(Bool), # strikethrough, inverse
        weaknothing(Symbol), weaknothing(Symbol), # weight, slant
        weaknothing(SimpleColor), weaknothing(SimpleColor), # foreground, background
        weaknothing(SimpleColor), weaknothing(Symbol), # underline, underline_style
        Memory{Face}())) # inherit

function Face(; font::Union{Nothing, String} = nothing,
              height::Union{Nothing, <:Integer, <:AbstractFloat} = nothing,
              weight::Union{Nothing, Symbol} = nothing,
              slant::Union{Nothing, Symbol} = nothing,
              foreground = nothing, # nothing, or SimpleColor-able value
              background = nothing, # nothing, or SimpleColor-able value
              underline::Union{Nothing, Bool, SimpleColor,
                               Symbol, Face, RGBTuple, UInt32,
                               Tuple{<:Any, Symbol}} = nothing,
              strikethrough::Union{Nothing, Bool} = nothing,
              inverse::Union{Nothing, Bool} = nothing,
              inherit::Union{Nothing, Face, Vector{Face}, Symbol, Vector{Symbol}} = nothing,
              _...) # Simply ignore unrecognised keyword arguments.
    if all(isnothing, (font, height, weight, slant, foreground, background,
                       underline, strikethrough, inverse, inherit))
        return EMPTY_FACE
    end
    inheritlist = if isnothing(inherit)
        Memory{Face}()
    elseif inherit isa Vector{Face}
        inherit.ref.mem
    elseif inherit isa Face
        mem = Memory{Face}(undef, 1)
        mem[1] = inherit
        mem
    elseif inherit isa Vector{Symbol} # Backwards compat (1)
        Base.depwarn("Using symbols to refer to faces is deprecated as of v1.14. Construct face directly with `face\"\"` instead.", :Face)
        [lookmakeface(fname) for fname in inherit].ref.mem
    elseif inherit isa Symbol # Backwards compat (2)
        Base.depwarn("Using symbols to refer to faces is deprecated as of v1.14. Construct face directly with `face\"\"` instead.", :Face)
        mem = Memory{Face}(undef, 1)
        mem[1] = lookmakeface(inherit)
        mem
    end
    ascolor(::Nothing) = weaknothing(SimpleColor)
    ascolor(c::AbstractString) = parse(SimpleColor, c)
    ascolor(c::Any) = convert(SimpleColor, c)
    ul, ulstyle = if isnothing(underline)
        weaknothing(SimpleColor), weaknothing(Symbol)
    elseif underline isa Tuple{<:Any, Symbol}
        ascolor(underline[1]), underline[2]
    elseif underline in (:straight, :double, :curly, :dotted, :dashed)
        weaknothing(SimpleColor), underline
    elseif underline isa Bool
        weaknothing(SimpleColor), ifelse(underline, :straight, strongnothing(Symbol))
    else
        ascolor(underline), :straight
    end
    f = FaceDef(something(font, weaknothing(String)),
              if isnothing(height)
                  weaknothing(UInt32)
              elseif height isa AbstractFloat
                  height > 0 || throw(ArgumentError("Face height factor must be positive"))
                  reinterpret(UInt32, Float32(height)) | ~(typemax(UInt32) >> 1)
              else
                  height < 0xff800000 || throw(ArgumentError("Face height in deci-pt must be less than $(0xff7fffff - 1)"))
                  UInt32(height)
              end,
              something(strikethrough, weaknothing(Bool)),
              something(inverse, weaknothing(Bool)),
              something(weight, weaknothing(Symbol)),
              something(slant, weaknothing(Symbol)),
              ascolor(foreground),
              ascolor(background),
              ul, ulstyle,
              inheritlist)
    Face(f)
end

Base.@constprop :aggressive Base.@assume_effects :foldable :nothrow :notaskstate function Base.getproperty(face::Face, attr::Symbol)
    attr == :f && return getfield(face, :f)
    val = getfield(getfield(face, :f), attr)
    if attr == :underline
        style = getfield(getfield(face, :f), :underline_style)
        if !isnothinglike(val) || !isnothinglike(style)
            if !isnothinglike(val) val end, style
        end
    elseif isnothinglike(val)
    elseif attr == :height
        if iszero(val & ~(typemax(UInt32) >> 1)) # Int
            val % Int32
        else # Float
            reinterpret(Float32, val & (typemax(UInt32) >> 1))
        end
    elseif attr ∈ (:strikethrough, :inverse)
        if val != 0x3
            val == 0x1
        end
    else
        val
    end
end

Base.propertynames(::Face) = setdiff(fieldnames(FaceDef), (:underline_style,))

function Base.:(==)(a::FaceDef, b::FaceDef)
    a.font            == b.font &&
    a.height         === b.height &&
    a.weight          == b.weight &&
    a.slant           == b.slant &&
    a.foreground      == b.foreground &&
    a.background      == b.background &&
    a.underline       == b.underline &&
    a.underline_style == b.underline_style &&
    a.strikethrough   == b.strikethrough &&
    a.inverse         == b.inverse &&
    a.inherit         == b.inherit
end

Base.:(==)(a::Face, b::Face) = a.f == b.f

Base.hash(f::FaceDef, h::UInt) =
    mapfoldr(Base.Fix1(getfield, f), hash, fieldnames(FaceDef), init=hash(FaceDef, h))

Base.hash(f::Face, h::UInt) = hash(f.f, hash(Face, h))

Base.copy(f::Face) = Face(f.f)

"""
    merge(initial::StyledStrings.Face, others::StyledStrings.Face...)

Merge the properties of the `initial` face and `others`, with later faces taking priority.

This is used to combine the styles of multiple faces, and to resolve inheritance.
"""
Base.merge(a::Face, b::Face) = merge(a.f, b.f)

Base.merge(a::Face, b::Face, others::Face...) = merge(merge(a, b), others...)

Base.merge(a::FaceDef, b::Face) = merge(a, b.f) # Comes up from 3+ arg merges

function Base.merge(a::FaceDef, b::FaceDef)
    Base.@constprop :aggressive function mergeattr(a1::FaceDef, b1::FaceDef, attr::Symbol)
        a_attr = getfield(a1, attr)
        b_attr = getfield(b1, attr)
        if isweaknothing(b_attr)
            a_attr
        elseif isstrongnothing(b_attr)
            weaknothing(b_attr)
        else
            b_attr
        end
    end
    if isempty(b.inherit)
        abheight = if isstrongnothing(b.height)
            weaknothing(b.height)
        elseif isnothinglike(b.height)
            a.height
        elseif isnothinglike(a.height)
            b.height
        elseif iszero(b.height & ~(typemax(UInt32) >> 1)) # b.height::Int
            b.height
        elseif iszero(a.height & ~(typemax(UInt32) >> 1)) # a.height::Int
            aint = reinterpret(UInt32, a.height)
            bfloat = reinterpret(Float32, b.height & (typemax(UInt32) >> 1))
            round(UInt32, aint * bfloat)
        else # a.height::Float64, b.height::Float64
            afloat = reinterpret(Float32, a.height & (typemax(UInt32) >> 1))
            bfloat = reinterpret(Float32, b.height)
            reinterpret(UInt32, afloat * bfloat)
        end
        Face(FaceDef(
            mergeattr(a, b, :font),
            abheight,
            mergeattr(a, b, :strikethrough),
            mergeattr(a, b, :inverse),
            mergeattr(a, b, :weight),
            mergeattr(a, b, :slant),
            mergeattr(a, b, :foreground),
            mergeattr(a, b, :background),
            mergeattr(a, b, :underline),
            mergeattr(a, b, :underline_style),
            a.inherit))
    else
        b_noinherit = Face(FaceDef(
            b.font, b.height, b.strikethrough, b.inverse,
            b.weight, b.slant, b.foreground, b.background,
            b.underline, b.underline_style, Face[]))
        b_inheritance = map(f -> get(FACES.current[], f, f), Iterators.reverse(b.inherit))
        b_resolved = merge(foldl(merge, b_inheritance), b_noinherit)
        merge(a, b_resolved)
    end
end

# FIXME: Temporary method
# function Base.show(io::IO, face::Face)
#     name = get(FACES.names, face, nothing)
#     if !isnothing(name)
#         print(io, "face\"", name, '"')
#         haskey(FACES.unregistered, name) && get(io, :compact, false) === true &&
#             print(io, " (unregistered)")
#         return
#     end
#     show(io, Face)
#     print(io, '(')
#     firstattr = true
#     for prop in propertynames(face)
#         val = getproperty(face, prop)
#         isnothing(val) && continue
#         prop === :inherit && isempty(val) && continue
#         firstattr || print(io, ", ")
#         print(io, prop, '=', val)
#         firstattr = false
#     end
#     print(io, ')')
# end
