# This file is a part of Julia. License is MIT: https://julialang.org/license

const RGBTuple = NamedTuple{(:r, :g, :b), NTuple{3, UInt8}}

# Splitting `nothing` in two to allow for three-valued logic
struct WeakNothing end
struct StrongNothing end

# For efficient byte-shaped encoding of weight/slant/underline style
const WEIGHT_NAMES = (:thin, :extralight, :light, :semilight, :normal, :medium, :semibold, :bold, :extrabold, :black)
const SLANT_NAMES = (:italic, :oblique, :normal)
const UNDERLINE_STYLE_NAMES = (:straight, :double, :curly, :dotted, :dashed)

# A precursor to `Face` that parameterises `_F` to break the otherwise circular dependency.
# The field order packs it into 56 bytes, one 64-byte `Face` allocation.
# NOTE: The fact that 4-value unions are split by the compiler is critical to the efficient
# layout of this struct, and performance of related code.
struct _FaceDef{_F}
    font::Union{String, WeakNothing, StrongNothing}
    foreground::Union{_F, RGBTuple, WeakNothing, StrongNothing}
    background::Union{_F, RGBTuple, WeakNothing, StrongNothing}
    underline::Union{_F, RGBTuple, WeakNothing, StrongNothing}
    height::UInt32
    weight::UInt8
    slant::UInt8
    underline_style::UInt8
    strikethrough::UInt8
    inverse::UInt8
    inherit::Memory{_F}
end

# We want to intern `Face`s at runtime, and be able to
# compare them by pointer equality for speed within
# hot code paths. For these reasons, we have defined
# `_FaceDef` to hold the /actual/ data, and `Face` as
# a 'mutable' wrapper around it. The only reason why
# the sole field of `Face` is not a `const` is because
# we are unable to create circular references otherwise
# (which are useful for defining special base faces).
mutable struct Face
    f::_FaceDef{Face}
    Face(f::_FaceDef{Face}) = new(f)
    global uninitialised_face() = new() # Just for `new_recursive_fg_face`
end

Base.setproperty!(::Face, ::Symbol, ::Any) = throw(ArgumentError("Faces are immutable"))

const FaceDef = _FaceDef{Face}

struct SimpleColor
    value::Union{Face, RGBTuple}
end

@doc """
A [`Face`](@ref) is a collection of graphical attributes for displaying text.
Faces control how text is displayed in the terminal, and possibly other
places too.

Most of the time, a [`Face`](@ref) will be given a name in a palette (see
[`@defpalette!`](@ref)) and be referred to by that name with [`face""`](@ref @face_str).

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
- `inverse` (a `Bool`): Whether the foreground and background colors should
  be swapped.
- `inherit` (a `Vector{Face}`): Faces to inherit from, with earlier faces
  taking priority. All faces inherit from the `default` face.

# Examples

```
julia> Face(foreground = face"red", weight = :bold, underline=true)
Face (sample)
        weight: bold
    foreground: ■ red
     underline: true

julia> Face(slant = :italic, inherit = face"emphasis")
Face (sample)
         slant: italic
       inherit: emphasis(*)
```
""" Face

@doc """
    struct SimpleColor

A basic representation of a color, intended for string styling purposes.
It can either contain a named color (like `:red`), or an `RGBTuple` which
is a NamedTuple specifying an `r`, `g`, `b` color with a bit-depth of 8.

# Constructors

```julia
SimpleColor(face::Face)    # e.g. face"red"
SimpleColor(r::Integer, g::Integer, b::Integer) # 0-255
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
    # Base.depwarn("Creating a SimpleColor from a face name Symbol is deprecated as of v1.14. Use faces directly instead, such as from `face\"colourname\"`", :convert)
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
        all(isxdigit, SubString(rgb, 2))
        SimpleColor(parse(UInt8, rgb[2:3], base=16),
                    parse(UInt8, rgb[4:5], base=16),
                    parse(UInt8, rgb[6:7], base=16))
    elseif !isempty(rgb) && ('a' <= rgb[1] <= 'z' || 'A' <= rgb[1] <= 'Z')
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

weaknothing(::Type{N}) where {N <: Unsigned} = -(0x2 * one(N))
weaknothing(::Type{Bool}) = weaknothing(UInt8)
weaknothing(::Type) = WeakNothing()
weaknothing(x) = weaknothing(typeof(x))

isweaknothing(::WeakNothing) = true
isweaknothing(u::Unsigned) = u == weaknothing(typeof(u))
isweaknothing(u::UInt32) = isnothingflavour(u) && !isstrongnothing(u)
isweaknothing(::Any) = false

strongnothing(::Type{N}) where {N <: Unsigned} = -one(N)
strongnothing(::Type{Bool}) = strongnothing(UInt8)
strongnothing(::Type) = StrongNothing()
strongnothing(x) = strongnothing(typeof(x))

isstrongnothing(::StrongNothing) = true
isstrongnothing(u::Unsigned) = u == strongnothing(typeof(u))
isstrongnothing(::Any) = false

isnothingflavour(x) = isweaknothing(x) || isstrongnothing(x)
isnothingflavour(u::UInt32) = u & 0xff800000 == 0xff800000

function attrbyte(names::Tuple{Vararg{Symbol}}, name::Symbol)
    index = findfirst(==(name), names)
    if !isnothing(index) UInt8(index - 1) end
end

const NORMAL_WEIGHT = attrbyte(WEIGHT_NAMES, :normal)
const NORMAL_SLANT = attrbyte(SLANT_NAMES, :normal)
const STRAIGHT_UNDERLINE = attrbyte(UNDERLINE_STYLE_NAMES, :straight)

const NO_INHERIT = Memory{Face}()
const EMPTY_FACE = Face(FaceDef(
        WeakNothing(), WeakNothing(), WeakNothing(), WeakNothing(), # font, foreground, background, underline
        weaknothing(UInt32), # height
        weaknothing(UInt8), weaknothing(UInt8), weaknothing(UInt8), # weight, slant, underline_style
        weaknothing(UInt8), weaknothing(UInt8), # strikethrough, inverse
        NO_INHERIT))

function new_recursive_fg_face()
    f = uninitialised_face() # We can't reference `f` before creation
    setfield!(f, :f, FaceDef(
        WeakNothing(), f, WeakNothing(), WeakNothing(), # <- self-reference here (fg)
        weaknothing(UInt32),
        weaknothing(UInt8), weaknothing(UInt8), weaknothing(UInt8),
        weaknothing(UInt8), weaknothing(UInt8),
        Memory{Face}()))
    f
end

"""
    BASE_FACES

A collection of special faces that represent the basic terminal colors.
These are special in the sense that, unlike all other faces, their colour
may not be known. They *do* have a colour though, and for handling this
situation, it is easiest if these faces' foreground colour is a (circular)
reference to the face itself ("red is red" rather than "red is #ff0000"
or "red is nothing").
"""
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

# With our flavours of nothing defined, we can now define the public Face constructor.

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
        NO_INHERIT
    elseif inherit isa Vector{Face}
        inherit.ref.mem
    elseif inherit isa Face
        mem = Memory{Face}(undef, 1)
        mem[1] = inherit
        mem
    elseif inherit isa Vector{Symbol} # Backwards compat (1)
        # Base.depwarn("Using symbols to refer to faces is deprecated as of v1.14. Reference faces directly with `face\"\"` instead.", :Face)
        [lookmakeface(fname) for fname in inherit].ref.mem
    elseif inherit isa Symbol # Backwards compat (2)
        # Base.depwarn("Using symbols to refer to faces is deprecated as of v1.14. Reference faces directly with `face\"\"` instead.", :Face)
        mem = Memory{Face}(undef, 1)
        mem[1] = lookmakeface(inherit)
        mem
    end
    ascolor(::Nothing) = WeakNothing()
    ascolor(c::Union{Face, RGBTuple}) = c
    ascolor(c::AbstractString) = parse(SimpleColor, c).value
    ascolor(c::Any) = convert(SimpleColor, c).value
    asbyte(::Nothing, ::Tuple{Vararg{Symbol}}, ::String) = weaknothing(UInt8)
    asbyte(name::Symbol, names::Tuple{Vararg{Symbol}}, attr::String) =
        @something attrbyte(names, name) throw(ArgumentError(
            "invalid Face $attr $(repr(name)), expected one of $(join(map(repr, names), ", ", " or "))"))
    ul, ulstyle = if isnothing(underline)
        WeakNothing(), weaknothing(UInt8)
    elseif underline isa Tuple{<:Any, Symbol}
        ascolor(underline[1]), asbyte(underline[2], UNDERLINE_STYLE_NAMES, "underline style")
    elseif underline in UNDERLINE_STYLE_NAMES
        WeakNothing(), asbyte(underline, UNDERLINE_STYLE_NAMES, "underline style")
    elseif underline isa Bool
        WeakNothing(), ifelse(underline, STRAIGHT_UNDERLINE, strongnothing(UInt8))
    else
        ascolor(underline), STRAIGHT_UNDERLINE
    end
    height1 = if isnothing(height)
        weaknothing(UInt32)
    elseif height isa AbstractFloat
        height > 0 || throw(ArgumentError("Face height factor must be positive"))
        reinterpret(UInt32, Float32(height)) | ~(typemax(UInt32) >> 1)
    else
        height < 0xff800000 || throw(ArgumentError("Face height in deci-pt must be less than $(0xff7fffff - 1)"))
        UInt32(height)
    end
    f = FaceDef(something(font, WeakNothing()),
                ascolor(foreground),
                ascolor(background),
                ul,
                height1,
                asbyte(weight, WEIGHT_NAMES, "weight"),
                asbyte(slant, SLANT_NAMES, "slant"),
                ulstyle,
                something(strikethrough, weaknothing(Bool)),
                something(inverse, weaknothing(Bool)),
                inheritlist)
    Face(f)
end

Base.@constprop :aggressive Base.@assume_effects :foldable :notaskstate function Base.getproperty(face::Face, attr::Symbol)
    attr == :f && return getfield(face, :f)
    val = getfield(getfield(face, :f), attr)
    if attr == :underline
        style = getfield(getfield(face, :f), :underline_style)
        if !isnothingflavour(val) || !isnothingflavour(style)
            (if !isnothingflavour(val) SimpleColor(val) end,
             if !isnothingflavour(style) UNDERLINE_STYLE_NAMES[style + 1] end)
        end
    elseif isnothingflavour(val)
        nothing
    elseif attr == :height
        if iszero(val & ~(typemax(UInt32) >> 1)) # Int
            val % Int32
        else # Float
            reinterpret(Float32, val & (typemax(UInt32) >> 1))
        end
    elseif attr ∈ (:foreground, :background)
        SimpleColor(val)
    elseif attr == :weight
        WEIGHT_NAMES[val + 1]
    elseif attr == :slant
        SLANT_NAMES[val + 1]
    elseif attr ∈ (:strikethrough, :inverse)
        val == 0x1
    else
        val
    end
end

Base.propertynames(::Face) =
    (:font, :height, :strikethrough, :inverse, :weight, :slant, :foreground, :background, :underline, :inherit)

function Base.:(==)(a::FaceDef, b::FaceDef)
    a.font            === b.font &&
    a.foreground      === b.foreground &&
    a.background      === b.background &&
    a.underline       === b.underline &&
    a.height          === b.height &&
    a.weight          === b.weight &&
    a.slant           === b.slant &&
    a.underline_style === b.underline_style &&
    a.strikethrough   === b.strikethrough &&
    a.inverse         === b.inverse &&
    a.inherit          == b.inherit
end

Base.:(==)(a::Face, b::Face) = a.f == b.f

function Base.hash(f::FaceDef, h::UInt)
    # Colours hash by identity, as they compare; structurally, the base faces would recurse
    h = hash(f.font, hash(objectid(f.foreground), hash(objectid(f.background), hash(objectid(f.underline), hash(FaceDef, h)))))
    h = hash(f.height, hash(f.weight, hash(f.slant, hash(f.underline_style, h))))
    h = hash(f.strikethrough, hash(f.inverse, h))
    foldl((h, face) -> hash(face, h), f.inherit; init = h)
end

Base.hash(f::Face, h::UInt) = hash(f.f, hash(Face, h))

Base.copy(f::Face) = Face(f.f)

"""
    merge(initial::StyledStrings.Face, others::StyledStrings.Face...)

Merge the properties of the `initial` face and `others`, with later faces taking priority.

This is used to combine the styles of multiple faces, and to resolve inheritance.

A weak nothing in a later face keeps the earlier value; a strong nothing (as from
`underline = false`) replaces it and persists. Integer heights replace, float
heights scale. `merge` is thus idempotent and associative, with `Face()` as identity.
"""
Base.merge(a::Face, b::Face) = Face(merge(a.f, b.f))

Base.merge(a::Face, b::Face, others::Face...) = merge(merge(a, b), others...)

function Base.merge(a::FaceDef, b::FaceDef)
    mergeattr(va, vb) = if isweaknothing(vb) va else vb end
    if isempty(b.inherit)
        abheight = if isweaknothing(b.height)
            a.height
        elseif isnothingflavour(a.height) || isstrongnothing(b.height)
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
        FaceDef(mergeattr(a.font, b.font),
                mergeattr(a.foreground, b.foreground),
                mergeattr(a.background, b.background),
                mergeattr(a.underline, b.underline),
                abheight,
                mergeattr(a.weight, b.weight),
                mergeattr(a.slant, b.slant),
                mergeattr(a.underline_style, b.underline_style),
                mergeattr(a.strikethrough, b.strikethrough),
                mergeattr(a.inverse, b.inverse),
                a.inherit)
    else
        b_noinherit = FaceDef(
            b.font, b.foreground, b.background, b.underline, b.height,
            b.weight, b.slant, b.underline_style, b.strikethrough, b.inverse, Face[])
        # A plain loop, not a fold: passing `merge` to a higher-order function makes the
        # recursion through it uninferrable, which breaks trimming.
        inherited = EMPTY_FACE.f
        for face in Iterators.reverse(b.inherit)
            inherited = merge(inherited, get(FACES.current[], face, face).f)
        end
        merge(a, merge(inherited, b_noinherit))
    end
end
