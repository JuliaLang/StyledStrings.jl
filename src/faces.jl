# This file is a part of Julia. License is MIT: https://julialang.org/license

const RGBTuple = NamedTuple{(:r, :g, :b), NTuple{3, UInt8}}

# Two kinds of `nothing`. A weak nothing leaves an attribute unset. A strong nothing resets
# it: in `override` it clears the value beneath, and elsewhere it acts as unset. An attribute
# that is explicitly off holds a value, as `NO_UNDERLINE` is for the underline style.
struct WeakNothing end
struct StrongNothing end

# The named styles of the byte-encoded attributes, in byte order
const ATTRIBUTES = (
    weights = (:thin, :extralight, :light, :semilight, :normal, :medium, :semibold, :bold, :extrabold, :black),
    slants = (:italic, :oblique, :normal),
    underlines = (:straight, :double, :curly, :dotted, :dashed))

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
[`@defpalette`](@ref)) and be referred to by that name with [`face""`](@ref @face_str).

# Attributes

All attributes can be set via the keyword constructor, and default to `nothing`.

- `height` (an integer or a float): The height in either deci-pt (when an integer,
  held as an `Int32`), or as a factor of the base size (when a float, held as a `Float32`).
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
  - a `Bool`: `true` underlines the text, keeping any underline colour from
    inherited or enclosing faces. `false` removes the underline and any such
    colour.\\
  - a `SimpleColor`: The text should be underlined with this color.\\
  - a `Symbol`: The underline style alone, one of `:straight`, `:double`,
    `:curly`, `:dotted`, or `:dashed`. Any underline colour from inherited
    or enclosing faces is kept.\\
  - a `Tuple{Nothing, Symbol}`: The text should be underlined using the style
    set by the Symbol, as a plain underline with no colour.\\
  - a `Tuple{SimpleColor, Symbol}`: The text should be underlined in the specified
    SimpleColor, and using the style specified by the Symbol.
- `strikethrough` (a `Bool`): Whether the text should be struck through.
- `inverse` (a `Bool`): Whether the foreground and background colors should
  be swapped.
- `inherit` (a `Vector{Face}`, read back as a `Memory{Face}`): Faces to inherit from, with earlier faces
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

SimpleColor(namedcolor::Symbol) = convert(SimpleColor, namedcolor)

"""
    tryparse(::Type{SimpleColor}, rgb::AbstractString)

Attempt to parse `rgb` as a `SimpleColor`. A hex colour, as `#rrggbb` or `0xrrggbb`, is
converted into a `RGBTuple`-backed `SimpleColor`. A face name, which is a Julia identifier
or a dotted path of them, is converted to a [`Face`](@ref)-backed `SimpleColor`.

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
function Base.tryparse(::Type{SimpleColor}, rgb::AbstractString)
    hex = if startswith(rgb, '#') SubString(rgb, 2) elseif startswith(rgb, "0x") SubString(rgb, 3) end
    if !isnothing(hex) && ncodeunits(hex) == 6 && all(isxdigit, hex)
        SimpleColor(parse(UInt8, hex[1:2], base=16),
                    parse(UInt8, hex[3:4], base=16),
                    parse(UInt8, hex[5:6], base=16))
    elseif isnothing(hex) && all(Base.isidentifier, eachsplit(rgb, '.'))
        SimpleColor(lookmakeface(registrykey(rgb), false))
    end
end

"""
    parse(::Type{SimpleColor}, rgb::AbstractString)

An analogue of `tryparse(SimpleColor, rgb::AbstractString)` (which see),
that raises an error instead of returning `nothing`.
"""
function Base.parse(::Type{SimpleColor}, rgb::AbstractString)
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

function attrnames(attr::Symbol)
    if attr === :weight ATTRIBUTES.weights
    elseif attr === :slant ATTRIBUTES.slants
    elseif attr === :underline ATTRIBUTES.underlines
    else throw(ArgumentError("$(repr(attr)) is not a byte-encoded Face attribute"))
    end
end

# Folds to a constant for a literal `attr` and `name`, as in `attrbyte(:weight, :normal)`
Base.@assume_effects :foldable function attrbyte(attr::Symbol, name::Symbol)
    index = findfirst(==(name), attrnames(attr))
    if !isnothing(index) UInt8(index - 1) end
end

const NO_UNDERLINE = UInt8(length(ATTRIBUTES.underlines)) # The style byte of `underline = false`

# The encoding of a height in deci-pt (an integer) or as a factor (a float), if it is in range
function heightbits(height::Real)
    if height isa Integer
        if 0 <= height <= typemax(Int32) UInt32(height) end
    elseif isfinite(Float32(height)) && Float32(height) > 0
        reinterpret(UInt32, Float32(height)) | ~(typemax(UInt32) >> 1)
    end
end

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
                               Symbol, Face, RGBTuple, UInt32, AbstractString,
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
        Memory{Face}(inherit)
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
    asbyte(::Nothing, ::Symbol) = weaknothing(UInt8)
    asbyte(name::Symbol, attr::Symbol) =
        @something attrbyte(attr, name) throw(ArgumentError(
            "invalid Face $(if attr === :underline "underline style" else attr end) $(repr(name)), \
             expected one of $(join(map(repr, attrnames(attr)), ", ", " or "))"))
    ul, ulstyle = if isnothing(underline)
        WeakNothing(), weaknothing(UInt8)
    elseif underline isa Tuple{<:Any, Symbol} # nothing in a tuple is the default colour, not an unset one
        if isnothing(underline[1]) BASE_FACES.foreground else ascolor(underline[1]) end,
        asbyte(underline[2], :underline)
    elseif underline in ATTRIBUTES.underlines
        WeakNothing(), asbyte(underline, :underline)
    elseif underline isa AbstractString && !(startswith(underline, '#') || startswith(underline, "0x"))
        throw(ArgumentError("invalid Face underline $(repr(underline)), a string must be a hex colour such as \"#ff0000\""))
    elseif underline === true
        WeakNothing(), attrbyte(:underline, :straight)
    elseif underline === false # Off, and drops any inherited colour
        BASE_FACES.foreground, NO_UNDERLINE
    else
        ascolor(underline), attrbyte(:underline, :straight)
    end
    height1 = if isnothing(height)
        weaknothing(UInt32)
    else
        @something heightbits(height) throw(ArgumentError(
            "Face height must be deci-pt from 0 to $(typemax(Int32)), or a positive finite Float32 factor"))
    end
    f = FaceDef(something(font, WeakNothing()),
                ascolor(foreground),
                ascolor(background),
                ul,
                height1,
                asbyte(weight, :weight),
                asbyte(slant, :slant),
                ulstyle,
                something(strikethrough, weaknothing(Bool)),
                something(inverse, weaknothing(Bool)),
                inheritlist)
    Face(f)
end

Base.@constprop :aggressive Base.@assume_effects :foldable :notaskstate Base.getproperty(face::Face, attr::Symbol) =
    if attr == :f getfield(face, :f) else faceproperty(getfield(face, :f), attr) end

# The `attr` of `def`, in the form that the `Face` constructor takes
Base.@constprop :aggressive Base.@assume_effects :foldable :notaskstate function faceproperty(def::FaceDef, attr::Symbol)
    val = getfield(def, attr)
    if attr == :underline
        style = getfield(def, :underline_style)
        if style == NO_UNDERLINE
            false
        elseif isnothingflavour(style)
            if !isnothingflavour(val) && val !== BASE_FACES.foreground; (SimpleColor(val), nothing) end
        elseif isnothingflavour(val)
            if style == attrbyte(:underline, :straight) true else ATTRIBUTES.underlines[style + 1] end
        else # A colour, where the default foreground stands for the `nothing` of a tuple
            (if val !== BASE_FACES.foreground SimpleColor(val) end, ATTRIBUTES.underlines[style + 1])
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
        ATTRIBUTES.weights[val + 1]
    elseif attr == :slant
        ATTRIBUTES.slants[val + 1]
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
    # Colours hash by identity, as they compare; hashing by structure would recurse through the base faces
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

An attribute that a later face leaves unset keeps its earlier value. Integer heights
replace, and float heights scale. Other attributes merge associatively.
"""
Base.merge(a::Face, b::Face) = Face(merge(a.f, b.f))

Base.merge(a::Face, b::Face, others::Face...) = merge(merge(a, b), others...)

function Base.merge(a::FaceDef, b::FaceDef)
    mergeattr(va, vb) = if isnothingflavour(vb) va else vb end
    if isempty(b.inherit)
        abheight = if isnothingflavour(b.height)
            a.height
        elseif isnothingflavour(a.height)
            b.height
        elseif iszero(b.height & ~(typemax(UInt32) >> 1)) # b.height::Int
            b.height
        elseif iszero(a.height & ~(typemax(UInt32) >> 1)) # a.height::Int
            aint = reinterpret(UInt32, a.height)
            bfloat = reinterpret(Float32, b.height & (typemax(UInt32) >> 1))
            round(UInt32, min(aint * Float64(bfloat), typemax(Int32))) # Larger would set the float tag bit
        else # a.height::Float32, b.height::Float32
            afloat = reinterpret(Float32, a.height & (typemax(UInt32) >> 1))
            bfloat = reinterpret(Float32, b.height)
            reinterpret(UInt32, max(afloat * bfloat, -floatmax(Float32))) # Negative from `b`'s tag bit; -Inf is unset
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
        # A loop rather than a fold: passing `merge` to `mapfoldl` makes the recursion
        # uninferrable, which breaks trimming.
        inherited = EMPTY_FACE.f
        for face in Iterators.reverse(b.inherit)
            inherited = merge(inherited, get(FACES.current[], face, face).f)
        end
        merge(a, merge(inherited, b_noinherit))
    end
end
