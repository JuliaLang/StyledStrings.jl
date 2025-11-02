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
struct Face
    font::Union{Nothing, String}
    height::Union{Nothing, Int, Float64}
    weight::Union{Nothing, Symbol}
    slant::Union{Nothing, Symbol}
    foreground::Union{Nothing, SimpleColor}
    background::Union{Nothing, SimpleColor}
    underline::Union{Nothing, Bool, SimpleColor,
                     Tuple{<:Union{Nothing, SimpleColor}, Symbol}}
    strikethrough::Union{Nothing, Bool}
    inverse::Union{Nothing, Bool}
    inherit::Vector{Symbol}
end

function Face(; font::Union{Nothing, String} = nothing,
              height::Union{Nothing, Int, Float64} = nothing,
              weight::Union{Nothing, Symbol} = nothing,
              slant::Union{Nothing, Symbol} = nothing,
              foreground = nothing, # nothing, or SimpleColor-able value
              background = nothing, # nothing, or SimpleColor-able value
              underline::Union{Nothing, Bool, SimpleColor,
                               Symbol, RGBTuple, UInt32,
                               Tuple{<:Any, Symbol}
                               } = nothing,
              strikethrough::Union{Nothing, Bool} = nothing,
              inverse::Union{Nothing, Bool} = nothing,
              inherit::Union{Symbol, Vector{Symbol}} = Symbol[],
              _...) # Simply ignore unrecognised keyword arguments.
    ascolor(::Nothing) = nothing
    ascolor(c::AbstractString) = parse(SimpleColor, c)
    ascolor(c::Any) = convert(SimpleColor, c)
    Face(font, height, weight, slant,
         ascolor(foreground), ascolor(background),
         if underline isa Tuple{Any, Symbol}
             (ascolor(underline[1]), underline[2])
         elseif underline in (:straight, :double, :curly, :dotted, :dashed)
             (nothing, underline)
         elseif underline isa Bool
             underline
         else
             ascolor(underline)
         end,
         strikethrough,
         inverse,
         if inherit isa Symbol
             [inherit]
         else inherit end)
end

function Base.:(==)(a::Face, b::Face)
    a.font == b.font &&
    a.height === b.height &&
    a.weight == b.weight &&
    a.slant == b.slant &&
    a.foreground == b.foreground &&
    a.background == b.background &&
    a.underline == b.underline &&
    a.strikethrough == b.strikethrough &&
    a.inverse == b.inverse &&
    a.inherit == b.inherit
end

Base.hash(f::Face, h::UInt) =
    mapfoldr(Base.Fix1(getfield, f), hash, fieldnames(Face), init=hash(Face, h))

Base.copy(f::Face) =
    Face(f.font, f.height, f.weight, f.slant,
         f.foreground, f.background, f.underline,
         f.strikethrough, f.inverse, copy(f.inherit))

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

"""
Globally named [`Face`](@ref)s.

`default` gives the initial values of the faces, and `current` holds the active
(potentially modified) set of faces. This two-set system allows for any
modifications to the active faces to be undone.
"""
const FACES = let base = Dict{Symbol, Face}(
    # Base is special, it must be completely specified
    # and everything inherits from it.
    :default => Face(
        "monospace", 120,      # font, height
        :normal, :normal,      # weight, slant
        SimpleColor(:foreground), # foreground
        SimpleColor(:background), # background
        false, false, false,   # underline, strikethrough, overline
        Symbol[]),              # inherit
    # Property faces
    :bold => Face(weight=:bold),
    :light => Face(weight=:light),
    :italic => Face(slant=:italic),
    :underline => Face(underline=true),
    :strikethrough => Face(strikethrough=true),
    :inverse => Face(inverse=true),
    # Basic color faces
    :black => Face(foreground=:black),
    :red => Face(foreground=:red),
    :green => Face(foreground=:green),
    :yellow => Face(foreground=:yellow),
    :blue => Face(foreground=:blue),
    :magenta => Face(foreground=:magenta),
    :cyan => Face(foreground=:cyan),
    :white => Face(foreground=:white),
    :bright_black => Face(foreground=:bright_black),
    :grey => Face(foreground=:bright_black),
    :gray => Face(foreground=:bright_black),
    :bright_red => Face(foreground=:bright_red),
    :bright_green => Face(foreground=:bright_green),
    :bright_yellow => Face(foreground=:bright_yellow),
    :bright_blue => Face(foreground=:bright_blue),
    :bright_magenta => Face(foreground=:bright_magenta),
    :bright_cyan => Face(foreground=:bright_cyan),
    :bright_white => Face(foreground=:bright_white),
    # Useful common faces
    :shadow => Face(foreground=:bright_black),
    :region => Face(background=0x636363),
    :emphasis => Face(foreground=:blue),
    :highlight => Face(inherit=:emphasis, inverse=true),
    :code => Face(foreground=:cyan),
    # Styles of generic content categories
    :error => Face(foreground=:bright_red),
    :warning => Face(foreground=:yellow),
    :success => Face(foreground=:green),
    :info => Face(foreground=:bright_cyan),
    :note => Face(foreground=:grey),
    :tip => Face(foreground=:bright_green),
    # Stacktraces (on behalf of Base)
    :julia_stacktrace_frameindex => Face(),
    :julia_stacktrace_location => Face(inherit=:shadow),
    :julia_stacktrace_filename => Face(underline=true, inherit=:julia_stacktrace_location),
    :julia_stacktrace_fileline => Face(inherit=:julia_stacktrace_filename),
    :julia_stacktrace_repetition => Face(inherit=:warning),
    :julia_stacktrace_inlined => Face(inherit=:julia_stacktrace_repetition),
    :julia_stacktrace_basemodule => Face(inherit=:shadow),
    # Log messages
    :log_error => Face(inherit=[:error, :bold]),
    :log_warn => Face(inherit=[:warning, :bold]),
    :log_info => Face(inherit=[:info, :bold]),
    :log_debug => Face(foreground=:blue, inherit=:bold),
    # Julia prompts
    :repl_prompt => Face(weight=:bold),
    :repl_prompt_julia => Face(inherit=[:green, :repl_prompt]),
    :repl_prompt_help => Face(inherit=[:yellow, :repl_prompt]),
    :repl_prompt_shell => Face(inherit=[:red, :repl_prompt]),
    :repl_prompt_pkg => Face(inherit=[:blue, :repl_prompt]),
    :repl_prompt_beep => Face(inherit=[:shadow, :repl_prompt]),
    )
    light = Dict{Symbol, Face}(
        :region => Face(background=0xaaaaaa),
    )
    dark = Dict{Symbol, Face}(
        :region => Face(background=0x363636),
    )
    basecolors = Dict{Symbol, RGBTuple}(
        :background     => (r = 0xff, g = 0xff, b = 0xff),
        :foreground     => (r = 0x00, g = 0x00, b = 0x00),
        :black          => (r = 0x1c, g = 0x1a, b = 0x23),
        :red            => (r = 0xa5, g = 0x1c, b = 0x2c),
        :green          => (r = 0x25, g = 0xa2, b = 0x68),
        :yellow         => (r = 0xe5, g = 0xa5, b = 0x09),
        :blue           => (r = 0x19, g = 0x5e, b = 0xb3),
        :magenta        => (r = 0x80, g = 0x3d, b = 0x9b),
        :cyan           => (r = 0x00, g = 0x97, b = 0xa7),
        :white          => (r = 0xdd, g = 0xdc, b = 0xd9),
        :bright_black   => (r = 0x76, g = 0x75, b = 0x7a),
        :bright_red     => (r = 0xed, g = 0x33, b = 0x3b),
        :bright_green   => (r = 0x33, g = 0xd0, b = 0x79),
        :bright_yellow  => (r = 0xf6, g = 0xd2, b = 0x2c),
        :bright_blue    => (r = 0x35, g = 0x83, b = 0xe4),
        :bright_magenta => (r = 0xbf, g = 0x60, b = 0xca),
        :bright_cyan    => (r = 0x26, g = 0xc6, b = 0xda),
        :bright_white   => (r = 0xf6, g = 0xf5, b = 0xf4))
    (themes = (; base, light, dark),
     modifications = (base = Dict{Symbol, Face}(), light = Dict{Symbol, Face}(), dark = Dict{Symbol, Face}()),
     current = ScopedValue(copy(base)),
     basecolors = basecolors,
     lock = ReentrantLock())
end

## Adding and resetting faces ##

"""
    addface!(name::Symbol => default::Face, theme::Symbol = :base)

Create a new face by the name `name`. So long as no face already exists by this
name, `default` is added to both `FACES.themes[theme]` and (a copy of) to
`FACES.current`, with the current value returned.

The `theme` should be either `:base`, `:light`, or `:dark`.

Should the face `name` already exist, `nothing` is returned.

# Examples

```jldoctest; setup = :(import StyledStrings: Face, addface!)
julia> addface!(:mypkg_myface => Face(slant=:italic, underline=true))
Face (sample)
         slant: italic
     underline: true
```
"""
function addface!((name, default)::Pair{Symbol, Face}, theme::Symbol = :base)
    current = FACES.current[]
    @lock FACES.lock if !haskey(FACES.themes[theme], name)
        FACES.themes[theme][name] = default
        current[name] = if haskey(current, name)
            merge(copy(default), current[name])
        else
            copy(default)
        end
    end
end

"""
    resetfaces!()

Reset the current global face dictionary to the default value.
"""
function resetfaces!()
    @lock FACES.lock begin
        current = FACES.current[]
        empty!(current)
        for (key, val) in FACES.themes.base
            current[key] = val
        end
        if current === FACES.current.default # Only when top-level
            map(empty!, values(FACES.modifications))
        end
        current
    end
end

"""
    resetfaces!(name::Symbol)

Reset the face `name` to its default value, which is returned.

If the face `name` does not exist, nothing is done and `nothing` returned.
In the unlikely event that the face `name` does not have a default value,
it is deleted, a warning message is printed, and `nothing` returned.
"""
function resetfaces!(name::Symbol, theme::Symbol = :base)
    current = FACES.current[]
    @lock FACES.lock if !haskey(current, name) # Nothing to reset
    elseif haskey(FACES.themes[theme], name)
        current === FACES.current.default &&
            delete!(FACES.modifications[theme], name)
        current[name] = copy(FACES.themes[theme][name])
    else # This shouldn't happen
        delete!(current, name)
        @warn """The face $name was reset, but it had no default value, and so has been deleted instead!,
                 This should not have happened, perhaps the face was added without using `addface!`?"""
    end
end

"""
    withfaces(f, kv::Pair...)
    withfaces(f, kvpair_itr)

Execute `f` with `FACES``.current` temporarily modified by zero or more `:name
=> val` arguments `kv`, or `kvpair_itr` which produces `kv`-form values.

`withfaces` is generally used via the `withfaces(kv...) do ... end` syntax. A
value of `nothing` can be used to temporarily unset a face (if it has been
set). When `withfaces` returns, the original `FACES``.current` has been
restored.

# Examples

```jldoctest; setup = :(import StyledStrings: Face, withfaces)
julia> withfaces(:yellow => Face(foreground=:red), :green => :blue) do
           println(styled"{yellow:red} and {green:blue} mixed make {magenta:purple}")
       end
red and blue mixed make purple
```
"""
function withfaces(f, keyvals_itr)
    # Before modifying the current `FACES`, we should ensure
    # that we've loaded the user's customisations.
    load_customisations!()
    if !(eltype(keyvals_itr) <: Pair{Symbol})
        throw(MethodError(withfaces, (f, keyvals_itr)))
    end
    newfaces = copy(FACES.current[])
    for (name, face) in keyvals_itr
        if face isa Face
            newfaces[name] = face
        elseif face isa Symbol
            newfaces[name] = get(Face, FACES.current[], face)
        elseif face isa Vector{Symbol}
            newfaces[name] = Face(inherit=face)
        elseif haskey(newfaces, name)
            delete!(newfaces, name)
        end
    end
    @with(FACES.current => newfaces, f())
end

withfaces(f, keyvals::Pair{Symbol, <:Union{Face, Symbol, Vector{Symbol}, Nothing}}...) =
    withfaces(f, keyvals)

withfaces(f) = f()

## Face combination and inheritance ##

"""
    merge(initial::StyledStrings.Face, others::StyledStrings.Face...)

Merge the properties of the `initial` face and `others`, with later faces taking priority.

This is used to combine the styles of multiple faces, and to resolve inheritance.
"""
function Base.merge(a::Face, b::Face)
    if isempty(b.inherit)
        # Extract the heights to help type inference a bit to be able
        # to narrow the types in e.g. `aheight * bheight`
        aheight = a.height
        bheight = b.height
        abheight = if isnothing(bheight) aheight
        elseif isnothing(aheight) bheight
        elseif bheight isa Int bheight
        elseif aheight isa Int round(Int, aheight * bheight)
        else aheight * bheight end
        Face(if isnothing(b.font)          a.font          else b.font          end,
             abheight,
             if isnothing(b.weight)        a.weight        else b.weight        end,
             if isnothing(b.slant)         a.slant         else b.slant         end,
             if isnothing(b.foreground)    a.foreground    else b.foreground    end,
             if isnothing(b.background)    a.background    else b.background    end,
             if isnothing(b.underline)     a.underline     else b.underline     end,
             if isnothing(b.strikethrough) a.strikethrough else b.strikethrough end,
             if isnothing(b.inverse)       a.inverse       else b.inverse       end,
             a.inherit)
    else
        b_noinherit = Face(
            b.font, b.height, b.weight, b.slant, b.foreground, b.background,
            b.underline, b.strikethrough, b.inverse, Symbol[])
        b_inheritance = map(fname -> get(Face, FACES.current[], fname), Iterators.reverse(b.inherit))
        b_resolved = merge(foldl(merge, b_inheritance), b_noinherit)
        merge(a, b_resolved)
    end
end

Base.merge(a::Face, b::Face, others::Face...) = merge(merge(a, b), others...)

## Getting the combined face from a set of properties ##

# Putting these inside `getface` causes the julia compiler to box it
_mergedface(face::Face) = face
_mergedface(face::Symbol) = get(Face, FACES.current[], face)
_mergedface(faces::Vector) = mapfoldl(_mergedface, merge, Iterators.reverse(faces))

"""
    getface(faces)

Obtain the final merged face from `faces`, an iterator of
[`Face`](@ref)s, face name `Symbol`s, and lists thereof.
"""
function getface(faces)
    isempty(faces) && return FACES.current[][:default]
    combined = mapfoldl(_mergedface, merge, faces)::Face
    if !isempty(combined.inherit)
        combined = merge(Face(), combined)
    end
    merge(FACES.current[][:default], combined)
end

"""
    getface(annotations::Vector{@NamedTuple{label::Symbol, value::Any}})

Combine all of the `:face` annotations with `getfaces`.
"""
function getface(annotations::Vector{@NamedTuple{label::Symbol, value::Any}})
    faces = (ann.value for ann in annotations if ann.label === :face)
    getface(faces)
end

getface(face::Face) = merge(FACES.current[][:default], merge(Face(), face))
getface(face::Symbol) = getface(get(Face, FACES.current[], face))

"""
    getface()

Obtain the default face.
"""
getface() = FACES.current[][:default]

## Face/AnnotatedString integration ##

"""
    getface(s::AnnotatedString, i::Integer)

Get the merged [`Face`](@ref) that applies to `s` at index `i`.
"""
getface(s::AnnotatedString, i::Integer) =
    getface(map(last, annotations(s, i)))

"""
    getface(c::AnnotatedChar)

Get the merged [`Face`](@ref) that applies to `c`.
"""
getface(c::AnnotatedChar) = getface(c.annotations)

"""
    face!(str::Union{<:AnnotatedString, <:SubString{<:AnnotatedString}},
          [range::UnitRange{Int},] face::Union{Symbol, Face})

Apply `face` to `str`, along `range` if specified or the whole of `str`.
"""
face!(s::Union{<:AnnotatedString, <:SubString{<:AnnotatedString}},
      range::UnitRange{Int}, face::Union{Symbol, Face, <:Vector{<:Union{Symbol, Face}}}) =
          annotate!(s, range, :face, face)

face!(s::Union{<:AnnotatedString, <:SubString{<:AnnotatedString}},
      face::Union{Symbol, Face, <:Vector{<:Union{Symbol, Face}}}) =
          annotate!(s, firstindex(s):lastindex(s), :face, face)

## Reading face definitions from a dictionary ##

"""
    loadface!(name::Symbol => update::Face)

Merge the face `name` in `FACES``.current` with `update`. If the face `name`
does not already exist in `FACES``.current`, then it is set to `update`. To
reset a face, `update` can be set to `nothing`.

# Examples

```jldoctest; setup = :(import StyledStrings: Face, loadface!)
julia> loadface!(:red => Face(foreground=0xff0000))
Face (sample)
    foreground: #ff0000
```
"""
function loadface!((name, update)::Pair{Symbol, Face}, theme::Symbol = :base)
    @lock FACES.lock begin
        current = FACES.current[]
        if FACES.current.default === current # Only save top-level modifications
            mface = get(FACES.modifications[theme], name, nothing)
            isnothing(mface) || (update = merge(mface, update))
            FACES.modifications[theme][name] = update
        end
        cface = get(current, name, nothing)
        isnothing(cface) || (update = merge(cface, update))
        current[name] = update
    end
end

function loadface!((name, _)::Pair{Symbol, Nothing})
    if haskey(FACES.current[], name)
        resetfaces!(name)
    end
end

"""
    loaduserfaces!(faces::Dict{String, Any})

For each face specified in `Dict`, load it to `FACES``.current`.
"""
function loaduserfaces!(faces::Dict{String, Any}, prefix::Union{String, Nothing}=nothing, theme::Symbol = :base)
    theme == :base && prefix ∈ map(String, setdiff(keys(FACES.themes), (:base,))) &&
        return loaduserfaces!(faces, nothing, Symbol(prefix))
    for (name, spec) in faces
        fullname = if isnothing(prefix)
            name
        else
            string(prefix, '_', name)
        end
        fspec = filter((_, v)::Pair -> !(v isa Dict), spec)
        fnest = filter((_, v)::Pair -> v isa Dict, spec)
        !isempty(fspec) &&
            loadface!(Symbol(fullname) => convert(Face, fspec), theme)
        !isempty(fnest) &&
            loaduserfaces!(fnest, fullname, theme)
    end
end

"""
    loaduserfaces!(tomlfile::String)

Load all faces declared in the Faces.toml file `tomlfile`.
"""
loaduserfaces!(tomlfile::String) = loaduserfaces!(Base.parsed_toml(tomlfile))

function Base.convert(::Type{Face}, spec::Dict{String,Any})
    Face(if haskey(spec, "font") && spec["font"] isa String
             spec["font"]::String
         end,
         if haskey(spec, "height") && (spec["height"] isa Int || spec["height"] isa Float64)
             spec["height"]::Union{Int,Float64}
         end,
         if haskey(spec, "weight") && spec["weight"] isa String
             Symbol(spec["weight"]::String)
         elseif haskey(spec, "bold") && spec["bold"] isa Bool
             ifelse(spec["bold"]::Bool, :bold, :normal)
         end,
         if haskey(spec, "slant") && spec["slant"] isa String
             Symbol(spec["slant"]::String)
         elseif haskey(spec, "italic") && spec["italic"] isa Bool
             ifelse(spec["italic"]::Bool, :italic, :normal)
         end,
         if haskey(spec, "foreground") && spec["foreground"] isa String
             tryparse(SimpleColor, spec["foreground"]::String)
         elseif haskey(spec, "fg") && spec["fg"] isa String
             tryparse(SimpleColor, spec["fg"]::String)
         end,
         if haskey(spec, "background") && spec["background"] isa String
             tryparse(SimpleColor, spec["background"]::String)
         elseif haskey(spec, "bg") && spec["bg"] isa String
             tryparse(SimpleColor, spec["bg"]::String)
         end,
         if !haskey(spec, "underline")
         elseif spec["underline"] isa Bool
             spec["underline"]::Bool
         elseif spec["underline"] isa String
             tryparse(SimpleColor, spec["underline"]::String)
         elseif spec["underline"] isa Vector{String} && length(spec["underline"]::Vector{String}) == 2
             color_str, style_str = (spec["underline"]::Vector{String})
             color = tryparse(SimpleColor, color_str)
             (color, Symbol(style_str))
         end,
         if !haskey(spec, "strikethrough")
         elseif spec["strikethrough"] isa Bool
             spec["strikethrough"]::Bool
         elseif spec["strikethrough"] isa String
             tryparse(SimpleColor, spec["strikethrough"]::String)
         end,
         if haskey(spec, "inverse") && spec["inverse"] isa Bool
             spec["inverse"]::Bool end,
         if !haskey(spec, "inherit")
             Symbol[]
         elseif spec["inherit"] isa String
             [Symbol(spec["inherit"]::String)]
         elseif spec["inherit"] isa Vector{String}
             [Symbol(name) for name in spec["inherit"]::Vector{String}]
         else
             Symbol[]
         end)
end

## Recolouring ##

const recolor_hooks = Function[]
const recolor_lock = ReentrantLock()

"""
    recolor(f::Function)

Register a hook function `f` to be called whenever the colors change.

Usually hooks will be called once after terminal colors have been
determined. These hooks enable dynamic retheming, but are specifically *not* run when faces
are changed. They sit in between the default faces and modifications layered on
top with `loadface!` and user customisations.
"""
function recolor(f::Function)
    @lock recolor_lock push!(recolor_hooks, f)
    nothing
end

"""
    setcolors!(color::Vector{Pair{Symbol, RGBTuple}})

Update the known base colors with those in `color`, and recalculate current faces.

`color` should be a complete list of known colours. If `:foreground` and
`:background` are both specified, the faces in the light/dark theme will be
loaded. Otherwise, only the base theme will be applied.
"""
function setcolors!(color::Vector{Pair{Symbol, RGBTuple}})
    lock(recolor_lock)
    lock(FACES.lock)
    try
        # Apply colors
        fg, bg = nothing, nothing
        for (name, rgb) in color
            FACES.basecolors[name] = rgb
            if name === :foreground
                fg = rgb
            elseif name === :background
                bg = rgb
            end
        end
        newtheme = if isnothing(fg) || isnothing(bg)
            :unknown
        else
            ifelse(sum(fg) > sum(bg), :dark, :light)
        end
        # Reset all themes to defaults
        current = FACES.current[]
        for theme in keys(FACES.themes), (name, _) in FACES.modifications[theme]
            default = get(FACES.themes.base, name, nothing)
            isnothing(default) && continue
            current[name] = default
        end
        if newtheme ∈ keys(FACES.themes)
            for (name, face) in FACES.themes[newtheme]
                current[name] = merge(current[name], face)
            end
        end
        # Run recolor hooks
        for hook in recolor_hooks
            hook()
        end
        # Layer on modifications
        for theme in keys(FACES.themes)
            theme ∈ (:base, newtheme) || continue
            for (name, face) in FACES.modifications[theme]
                current[name] = merge(current[name], face)
            end
        end
    finally
        unlock(FACES.lock)
        unlock(recolor_lock)
    end
end

## Color utils ##

"""
    UNRESOLVED_COLOR_FALLBACK

The fallback `RGBTuple` used when asking for a color that is not defined.
"""
const UNRESOLVED_COLOR_FALLBACK = (r = 0xff, g = 0x00, b = 0xff) # Pink

"""
    MAX_COLOR_FORWARDS

The maximum number of times to follow color references when resolving a color.
"""
const MAX_COLOR_FORWARDS = 12

"""
    try_rgbcolor(name::Symbol, stamina::Int = MAX_COLOR_FORWARDS)

Attempt to resolve `name` to an `RGBTuple`, taking up to `stamina` steps.
"""
function try_rgbcolor(name::Symbol, stamina::Int = MAX_COLOR_FORWARDS)
    for s in stamina:-1:1 # Do this instead of a while loop to prevent cyclic lookups
        face = get(FACES.current[], name, Face())
        fg = face.foreground
        if isnothing(fg)
            isempty(face.inherit) && break
            for iname in face.inherit
                irgb = try_rgbcolor(iname, s - 1)
                !isnothing(irgb) && return irgb
            end
        end
        fg.value isa RGBTuple && return fg.value
        fg.value == name && return get(FACES.basecolors, name, nothing)
        name = fg.value
    end
end

"""
    rgbcolor(color::Union{Symbol, SimpleColor})

Resolve a `color` to an `RGBTuple`.

The resolution follows these steps:
1. If `color` is a `SimpleColor` holding an `RGBTuple`, that is returned.
2. If `color` names a face, the face's foreground color is used.
3. If `color` names a base color, that color is used.
4. Otherwise, `UNRESOLVED_COLOR_FALLBACK` (bright pink) is returned.
"""
function rgbcolor(color::Union{Symbol, SimpleColor})
    name = if color isa Symbol
        color
    elseif color isa SimpleColor
        color.value
    end
    name isa RGBTuple && return name
    @something(try_rgbcolor(name),
               get(FACES.basecolors, name, UNRESOLVED_COLOR_FALLBACK))
end

"""
    blend(a::Union{Symbol, SimpleColor}, [b::Union{Symbol, SimpleColor} => α::Real]...)

Blend colors `a` and `b` in Oklab space, with mix ratio `α` (0–1).

The colors `a` and `b` can either be `SimpleColor`s, or `Symbol`s naming a face
or base color. The mix ratio `α` combines `(1 - α)` of `a` with `α` of `b`.

Multiple colors can be blended at once by providing multiple `b => α` pairs.

# Examples

```julia-repl
julia> blend(SimpleColor(0xff0000), SimpleColor(0x0000ff), 0.5)
SimpleColor(■ #8b54a1)

julia> blend(:red, :yellow, 0.7)
SimpleColor(■ #d47f24)

julia> blend(:green, SimpleColor(0xffffff), 0.3)
SimpleColor(■ #74be93)
```
"""
function blend end

function blend(primaries::Pair{RGBTuple, <:Real}...)
     function oklab(rgb::RGBTuple)
        r, g, b = (rgb.r / 255)^2.2, (rgb.g / 255)^2.2, (rgb.b / 255)^2.2
        l = cbrt(0.4122214708 * r + 0.5363325363 * g + 0.0514459929 * b)
        m = cbrt(0.2119034982 * r + 0.6806995451 * g + 0.1073969566 * b)
        s = cbrt(0.0883024619 * r + 0.2817188376 * g + 0.6299787005 * b)
        L = 0.2104542553 * l + 0.7936177850 * m - 0.0040720468 * s
        a = 1.9779984951 * l - 2.4285922050 * m + 0.4505937099 * s
        b = 0.0259040371 * l + 0.7827717662 * m - 0.8086757660 * s
        (; L, a, b)
    end
    function rgb((; L, a, b))
        tohex(v) = round(UInt8, min(255.0, 255 * max(0.0, v)^(1 / 2.2)))
        l = (L + 0.3963377774 * a + 0.2158037573 * b)^3
        m = (L - 0.1055613458 * a - 0.0638541728 * b)^3
        s = (L - 0.0894841775 * a - 1.2914855480 * b)^3
        r = 4.0767416621 * l - 3.3077115913 * m + 0.2309699292 * s
        g = -1.2684380046 * l + 2.6097574011 * m - 0.3413193965 * s
        b = -0.0041960863 * l - 0.7034186147 * m + 1.7076147010 * s
        (r = tohex(r), g = tohex(g), b = tohex(b))
    end
    L′, a′, b′ = 0.0, 0.0, 0.0
    for (color, α) in primaries
        lab = oklab(color)
        L′ += lab.L * α
        a′ += lab.a * α
        b′ += lab.b * α
    end
    mix = (L = L′, a = a′, b = b′)
    rgb(mix)
end

blend(base::RGBTuple, primaries::Pair{RGBTuple, <:Real}...) =
    blend(base => 1.0 - sum(last, primaries), primaries...)

blend(primaries::Pair{<:Union{Symbol, SimpleColor}, <:Real}...) =
    SimpleColor(blend((rgbcolor(c) => w for (c, w) in primaries)...))

blend(base::Union{Symbol, SimpleColor}, primaries::Pair{<:Union{Symbol, SimpleColor}, <:Real}...) =
    SimpleColor(blend(rgbcolor(base), (rgbcolor(c) => w for (c, w) in primaries)...))

blend(a::Union{Symbol, SimpleColor}, b::Union{Symbol, SimpleColor}, α::Real) =
    blend(a => 1 - α, b => α)
