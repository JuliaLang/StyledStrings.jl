# This file is a part of Julia. License is MIT: https://julialang.org/license

const STANDARD_FACES = let
    # Colours
    (; black, red, green, yellow, blue, magenta, cyan, white,
     bright_black, bright_red, bright_green, bright_yellow,
     bright_blue, bright_magenta, bright_cyan, bright_white,
     foreground, background) = BASE_FACES
    default = Face(FaceDef(
        "monospace",            # font
        foreground, background, # foreground, background
        WeakNothing(),          # underline (color)
        120,                    # height
        NORMAL_WEIGHT, NORMAL_SLANT,
        strongnothing(UInt8),   # underline (style)
        0x00, 0x00,             # strikethrough, inverse
        Memory{Face}()))
    # Property faces
    bold = Face(weight=:bold)
    light = Face(weight=:light)
    italic = Face(slant=:italic)
    underline = Face(underline=true)
    strikethrough = Face(strikethrough=true)
    inverse = Face(inverse=true)
    # Useful common faces
    shadow = Face(foreground=bright_black)
    region = Face(background=0x636363)
    emphasis = Face(foreground=blue)
    link = Face(underline=blue)
    highlight = Face(inherit=emphasis, inverse=true)
    code = Face(foreground=cyan)
    key = Face(inherit=code)
    # Styles of generic content categories
    error = Face(foreground=bright_red)
    warning = Face(foreground=yellow)
    success = Face(foreground=green)
    info = Face(foreground=bright_cyan)
    note = Face(foreground=bright_black)
    tip = Face(foreground=bright_green)
    # Log messages
    log_error = Face(foreground=error, weight=:bold)
    log_warn = Face(foreground=warning, weight=:bold)
    log_info = Face(foreground=info, weight=:bold)
    log_debug = Face(foreground=blue, weight=:bold)
    # Julia prompts
    REPL_prompt = Face(weight=:bold)
    REPL_prompt_julia = Face(inherit=[green, REPL_prompt])
    REPL_prompt_help = Face(inherit=[yellow, REPL_prompt])
    REPL_prompt_shell = Face(inherit=[red, REPL_prompt])
    REPL_prompt_pkg = Face(inherit=[blue, REPL_prompt])
    REPL_prompt_beep = Face(inherit=[shadow, REPL_prompt])
    # All together now
    (; default, foreground, background,
     # Property faces
     bold, light, italic, underline, strikethrough, inverse,
     # Basic color faces
     black, red, green, yellow, blue, magenta, cyan, white,
     bright_black, bright_red, bright_green, bright_yellow,
     bright_blue, bright_magenta, bright_cyan, bright_white,
     # Useful common faces
     shadow, region, emphasis, link, highlight, code, key,
     # Styles of generic content categories
     error, warning, success, info, note, tip,
     # Log messages
     log_error, log_warn, log_info, log_debug,
     # Julia prompts
     REPL_prompt, REPL_prompt_julia, REPL_prompt_help,
     REPL_prompt_shell, REPL_prompt_pkg, REPL_prompt_beep)
end

const UNCACHED = copy(EMPTY_FACE) # Marks an empty cache slot: a face nothing else can reference

function emptycache!(cache::AtomicMemory{Pair{Face, Face}})
    for i in eachindex(cache)
        @atomic cache[i] = UNCACHED => UNCACHED
    end
    cache
end

emptycache() = emptycache!(AtomicMemory{Pair{Face, Face}}(undef, 256))

"""
Globally named [`Face`](@ref)s.

`default` gives the initial values of the faces, and `current` holds the active
(potentially modified) set of faces. This two-set system allows for any
modifications to the active faces to be undone.
"""
const FACES = let
    # Bidirectional mapping of names to faces
    POOL = IdDict{Symbol, Face}(pairs(STANDARD_FACES))
    NAMES = IdDict(f => n for (n, f) in POOL)
    # Aliases
    POOL[:warn] = STANDARD_FACES.warning
    POOL[:grey] = STANDARD_FACES.bright_black
    POOL[:gray] = STANDARD_FACES.bright_black
    # Themes and base colors
    light = IdDict{Face, Face}(
        STANDARD_FACES.region => Face(background=0xaaaaaa),
    )
    dark = IdDict{Face, Face}(
        STANDARD_FACES.region => Face(background=0x363636),
    )
    basecolors = IdDict{Face, RGBTuple}( # Based on Gnome HIG colours
        BASE_FACES.foreground     => (r = 0xf6, g = 0xf5, b = 0xf4),
        BASE_FACES.background     => (r = 0x24, g = 0x1f, b = 0x31),
        BASE_FACES.black          => (r = 0x1c, g = 0x1a, b = 0x23),
        BASE_FACES.red            => (r = 0xa5, g = 0x1c, b = 0x2c),
        BASE_FACES.green          => (r = 0x25, g = 0xa2, b = 0x68),
        BASE_FACES.yellow         => (r = 0xe5, g = 0xa5, b = 0x09),
        BASE_FACES.blue           => (r = 0x19, g = 0x5e, b = 0xb3),
        BASE_FACES.magenta        => (r = 0x80, g = 0x3d, b = 0x9b),
        BASE_FACES.cyan           => (r = 0x00, g = 0x97, b = 0xa7),
        BASE_FACES.white          => (r = 0xdd, g = 0xdc, b = 0xd9),
        BASE_FACES.bright_black   => (r = 0x76, g = 0x75, b = 0x7a),
        BASE_FACES.bright_red     => (r = 0xed, g = 0x33, b = 0x3b),
        BASE_FACES.bright_green   => (r = 0x33, g = 0xd0, b = 0x79),
        BASE_FACES.bright_yellow  => (r = 0xf6, g = 0xd2, b = 0x2c),
        BASE_FACES.bright_blue    => (r = 0x35, g = 0x83, b = 0xe4),
        BASE_FACES.bright_magenta => (r = 0xbf, g = 0x60, b = 0xca),
        BASE_FACES.bright_cyan    => (r = 0x26, g = 0xc6, b = 0xda),
        BASE_FACES.bright_white   => (r = 0xf6, g = 0xf5, b = 0xf4))
    # Combined structure
    (pool = POOL,
     names = NAMES,
     unregistered = IdDict{Symbol, Face}(),
     themes = (; light, dark),
     current_theme = Ref(:base),
     modifications = (
         base = IdDict{Face, Face}(),
         light = IdDict{Face, Face}(),
         dark = IdDict{Face, Face}()),
     displacements = IdDict{Face, Face}(),
     current = ScopedValue(IdDict{Face, Face}()),
     cache = ScopedValue(emptycache()),
     basecolors = basecolors,
     lock = ReentrantLock())
end

## Adding and resetting faces ##

"""
    override(base::Face, mods::Face)

Create a new [`Face`](@ref) by overriding the attributes of `base` with those
set in `mods`.

Strong-nothing values in `mods` unset the corresponding property in `base` and
leave a weak-nothing in the result, while weak-nothing values in `mods` leave
the corresponding property in `base` unchanged.
"""
function override end

function override(base::FaceDef, mods::FaceDef)
    Base.@constprop :aggressive function mergeattr(a::FaceDef, b::FaceDef, attr::Symbol)
        a_attr = getfield(a, attr)
        b_attr = getfield(b, attr)
        if isweaknothing(b_attr)
            a_attr
        elseif isstrongnothing(b_attr)
            weaknothing(b_attr)
        else
            b_attr
        end
    end
    FaceDef(
        mergeattr(base, mods, :font),
        mergeattr(base, mods, :foreground),
        mergeattr(base, mods, :background),
        mergeattr(base, mods, :underline),
        mergeattr(base, mods, :height),
        mergeattr(base, mods, :weight),
        mergeattr(base, mods, :slant),
        mergeattr(base, mods, :underline_style),
        mergeattr(base, mods, :strikethrough),
        mergeattr(base, mods, :inverse),
        if isempty(mods.inherit)
            base.inherit
        else
            mods.inherit
        end)
end

override(base::Face, mods::Face) = Face(override(base.f, mods.f))

"""
    addface!(name::Symbol => default::Face, theme::Symbol = :base)

Create a new face by the name `name`. So long as no face already exists by this
name, `default` is added to both `FACES.themes[theme]` and (a copy of) to
`FACES.current`, with the current value returned.

The `theme` should be either `:base`, `:light`, or `:dark`.

Should the face `name` already exist, `nothing` is returned.

!!! warning Deprecated
    `addface!` is deprecated and will be removed in a future release. Please
    define faces with [`@defpalette!`](@ref) and [`@registerpalette!`](@ref) instead.

# Examples

```jldoctest; setup = :(import StyledStrings: Face, addface!)
julia> addface!(:mypkg_myface => Face(slant=:italic, underline=true))
Face (sample)
         slant: italic
     underline: true
```
"""
function addface!((name, default)::Pair{Symbol, Face}, theme::Symbol = :base)
    # Base.depwarn("`addface!` is deprecated as of v1.14 and will be removed in a future release. \
    #               Please define faces with `@defpalette!` and `@registerpalette!` instead.",
    #                :addface!)
    @lock FACES.lock if theme === :base
        haskey(FACES.pool, name) && @warn lazy"Face $name already exists, overriding"
        unreg = get(FACES.unregistered, name, nothing)
        isnothing(unreg) || register_displace!(unreg, default, name)
        FACES.pool[name] = default
        FACES.names[default] = name
    elseif !haskey(FACES.themes[theme], name)
        face = lookmakeface(name, false)
        FACES.themes[theme][face] = default
        relayer!(face)
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
        emptycache!(FACES.cache[])
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

!!! warning "Deprecated"
    `resetfaces!` is deprecated and will be removed in a future release.
    Please specify the face to be reset directly using `resetfaces(::Face)`.
"""
function resetfaces!(name::Symbol, theme::Symbol = :base)
    # Base.depwarn("`resetfaces!` is deprecated as of v1.14 and will be removed in a future release. \
    #               Please specify the face to be reset directly using `resetfaces(::Face)`.",
    #                :resetfaces!)
    face = get(FACES.pool, name, nothing)
    if !isnothing(face)
        resetfaces!(face, theme)
    end
end

"""
    resetfaces!(face::Face)

Reset the face `face` to its default value.

If the face is not registered, nothing is done.
"""
function resetfaces!(face::Face, theme::Symbol = :all)
    @lock FACES.lock begin
        delete!(FACES.current[], face)
        emptycache!(FACES.cache[])
        if FACES.current.default === FACES.current[] # Only when top-level
            if theme === :all
                for mode in values(FACES.modifications)
                    delete!(mode, face)
                end
            else
                delete!(FACES.modifications[theme], face)
            end
        end
    end
    nothing
end

"""
    remapfaces(s::AnnotatedString, kv::Pair{Face, Face}...) -> AnnotatedString

Substitute the faces annotating `s` according to `kv`, leaving other annotations as they are.

# Examples

```jldoctest; setup = :(import StyledStrings: Face, remapfaces)
julia> remapfaces(styled"some {red:important} text", face"red" => face"blue") |> annotations
1-element Vector{@NamedTuple{region::UnitRange{Int64}, label::Symbol, value::Face}}:
 (region = 6:14, label = :face, value = face"blue")
```
"""
function remapfaces(s::AnnotatedString, kv::Pair{Face, Face}...)
    remap = IdDict{Face, Face}(kv)
    AnnotatedString(s.string, map(annotations(s)) do (; region, label, value)
        if label === :face
            (; region, label, value = get(remap, value, value))
        else
            (; region, label, value)
        end
    end)
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
julia> withfaces(face"yellow" => Face(foreground=face"red"), face"green" => face"blue") do
           println(styled"{yellow:red} and {green:blue} mixed make {magenta:purple}")
       end
red and blue mixed make purple
```
"""
function withfaces(f, keyvals_itr)
    # Before modifying the current `FACES`, we should ensure
    # that we've loaded the user's customisations.
    load_customisations!()
    if eltype(keyvals_itr) <: Pair{Face}
    elseif eltype(keyvals_itr) <: Pair{Symbol}
        keyvals_itr = Iterators.map(keyvals_itr) do (k, v)
            lookmakeface(k) => v
        end
    else
        throw(MethodError(withfaces, (f, keyvals_itr)))
    end
    newfaces = copy(FACES.current[])
    for (face, new) in keyvals_itr
        if new isa Face
            newfaces[face] = new
        elseif new isa Symbol
            newf = lookmakeface(new)
            newfaces[face] = get(FACES.current[], newf, newf)
        elseif new isa Vector{Symbol}
            newfs = map(lookmakeface, new)
            newfaces[face] = Face(inherit=[get(FACES.current[], nf, nf) for nf in newfs])
        elseif new isa Vector{Face}
            newfaces[face] = Face(inherit=new)
        elseif haskey(newfaces, face)
            delete!(newfaces, face)
        end
    end
    @with(FACES.current => newfaces, FACES.cache => emptycache(), f())
end

function withfaces(f, keyvals::Pair{Symbol, <:Union{Symbol, Vector{Symbol}, Nothing}}...)
    # Base.depwarn("`withfaces` with `Symbol` face names is deprecated as of v1.14 and will be removed in a future release. \
    #               Instead you should specify the target faces directly as `Face`s (e.g. from `face\"\"`).",
    #                :withfaces)
    withfaces(f, keyvals)
end

withfaces(f, keyvals::Pair{Face, <:Union{Face, Union{Symbol, Face}, Vector{Face}, Vector{Union{Symbol, Face}}, Nothing}}...) =
    withfaces(f, keyvals)

withfaces(f) = f()

## Getting the combined face from a set of properties ##

# Putting these inside `getface` causes the julia compiler to box it
_mergedface(face::Face) = get(FACES.current[], face, face)
_mergedface(face::Symbol) = lookmakeface(face)
_mergedface(faces::Vector) = mapfoldl(_mergedface, merge, Iterators.reverse(faces))
_mergedface(face::Any) = _mergedface(foreignface(face))

"""
    foreignface(face) -> Face

Rebuild `face`, a `Face` from another loaded copy of StyledStrings, as one of ours.

The REPL runs on a private copy of the stdlib, which is not necessarily the one user code
loads (see `Base.require_stdlib`). Each copy displays its own faces, but a string may hold
faces of both copies, so faces of one can reach the other's `getface`.

Named faces (base colours included) map to ours by identity, so that self-referential base
faces terminate and theme overrides keyed on the equivalent face still apply. Other faces
with our `FaceDef` layout are copied field by field, those with our properties are rebuilt
from them, and anything else is a `MethodError`. A face name `Symbol`, which is how pre-1.14
copies refer to colours and inheritance, is looked up as ours.
"""
function foreignface(face)
    T = typeof(face)
    other = parentmodule(T)
    color(::Nothing) = nothing
    color(c)::Union{RGBTuple, Face} = if c.value isa RGBTuple c.value else foreignface(c.value) end
    samelayout(t, s) = if t isa Union && s isa Union
        issetequal(map(nameof, Base.uniontypes(t)), map(nameof, Base.uniontypes(s)))
    else
        !(t isa Union || s isa Union) && nameof(t) == nameof(s)
    end
    if nameof(other) !== :StyledStrings || nameof(T) !== :Face
        throw(MethodError(_mergedface, (face,)))
    end
    name = if isdefined(other, :FACES) && hasproperty(other.FACES, :names) get(other.FACES.names, face, nothing) end
    named = if !isnothing(name) get(FACES.pool, name, nothing) end
    isnothing(named) || return named
    if hasfield(T, :f) && fieldnames(fieldtype(T, :f)) == fieldnames(FaceDef) &&
        all(splat(samelayout), zip(fieldtypes(fieldtype(T, :f)), fieldtypes(FaceDef)))
        # Same encoding: copy the fields, rewriting only the nothings and references to `other`'s faces
        def = getfield(face, :f)
        attr(x) = if x isa other.WeakNothing
            WeakNothing()
        elseif x isa other.StrongNothing
            StrongNothing()
        elseif x isa other.Face
            foreignface(x)
        else
            x
        end
        Face(FaceDef(attr(def.font), attr(def.foreground), attr(def.background), attr(def.underline),
                     def.height, def.weight, def.slant, def.underline_style, def.strikethrough, def.inverse,
                     Face[foreignface(f) for f in def.inherit].ref.mem))
    elseif issetequal(propertynames(face), propertynames(EMPTY_FACE))
        underline = face.underline
        Face(font = face.font, height = face.height, weight = face.weight, slant = face.slant,
             foreground = color(face.foreground), background = color(face.background),
             underline = if underline isa Tuple
                             (color(underline[1]), underline[2])
                         elseif underline isa Union{Nothing, Bool}
                             underline
                         else
                             color(underline)
                         end,
             strikethrough = face.strikethrough, inverse = face.inverse,
             inherit = Face[foreignface(f) for f in face.inherit])
    else
        throw(MethodError(_mergedface, (face,)))
    end
end

foreignface(name::Symbol) = lookmakeface(name)

"""
    getface(faces)

Obtain the final merged face from `faces`, an iterator of
[`Face`](@ref)s, face name `Symbol`s, and lists thereof.
"""
function getface(faces)
    cdefault = getface()
    isempty(faces) && return cdefault
    merge(cdefault, mapfoldl(_mergedface, merge, faces)::Face)
end

"""
    getface(annotations::Vector{@NamedTuple{label::Symbol, value}}, cache = FACES.cache[])

Combine all of the `:face` annotations with `getfaces`.
"""
function getface(annotations::Vector{@NamedTuple{label::Symbol, value::V}},
                 cache::AtomicMemory{Pair{Face, Face}} = FACES.cache[]) where {V}
    faces = (ann.value for ann in annotations if ann.label === :face)
    face = nothing # A lone `Face`, the usual case, resolves without the fold
    for ann in annotations
        ann.label === :face || continue
        isnothing(face) && ann.value isa Face || return getface(faces)
        face = ann.value::Face
    end
    if isnothing(face) getface() else getface(face, cache) end
end

"""
    getface(face::Face, cache = FACES.cache[]) -> Face

Obtain `face` resolved against the current definitions and the default face, via `cache`.
"""
function getface(face::Face, cache::AtomicMemory{Pair{Face, Face}} = FACES.cache[])
    mixed = UInt(pointer_from_objref(face)) * 0x9e3779b97f4a7c15 # 64-bit golden ratio factor
    i, j = Int(mixed >> 56) + 1, Int(mixed >> 48 & 0xff) + 1
    slot1 = @atomic cache[i]
    slot1.first === face && return slot1.second
    slot2 = @atomic cache[j]
    slot2.first === face && return slot2.second
    current = FACES.current[]
    resolved = merge(get(current, STANDARD_FACES.default, STANDARD_FACES.default), get(current, face, face))
    at = if slot1.first === UNCACHED || slot2.first !== UNCACHED && isodd(mixed >> 40) i else j end
    @atomic cache[at] = face => resolved
    resolved
end

getface(face::Symbol) = getface(lookmakeface(face))

"""
    getface()

Obtain the default face.
"""
getface() = get(FACES.current[], STANDARD_FACES.default, STANDARD_FACES.default)

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
function face! end

face!(s::Union{<:AnnotatedString, <:SubString{<:AnnotatedString}}, range::UnitRange{Int}, face::Face) =
    annotate!(s, range, :face, face)

face!(s::Union{<:AnnotatedString, <:SubString{<:AnnotatedString}}, face) =
    face!(s, firstindex(s):lastindex(s), face)

# Deprecated API
function face!(s::Union{<:AnnotatedString, <:SubString{<:AnnotatedString}},
               range::UnitRange{Int}, faces::Vector{Symbol})
    for face in faces
        face!(s, range, face)
    end
end

# Deprecated API
face!(s::Union{<:AnnotatedString, <:SubString{<:AnnotatedString}}, range::UnitRange{Int}, face::Symbol) =
    annotate!(s, range, :face, lookmakeface(face))


## Reading face definitions from a dictionary ##

"""
    setface!(original::Face => update::Face, [theme::Symbol = :base])

Merge the current value of `original` with `update`.

# Examples

```jldoctest; setup = :(import StyledStrings: Face, setface!)
julia> setface!(face"red" => Face(foreground=0xff0000))
Face (sample)
    foreground: #ff0000
```
"""
function setface!((original, update)::Pair{Face, Face}, theme::Symbol = :base)
    @lock FACES.lock begin
        current = FACES.current[]
        if FACES.current.default === current # Only save top-level modifications
            mface = get(FACES.modifications[theme], original, nothing)
            isnothing(mface) || (update = override(mface, update))
            FACES.modifications[theme][original] = update
        end
        if theme ∈ (:base, FACES.current_theme[])
            update = override(get(current, original, original), update)
            current[original] = update
            emptycache!(FACES.cache[])
            update
        end
    end
end

"""
    loadface!(name::Symbol => update::Face)

Merge the current value of the face `name` with `update`.

!!! warning "Deprecated"
    `loadface!` with `Symbol` names is deprecated and will be removed in a future release.
    Instead you should specify the target face directly as a `Face` (e.g. from `face""`).
"""
function loadface!((name, update)::Pair{Symbol, Face}, theme::Symbol = :base)
    # Base.depwarn("`loadface!` with `Symbol` names is deprecated as of v1.14 and will be removed in a future release. \
    #               Instead you should call `setface!` and specify the target face directly as a `Face` (e.g. from `face\"\"`).",
    #                :loadface!)
    setface!(lookmakeface(name, false) => update, theme)
end

function loadface!((name, _)::Pair{Symbol, Nothing})
    # Base.depwarn("`loadface!` with `Symbol` names is deprecated as of v1.14 and will be removed in a future release. \
    #               Instead you should call `setface!` and specify the target face directly as a `Face` (e.g. from `face\"\"`).",
    #              :loadface!)
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
        spec isa Dict{String, Any} || continue
        fullname = if isnothing(prefix)
            name
        else
            string(prefix, '_', name)
        end
        fspec = filter((_, v)::Pair -> !(v isa Dict), spec)
        fnest = filter((_, v)::Pair -> v isa Dict, spec)
        if !isempty(fspec)
            face = lookmakeface(Symbol(fullname), false)
            setface!(face => convert(Face, fspec), theme)
        end
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
    function colorvalue(str::String)
        color = tryparse(SimpleColor, str)
        if isnothing(color) WeakNothing() else color.value end
    end
    function safeget(spc::Dict{String, Any}, ::Type{T}, keys::String...) where {T}
        val = nothing
        for key in keys
            val = get(spc, key, nothing)
            !isnothing(val) && break
        end
        if isnothing(val)
            weaknothing(T)
        elseif val isa String && val == "inherit"
            strongnothing(T)
        elseif T == SimpleColor && val isa String
            colorvalue(val)
        elseif T != SimpleColor && val isa T
            if T == Bool
                UInt8(val)
            else
                val
            end
        else
            weaknothing(T)
        end
    end
    namebyte(names::Tuple{Vararg{Symbol}}, str::String) = something(attrbyte(names, Symbol(str)), weaknothing(UInt8))
    font = safeget(spec, String, "font")
    height = let h = get(spec, "height", nothing)
        if isnothing(h)
            weaknothing(UInt32)
        elseif h isa String && h == "inherit"
            strongnothing(UInt32)
        elseif h isa Int
            UInt32(h)
        elseif h isa Float64
            reinterpret(UInt32, Float32(h)) & ~(typemax(UInt32) >> 1)
        else
            weaknothing(UInt32)
        end
    end
    weight = if haskey(spec, "weight") && spec["weight"] isa String
        if spec["weight"]::String == "inherit"
            strongnothing(UInt8)
        else
            namebyte(WEIGHT_NAMES, spec["weight"]::String)
        end
    elseif haskey(spec, "bold") && spec["bold"] isa Bool
        ifelse(spec["bold"]::Bool, namebyte(WEIGHT_NAMES, "bold"), NORMAL_WEIGHT)
    else
        weaknothing(UInt8)
    end
    slant = if haskey(spec, "slant") && spec["slant"] isa String
        if spec["slant"]::String == "inherit"
            strongnothing(UInt8)
        else
            namebyte(SLANT_NAMES, spec["slant"]::String)
        end
    elseif haskey(spec, "italic") && spec["italic"] isa Bool
        ifelse(spec["italic"]::Bool, namebyte(SLANT_NAMES, "italic"), NORMAL_SLANT)
    else
        weaknothing(UInt8)
    end
    foreground = safeget(spec, SimpleColor, "foreground", "fg")
    background = safeget(spec, SimpleColor, "background", "bg")
    ul, ulstyle = if !haskey(spec, "underline")
        WeakNothing(), weaknothing(UInt8)
    elseif spec["underline"] isa Bool
        WeakNothing(), ifelse(spec["underline"]::Bool, STRAIGHT_UNDERLINE, strongnothing(UInt8))
    elseif spec["underline"] isa String
        if spec["underline"]::String == "inherit"
            StrongNothing(), strongnothing(UInt8)
        else
            colorvalue(spec["underline"]::String), STRAIGHT_UNDERLINE
        end
    elseif spec["underline"] isa Vector{String} && length(spec["underline"]::Vector{String}) == 2
        color_str, style_str = (spec["underline"]::Vector{String})
        colorvalue(color_str), something(attrbyte(UNDERLINE_STYLE_NAMES, Symbol(style_str)), STRAIGHT_UNDERLINE)
    else
        WeakNothing(), weaknothing(UInt8)
    end
    strikethrough = safeget(spec, Bool, "strikethrough")
    inverse = safeget(spec, Bool, "inverse")
    inherit = if !haskey(spec, "inherit")
        Face[]
    elseif spec["inherit"] isa String
        [lookmakeface(Symbol(spec["inherit"]::String))]
    elseif spec["inherit"] isa Vector{String}
        [lookmakeface(Symbol(name)) for name in spec["inherit"]::Vector{String}]
    else
        Face[]
    end
    Face(FaceDef(font, foreground, background, ul, height,
                 weight, slant, ulstyle, strikethrough, inverse, inherit.ref.mem))
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
top with `setface!` and user customisations.
"""
function recolor(f::Function)
    @lock recolor_lock push!(recolor_hooks, f)
    nothing
end

"""
    relayer!(face::Face)

Recompute the current definition of `face` from its variant for the current
theme and its base and current-theme modifications, layered as `setcolors!` does.
"""
function relayer!(face::Face)
    theme = FACES.current_theme[]
    current = FACES.current.default
    delete!(current, face)
    function layer!(table)
        update = get(table, face, nothing)
        isnothing(update) && return
        current[face] = override(get(current, face, face), update)
    end
    theme === :base || layer!(FACES.themes[theme])
    layer!(FACES.modifications.base)
    theme === :base || layer!(FACES.modifications[theme])
    emptycache!(FACES.cache.default)
end

"""
    setcolors!(colors::Vector{Pair{Symbol, RGBTuple}})

Update the known base colors with those in `colors`, and recalculate current faces.

`color` should be a complete list of known colours. If `:foreground` and
`:background` are both specified, the faces in the light/dark theme will be
loaded. Otherwise, only the base theme will be applied.
"""
function setcolors!(colors::Vector{Pair{Symbol, RGBTuple}})
    lock(recolor_lock)
    lock(FACES.lock)
    # Make sure we've loaded customisations before re-layering them.
    load_customisations!()
    try
        # Apply colors
        fg, bg = nothing, nothing
        for (name, rgb) in colors
            FACES.basecolors[FACES.pool[name]] = rgb
            if name === :foreground
                fg = rgb
            elseif name === :background
                bg = rgb
            end
        end
        newtheme = if isnothing(fg) || isnothing(bg)
            :base
        else
            ifelse(sum(fg) > sum(bg), :dark, :light)
        end
        FACES.current_theme[] = newtheme
        # Reset all themes to defaults
        current = FACES.current[]
        empty!(current)
        if newtheme ∈ keys(FACES.themes)
            for (name, face) in FACES.themes[newtheme]
                current[name] = override(get(current, name, name), face)
            end
        end
        # Run recolor hooks
        for hook in recolor_hooks
            hook()
        end
        # Layer on modifications
        for theme in keys(FACES.modifications)
            theme ∈ (:base, newtheme) || continue
            for (name, face) in FACES.modifications[theme]
                current[name] = override(get(current, name, name), face)
            end
        end
        emptycache!(FACES.cache[])
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
    finalcolor(face::Face, stamina::Int = MAX_COLOR_FORWARDS)

Attempt to resolve `face` to a final color, taking up to `stamina` steps.

Produces an `RGBTuple` or `Face` if successful, `nothing` otherwise.
"""
function finalcolor(face::Face, stamina::Int = MAX_COLOR_FORWARDS)
    current = FACES.current[]
    for s in stamina:-1:1 # Do this instead of a while loop to prevent cyclic lookups
        fg = face.f.foreground
        if isnothingflavour(fg)
            isempty(face.inherit) && return nothing
            for iface in face.inherit
                irgb = finalcolor(iface, s - 1)
                !isnothing(irgb) && return irgb
            end
        elseif fg isa RGBTuple
            return fg
        else # fg isa Face
            face = get(current, fg, fg)
            face.f.foreground === fg && return face
        end
    end
end

function finalcolor(color::SimpleColor)
    value = color.value
    value isa RGBTuple && return value
    face = get(FACES.current[], value, value)
    face.f.foreground === value && return face # Final already, the usual case
    finalcolor(face)
end

"""
    rgbcolor(color::Union{Symbol, Face, SimpleColor})

Resolve a `color` to an `RGBTuple`.

The resolution follows these steps:
1. If `color` is a `SimpleColor` holding an `RGBTuple`, that is returned.
2. If `color` names a face, the face's foreground color is used.
3. If `color` names a base color, that color is used.
4. Otherwise, `UNRESOLVED_COLOR_FALLBACK` (bright pink) is returned.
"""
function rgbcolor end

function rgbcolor(color::SimpleColor)
    if color.value isa RGBTuple
        color.value
    else
        rgbcolor(get(FACES.current[], color.value, color.value))
    end
end

function rgbcolor(face::Face)
    color = finalcolor(face)
    if isnothing(color)
        UNRESOLVED_COLOR_FALLBACK
    elseif color isa RGBTuple
        color
    else
        get(FACES.basecolors, color, UNRESOLVED_COLOR_FALLBACK)
    end
end

function rgbcolor(color::Symbol)
    face = get(FACES.pool, color, nothing)
    isnothing(face) && return UNRESOLVED_COLOR_FALLBACK
    rgbcolor(get(FACES.current[], face, face))
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

function blend(x1::Pair{RGBTuple, <:Real}, x2::Pair{RGBTuple, <:Real}...)
    primaries = (x1, x2...)
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

blend((c0, w0)::Pair{<:Union{Symbol, Face, SimpleColor}, <:Real}, primaries::Pair{<:Union{Symbol, Face, SimpleColor}, <:Real}...) =
    SimpleColor(blend(rgbcolor(c0) => w0, (rgbcolor(c) => w for (c, w) in primaries)...))

blend(base::Union{Symbol, Face, SimpleColor}, primaries::Pair{<:Union{Symbol, Face, SimpleColor}, <:Real}...) =
    SimpleColor(blend(rgbcolor(base), (rgbcolor(c) => w for (c, w) in primaries)...))

blend(a::Union{Symbol, Face, SimpleColor}, b::Union{Symbol, Face, SimpleColor}, α::Real) =
    blend(a => 1 - α, b => α)
