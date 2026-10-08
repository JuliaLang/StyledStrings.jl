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
        foreground,             # underline (color)
        120,                    # height
        attrbyte(:weight, :normal), attrbyte(:slant, :normal),
        NO_UNDERLINE,           # underline (style)
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

const UNCACHED = copy(EMPTY_FACE) # Marks an empty cache slot; nothing else can reference this face

# Slot `s` resolves `keys[s]` to `defs[s]`, under the seqlock `versions[s]` (odd while written)
mutable struct FaceCache # Mutable, so reading `FACES.cache[]` doesn't allocate
    const versions::AtomicMemory{UInt}
    const keys::Memory{Face}
    const defs::Memory{FaceDef}
end

function emptycache!(cache::FaceCache)
    for slot in eachindex(cache.versions)
        writeslot!(cache, slot, UNCACHED, EMPTY_FACE.f)
    end
    cache
end

function emptycache()
    versions = AtomicMemory{UInt}(undef, 256)
    foreach(slot -> @atomic(:monotonic, versions[slot] = 0), eachindex(versions))
    emptycache!(FaceCache(versions, Memory{Face}(undef, 256), Memory{FaceDef}(undef, 256)))
end

function writeslot!(cache::FaceCache, slot::Int, face::Face, def::FaceDef)
    version = @atomic :monotonic cache.versions[slot]
    isodd(version) && return
    (; success) = @atomicreplace :acquire_release :monotonic cache.versions[slot] version => version + 1
    success || return
    Core.Intrinsics.atomic_fence(:release, :system) # Keeps the stores below after the odd version
    cache.keys[slot] = face
    cache.defs[slot] = def
    @atomic :release cache.versions[slot] = version + 2
end

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
     recolors = IdDict{Face, Face}(),
     displacements = IdDict{Face, Face}(),
     current = ScopedValue(IdDict{Face, Face}()),
     cache = ScopedValue(emptycache()),
     basecolors = basecolors,
     lock = ReentrantLock())
end

## Adding and resetting faces ##

# Throw when `def`, as the definition of `face`, inherits from `face` by way of `current`.
# A loop rather than `any`, as recursing through a closure is uninferrable, which breaks trimming.
function checkinherit(current::IdDict{Face, Face}, face::Face, def::Face)
    for parent in def.f.inherit
        parent === face && throw(ArgumentError( # By name, as showing a face does not trim
            "Face '$(get(FACES.names, face, :unnamed))' cannot inherit from itself, directly or through other faces"))
        checkinherit(current, face, get(current, parent, parent))
    end
end

"""
    override(base::Face, mods::Face)

Layer the modification `mods` over `base`: each attribute of `mods` replaces the one in
`base`, except a weak nothing, which leaves it unchanged. A strong nothing replaces it
too, resetting the attribute, as `"inherit"` does in a faces.toml file: the face then
falls back to what it inherits.
"""
function override end

function override(base::FaceDef, mods::FaceDef)
    layer(a, b) = if isweaknothing(b) a else b end
    FaceDef(layer(base.font, mods.font),
            layer(base.foreground, mods.foreground),
            layer(base.background, mods.background),
            layer(base.underline, mods.underline),
            layer(base.height, mods.height),
            layer(base.weight, mods.weight),
            layer(base.slant, mods.slant),
            layer(base.underline_style, mods.underline_style),
            layer(base.strikethrough, mods.strikethrough),
            layer(base.inverse, mods.inverse),
            if isempty(mods.inherit) base.inherit else mods.inherit end)
end

override(base::Face, mods::Face) = Face(override(base.f, mods.f))

"""
    addface!(name::Symbol => default::Face, theme::Symbol = :base)

Create a new face by the name `name`. So long as no face already exists by this
name, `default` is added to both `FACES.themes[theme]` and (a copy of) to
`FACES.current`, with the current value returned.

The `theme` should be either `:base`, `:light`, or `:dark`.

Should the face `name` already exist, `nothing` is returned.

!!! warning "Deprecated"
    `addface!` is deprecated and will be removed in a future release. Please
    define faces with [`@defpalette`](@ref) and [`@registerpalette`](@ref) instead.

# Examples

```jldoctest; setup = :(import StyledStrings: Face, addface!)
julia> addface!(:mypkg_myface => Face(slant=:italic, underline=true))
Face mypkg_myface (sample)
         slant: italic
     underline: true
```
"""
function addface!((name, default)::Pair{Symbol, Face}, theme::Symbol = :base)
    # Base.depwarn("`addface!` is deprecated as of v1.14 and will be removed in a future release. \
    #               Please define faces with `@defpalette` and `@registerpalette` instead.",
    #                :addface!)
    @lock FACES.lock begin
        face = if theme === :base
            haskey(FACES.pool, name) && return
            haskey(FACES.names, default) && throw(ArgumentError(
                "Cannot add `$name`, as the face `$(FACES.names[default])` is already registered. \
                 To base `$name` on it, use `Face(inherit = face\"$(FACES.names[default])\")` instead."))
            named = if default === EMPTY_FACE copy(default) else default end # `Face()` is shared
            unreg = get(FACES.unregistered, name, nothing)
            isnothing(unreg) || register_displace!(unreg, named, name)
            FACES.pool[name] = named
            FACES.names[named] = name
            named
        else
            themed = lookmakeface(name, false)
            haskey(FACES.themes[theme], themed) && return
            FACES.themes[theme][themed] = default
            themed
        end
        relayer!(face)
        emptycache!(FACES.cache.default)
        get(FACES.current.default, face, face)
    end
end

"""
    resetfaces!()

Reset the current global face dictionary to the default value.
"""
function resetfaces!()
    @lock FACES.lock begin
        current = FACES.current[]
        current === FACES.current.default && foreach(empty!, values(FACES.modifications)) # Only when top-level
        relayer!(current, modified = false)
        emptycache!(FACES.cache[])
        current
    end
end

"""
    resetfaces!(name::Symbol, theme::Symbol = :base)

Reset the face `name` to its default value, which is returned. The `theme` is
`:base`, `:light`, or `:dark`, as for `resetfaces!(::Face, theme)`.

If the face `name` does not exist, nothing is done and `nothing` returned.
In the unlikely event that the face `name` does not have a default value,
it is deleted, a warning message is printed, and `nothing` returned.

!!! warning "Deprecated"
    `resetfaces!` is deprecated and will be removed in a future release.
    Please specify the face to be reset directly using `resetfaces!(::Face)`.
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
    resetfaces!(face::Face, theme::Symbol = :all)

Reset the face `face` to its default value, undoing the changes made for `theme` (`:base`,
`:light`, or `:dark`), or for every theme with `:all`.

If the face is not registered, nothing is done.
"""
function resetfaces!(face::Face, theme::Symbol = :all)
    @lock FACES.lock begin
        delete!(FACES.current[], face)
        if FACES.current.default === FACES.current[] # Only when top-level
            if theme === :all
                for mode in values(FACES.modifications)
                    delete!(mode, face)
                end
            else
                delete!(FACES.modifications[theme], face)
            end
            relayer!(face)
        end
        emptycache!(FACES.cache[])
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
function remapfaces(s::AnnotatedString{S, V}, kv::Pair{Face, Face}...) where {S, V}
    remap = IdDict{Face, Face}(kv)
    AnnotatedString{S, V}(s.string, map(annotations(s)) do (; region, label, value)
        if label === :face && value isa Face
            (; region, label, value = get(remap, value, value))
        else
            (; region, label, value)
        end
    end)
end

"""
    withfaces(f, kv::Pair...)
    withfaces(f, kvpair_itr)

Execute `f` with `FACES``.current` temporarily modified by zero or more `face
=> val` arguments `kv`, or `kvpair_itr` which produces `kv`-form values. A face
given as `val` is taken as it is currently defined.

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
    eltype(keyvals_itr) <: Pair{<:Union{Face, Symbol}} ||
        throw(MethodError(withfaces, (f, keyvals_itr)))
    current = FACES.current[]
    function resolve(new)
        face = if new isa Symbol lookmakeface(new) else new end
        get(current, face, face)
    end
    newfaces = copy(current)
    for (key, new) in keyvals_itr
        face = if key isa Symbol lookmakeface(key) else key end
        if new isa Union{Face, Symbol}
            newfaces[face] = resolve(new)
        elseif new isa Vector
            newfaces[face] = Face(inherit = map(resolve, new))
        else
            delete!(newfaces, face)
        end
        checkinherit(newfaces, face, get(newfaces, face, face))
    end
    @with(FACES.current => newfaces, FACES.cache => emptycache(), f())
end

const FaceReplacement = Union{Face, Symbol, Vector{Face}, Vector{Symbol}, Vector{Union{Symbol, Face}}, Nothing}

function withfaces(f, keyvals::Pair{Symbol, <:FaceReplacement}...)
    # Base.depwarn("`withfaces` with `Symbol` face names is deprecated as of v1.14 and will be removed in a future release. \
    #               Instead you should specify the target faces directly as `Face`s (e.g. from `face\"\"`).",
    #                :withfaces)
    withfaces(f, keyvals)
end

withfaces(f, keyvals::Pair{Face, <:FaceReplacement}...) = withfaces(f, keyvals)

withfaces(f) = f()

## Getting the combined face from a set of properties ##

# Putting these inside `getface` causes the julia compiler to box it
_mergedface(face::Face) = get(FACES.current[], face, face)
_mergedface(face::Symbol) = _mergedface(lookmakeface(face))
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
copies refer to colours and inheritance, is looked up as ours. A value of another type is
converted with `convert(Face, face)`.
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
        return convert(Face, face)::Face
    end
    name = if isdefined(other, :FACES) && hasproperty(other.FACES, :names) get(other.FACES.names, face, nothing) end
    named = if !isnothing(name) get(FACES.pool, name, nothing) end
    isnothing(named) || return named
    if hasfield(T, :f) && fieldnames(fieldtype(T, :f)) == fieldnames(FaceDef) &&
        all(splat(samelayout), zip(fieldtypes(fieldtype(T, :f)), fieldtypes(FaceDef)))
        # Same layout: copy the fields, replacing only the nothings and the references to `other`'s faces
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
                         elseif underline isa Union{Nothing, Bool, Symbol}
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
getface(faces) = if isempty(faces) getface() else Face(resolvedef(faces)) end

function resolvedef(faces)
    default = resolvedef(STANDARD_FACES.default)
    merged = mapfoldl(face -> _mergedface(face).f, merge, faces)::FaceDef
    finalcolours(merge(default, merged), default)
end

"""
    resolvedef(annotations::AbstractVector{@NamedTuple{label::Symbol, value}}, cache = FACES.cache[])

Combine all of the `:face` annotations, as with [`getface`](@ref), into a `FaceDef`.
"""
function resolvedef(annotations::AbstractVector{@NamedTuple{label::Symbol, value::V}},
                     cache::FaceCache = FACES.cache[]) where {V}
    faces = (ann.value for ann in annotations if ann.label === :face)
    face = nothing # A single `Face`, the usual case, is resolved without the fold
    for ann in annotations # Rather than over `faces`, which is measurably slower
        ann.label === :face || continue
        isnothing(face) && ann.value isa Face || return resolvedef(faces)
        face = ann.value::Face
    end
    if isnothing(face) resolvedef(STANDARD_FACES.default, cache) else resolvedef(face, cache) end
end

"""
    getface(face::Face, cache = FACES.cache[]) -> Face

Obtain `face` resolved against the current definitions and the default face, via `cache`.
Its colours are resolved to their final values: an `RGBTuple` or a base colour face.
"""
function getface(face::Face, cache::FaceCache = FACES.cache[])
    def = resolvedef(face, cache)
    if def === face.f face else Face(def) end # Keeps an unchanged face's identity, and so its name
end

# Inlined, so a hit isn't copied out of a call
@inline function resolvedef(face::Face, cache::FaceCache = FACES.cache[])
    mixed = UInt(pointer_from_objref(face)) * 0x9e3779b97f4a7c15 # 64-bit golden ratio factor
    i, j = Int(mixed >> 56) + 1, Int(mixed >> 48 & 0xff) + 1
    for slot in (i, j)
        version = @atomic :acquire cache.versions[slot]
        cache.keys[slot] === face || continue
        def = cache.defs[slot]
        Core.Intrinsics.atomic_fence(:acquire, :system) # Completes the copy before the recheck
        iseven(version) && version === @atomic(:monotonic, cache.versions[slot]) && return def
    end
    resolvemiss(face, cache, i, j, isodd(mixed >> 40))
end

@noinline function resolvemiss(face::Face, cache::FaceCache, i::Int, j::Int, prefer_i::Bool)
    current = FACES.current[]
    def = if face === STANDARD_FACES.default
        finalcolours(get(current, face, face).f, face.f)
    else
        default = resolvedef(STANDARD_FACES.default, cache)
        finalcolours(merge(default, get(current, face, face).f), default)
    end
    at = if cache.keys[i] === UNCACHED || cache.keys[j] !== UNCACHED && prefer_i i else j end
    writeslot!(cache, at, face, def)
    def
end

getface(face::Symbol) = getface(lookmakeface(face))

"""
    getface()

Obtain the default face.
"""
getface() = getface(STANDARD_FACES.default)

## Face/AnnotatedString integration ##

"""
    getface(s::AnnotatedString, i::Integer)

Get the merged [`Face`](@ref) that applies to `s` at index `i`.
"""
getface(s::AnnotatedString, i::Integer) =
    getface([value for (; label, value) in annotations(s, i) if label === :face])

"""
    getface(c::AnnotatedChar)

Get the merged [`Face`](@ref) that applies to `c`.
"""
getface(c::AnnotatedChar) = getface([value for (; label, value) in c.annotations if label === :face])

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
    setface!(original::Face => update::Face, [theme::Symbol = :base]) -> Union{Face, Nothing}

Change `original` by layering `update` over its current definition, as `override` does, for
`theme` (`:base`, `:light`, or `:dark`). The new definition is returned, or `nothing` when
`theme` is not the active theme.

# Examples

```jldoctest; setup = :(import StyledStrings: Face, setface!)
julia> setface!(face"red" => Face(foreground=0xff0000))
Face (sample)
    foreground: #ff0000
```
"""
function setface!((original, update)::Pair{Face, Face}, theme::Symbol = :base)
    @lock FACES.lock begin
        isactive = theme ∈ (:base, FACES.current_theme[])
        RECOLORING[] && !isactive && return # Hooks run again on each theme change
        current = FACES.current[]
        checkinherit(current, original, update)
        if FACES.current.default === current # Only save top-level modifications
            layer = if RECOLORING[] FACES.recolors else FACES.modifications[theme] end
            prior = get(layer, original, nothing)
            layer[original] = if isnothing(prior) update else override(prior, update) end
            relayer!(original)
        elseif isactive
            current[original] = override(get(current, original, original), update)
        end
        if isactive
            emptycache!(FACES.cache[])
            get(current, original, original)
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
    resetfaces!(name)
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
    namebyte(attr::Symbol, str::String) = something(attrbyte(attr, Symbol(str)), weaknothing(UInt8))
    font = safeget(spec, String, "font")
    height = let h = get(spec, "height", nothing)
        if isnothing(h)
            weaknothing(UInt32)
        elseif h isa String && h == "inherit"
            strongnothing(UInt32)
        elseif h isa Union{Int, Float64}
            something(heightbits(h), weaknothing(UInt32))
        else
            weaknothing(UInt32)
        end
    end
    weight = if haskey(spec, "weight") && spec["weight"] isa String
        if spec["weight"]::String == "inherit"
            strongnothing(UInt8)
        else
            namebyte(:weight, spec["weight"]::String)
        end
    elseif haskey(spec, "bold") && spec["bold"] isa Bool
        ifelse(spec["bold"]::Bool, attrbyte(:weight, :bold), attrbyte(:weight, :normal))
    else
        weaknothing(UInt8)
    end
    slant = if haskey(spec, "slant") && spec["slant"] isa String
        if spec["slant"]::String == "inherit"
            strongnothing(UInt8)
        else
            namebyte(:slant, spec["slant"]::String)
        end
    elseif haskey(spec, "italic") && spec["italic"] isa Bool
        ifelse(spec["italic"]::Bool, attrbyte(:slant, :italic), attrbyte(:slant, :normal))
    else
        weaknothing(UInt8)
    end
    foreground = safeget(spec, SimpleColor, "foreground", "fg")
    background = safeget(spec, SimpleColor, "background", "bg")
    ul, ulstyle = if !haskey(spec, "underline")
        WeakNothing(), weaknothing(UInt8)
    elseif spec["underline"] === true
        WeakNothing(), attrbyte(:underline, :straight)
    elseif spec["underline"] === false
        BASE_FACES.foreground, NO_UNDERLINE
    elseif spec["underline"] isa String
        if spec["underline"]::String == "inherit"
            StrongNothing(), strongnothing(UInt8)
        else
            colorvalue(spec["underline"]::String), attrbyte(:underline, :straight)
        end
    elseif spec["underline"] isa Vector{String} && length(spec["underline"]::Vector{String}) == 2
        color_str, style_str = (spec["underline"]::Vector{String})
        if color_str == "inherit" StrongNothing() else colorvalue(color_str) end,
        something(attrbyte(:underline, Symbol(style_str)), attrbyte(:underline, :straight))
    else
        WeakNothing(), weaknothing(UInt8)
    end
    strikethrough = safeget(spec, Bool, "strikethrough")
    inverse = safeget(spec, Bool, "inverse")
    inherit = if !haskey(spec, "inherit")
        Face[]
    elseif spec["inherit"] isa String
        [lookmakeface(registrykey(spec["inherit"]::String))]
    elseif spec["inherit"] isa Vector{String}
        [lookmakeface(registrykey(name)) for name in spec["inherit"]::Vector{String}]
    else
        Face[]
    end
    Face(FaceDef(font, foreground, background, ul, height,
                 weight, slant, ulstyle, strikethrough, inverse, inherit.ref.mem))
end

## Recolouring ##

const recolor_hooks = Function[]
const RECOLORING = ScopedValue(false) # Whether `setface!` is called from a recolor hook

"""
    recolor(f::Function)

Register a hook function `f` to be called now, and again whenever the colors change.

An error from the first call propagates, and `f` is not registered. Errors from
later calls are logged.

These hooks enable dynamic retheming, but are specifically *not* run when faces
are changed. Faces set with `setface!` from a hook sit in between the default
faces and the modifications layered on top by other calls to `setface!` and user
customisations.
"""
function recolor(f::Function)
    @lock FACES.lock begin
        load_customisations!() # Were the hook to load them, they would be filed as its recolours
        @with RECOLORING => true f()
        push!(recolor_hooks, f)
    end
    nothing
end

"""
    relayer!(face::Face, current = FACES.current.default; modified = true)

Recompute the definition of `face` in `current` from its layers, leaving out the
modifications unless `modified`. The caller clears the face cache once its batch is done.
"""
function relayer!(face::Face, current::IdDict{Face, Face} = FACES.current.default; modified::Bool = true)
    theme = FACES.current_theme[]
    replacement = get(FACES.displacements, face, nothing)
    if isnothing(replacement)
        delete!(current, face)
    else
        current[face] = Face(inherit = replacement)
    end
    function layer!(table)
        update = get(table, face, nothing)
        isnothing(update) && return
        current[face] = override(get(current, face, face), update)
    end
    theme === :base || layer!(FACES.themes[theme])
    layer!(FACES.recolors)
    modified || return
    layer!(FACES.modifications.base)
    theme === :base || layer!(FACES.modifications[theme])
end

function relayer!(current::IdDict{Face, Face} = FACES.current.default; modified::Bool = true)
    empty!(current)
    # Every face with a layer, as `relayer!(face)` picks the layers that apply
    for table in (FACES.themes..., FACES.recolors, FACES.modifications..., FACES.displacements)
        foreach(face -> relayer!(face, current; modified), keys(table))
    end
end

"""
    setcolors!(colors::Vector{Pair{Symbol, RGBTuple}})

Update the known base colors with those in `colors`, and recalculate current faces.

`color` should be a complete list of known colours. If `:foreground` and
`:background` are both specified, the faces in the light/dark theme will be
loaded. Otherwise, only the base theme will be applied.
"""
function setcolors!(colors::Vector{Pair{Symbol, RGBTuple}})
    @lock FACES.lock begin
        # Make sure we've loaded customisations before re-layering them.
        load_customisations!()
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
        empty!(FACES.recolors)
        relayer!()
        @with RECOLORING => true for hook in recolor_hooks
            try
                Base.invokelatest(hook)
            catch err
                @error "Recolor hook failed" hook exception = (err, catch_backtrace())
            end
        end
        emptycache!(FACES.cache.default)
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
    original = face
    face = get(current, original, original)
    face.f.foreground === original && return original # A base colour already, the usual case
    for s in stamina:-1:1 # Do this instead of a while loop to prevent cyclic lookups
        fg = face.f.foreground
        if isnothingflavour(fg)
            for iface in face.f.inherit
                irgb = finalcolor(iface, s - 1)
                !isnothing(irgb) && return irgb
            end
            return nothing
        elseif fg isa RGBTuple
            return fg
        else # fg isa Face
            face = get(current, fg, fg)
            face.f.foreground === fg && return fg
        end
    end
end

# `face` with its colours followed to their final values. Those shared with the resolved
# `default` are final already, an unset one is the default's, and one that cannot be resolved is kept.
function finalcolours(face::FaceDef, default::FaceDef)
    final(c, dc) = if isnothingflavour(c) dc elseif c isa Face && c !== dc something(finalcolor(c), c) else c end
    (; font, foreground, background, underline, height, weight, slant,
     underline_style, strikethrough, inverse, inherit) = face
    fg = final(foreground, default.foreground)
    bg = final(background, default.background)
    ul = final(underline, default.underline)
    fg === foreground && bg === background && ul === underline && return face
    FaceDef(font, fg, bg, ul, height, weight, slant, underline_style, strikethrough, inverse, inherit)
end

function finalcolor(color::SimpleColor)
    value = color.value
    value isa RGBTuple && return value
    finalcolor(value)
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

rgbcolor(color::SimpleColor) = if color.value isa RGBTuple color.value else rgbcolor(color.value) end

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
    if !isnothing(face) rgbcolor(face) else UNRESOLVED_COLOR_FALLBACK end
end

"""
    blend(a, b, α::Real) -> SimpleColor
    blend(base, [b => α::Real]...) -> SimpleColor
    blend(a => wa::Real, [b => wb::Real]...) -> SimpleColor

Blend colors in Oklab space. Each color is a `SimpleColor`, or a `Face` whose
foreground color is used.

The mix ratio `α` (0–1) combines `(1 - α)` of `a` with `α` of `b`. Several colors
can be mixed into `base` with `b => α` pairs, and `base` takes the remaining
weight. When every color has a weight, the weights are used as given.

# Examples

```julia-repl
julia> blend(SimpleColor(0xff0000), SimpleColor(0x0000ff), 0.5)
SimpleColor(■ #8b54a1)

julia> blend(face"red", face"yellow", 0.7)
SimpleColor(■ #d47f24)

julia> blend(face"green", SimpleColor(0xffffff), 0.3)
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
    blend(base => 1.0 - sum(last, primaries; init = 0.0), primaries...)

blend((c0, w0)::Pair{<:Union{Symbol, Face, SimpleColor}, <:Real}, primaries::Pair{<:Union{Symbol, Face, SimpleColor}, <:Real}...) =
    SimpleColor(blend(rgbcolor(c0) => w0, (rgbcolor(c) => w for (c, w) in primaries)...))

blend(base::Union{Symbol, Face, SimpleColor}, primaries::Pair{<:Union{Symbol, Face, SimpleColor}, <:Real}...) =
    SimpleColor(blend(rgbcolor(base), (rgbcolor(c) => w for (c, w) in primaries)...))

blend(a::Union{Symbol, Face, SimpleColor}, b::Union{Symbol, Face, SimpleColor}, α::Real) =
    blend(a => 1 - α, b => α)
