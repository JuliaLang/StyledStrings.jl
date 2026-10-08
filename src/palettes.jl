# This file is a part of Julia. License is MIT: https://julialang.org/license

const MAGIC_DEFPALETTE_VARNAME = Symbol("##styledstrings-defpalette-variable#")
const MAGIC_USEPALETTE_VARNAME = Symbol("##styledstrings-usepalette-variable#")

const var"##styledstrings-defpalette-variable#" =
    (; namespace = "", base = STANDARD_FACES,
     light = (; region = FACES.themes.light[STANDARD_FACES.region]),
     dark = (; region = FACES.themes.dark[STANDARD_FACES.region]))

# A height of 0xff8... is invalid, and so can be used as a flag.
const UNDEF_CUSTOM_HEIGHT_FLAG = 0xff800001
const UNDEF_INUSE_HEIGHT_FLAG = 0xff800002

const FACE_KEYWORDS = (:font, :height, :weight, :slant, :foreground, :fg, :background, :bg,
                       :underline, :strikethrough, :inverse, :inherit)

struct UnknownFaceError <: Exception
    context::Module
    name::Symbol
end

function Base.showerror(io::IO, e::UnknownFaceError)
    isambiguous(e.context, e.name) &&
        return print(io, "Face '", e.name, "' is ambiguous in module ", e.context, ", as the palettes it uses give it to more than ",
                     "one face. Qualify it, or use one of the palettes under another name, as `@usepalette Foo: Foo as F`.")
    print(io, "Unknown face '", e.name, "' in module ", e.context, '.')
    suggestion = similarface(e.context, e.name)
    isnothing(suggestion) || print(io, " Did you mean '", suggestion, "'?")
    hasdefs = isdefined(e.context, MAGIC_DEFPALETTE_VARNAME)
    if hasdefs
        println(io, " Faces defined in the module:")
        for name in keys(getglobal(e.context, MAGIC_DEFPALETTE_VARNAME).base)
            println(io, "  - ", name)
        end
    end
    if isdefined(e.context, MAGIC_USEPALETTE_VARNAME)
        println(io, " Palettes used:")
        for text in getglobal(e.context, MAGIC_USEPALETTE_VARNAME).texts
            println(io, "  - ", text)
        end
    elseif !hasdefs
        print(io, " Only the standard faces are available, as ", e.context, " defines no palette and uses none.")
    end
end

# The first `depth` components of `path` name modules or named palettes, and the next does not
struct FacePathError <: Exception
    context::Module
    path::String
    depth::Int
end

function Base.showerror(io::IO, e::FacePathError)
    names = split(e.path, '.')
    if e.depth == length(names) - 1
        print(io, "`", join(names[1:e.depth], '.'), "` has no face named '", last(names), "'.")
    else
        print(io, "Cannot find the face '", e.path, "' from module ", e.context, ": `",
              join(names[1:e.depth+1], '.'), "` is not a module or named palette. Dotted face names are module paths.")
    end
    if haskey(FACES.pool, registrykey(e.path))
        print(io, " The face is registered as '", registrykey(e.path), "'; refer to it by the path of the ",
              "module that defines it, or by '", e.path, "' once that module's palette is used (see `@usepalette`).")
    end
end

registrykey(name::AbstractString) = Symbol(replace(name, '.' => '_'))

function paletteface(mod::Module, name::Symbol)
    if isdefined(mod, MAGIC_DEFPALETTE_VARNAME) && haskey(getglobal(mod, MAGIC_DEFPALETTE_VARNAME).base, name)
        getproperty(getglobal(mod, MAGIC_DEFPALETTE_VARNAME).base, name)::Face
    elseif isdefined(mod, MAGIC_USEPALETTE_VARNAME)
        get(getglobal(mod, MAGIC_USEPALETTE_VARNAME).faces, name, nothing)::Union{Face, Nothing}
    end
end

# Whether more than one palette that `mod` uses provides `name`, which is then held as `nothing`
isambiguous(mod::Module, name::Symbol) =
    isdefined(mod, MAGIC_USEPALETTE_VARNAME) && get(getglobal(mod, MAGIC_USEPALETTE_VARNAME).faces, name, missing) === nothing

# An ambiguous name must not fall back to a registered face of that name, such as a standard one
lookupface(mod::Module, name::Symbol) =
    @something(paletteface(mod, name), if !isambiguous(mod, name) get(FACES.pool, name, nothing) end,
               throw(UnknownFaceError(mod, name)))

# For a macro to emit. A face only the registry holds (as from `addface!`) is looked up
# when the code runs, as precompiled code would otherwise hold a copy of it.
function faceref(mod::Module, name::Symbol)
    face = paletteface(mod, name)
    isnothing(face) || return face
    registered = if !isambiguous(mod, name) get(FACES.pool, name, nothing) end
    if isnothing(registered) || any(f -> f === registered, STANDARD_FACES)
        registered
    else
        Expr(:call, lookmakeface, QuoteNode(name))
    end
end

function pathface(mod::Module, path::String)
    used = Symbol(path) # A name of a used palette, as `Name.face` or `namespace.face`
    face = paletteface(mod, used)
    isnothing(face) || return face
    isambiguous(mod, used) && return UnknownFaceError(mod, used)
    names = map(Symbol, eachsplit(path, '.'))
    holder::Any = mod
    for (depth, name) in enumerate(names[1:end-1])
        holder = if holder isa Module && isdefined(holder, name) getglobal(holder, name) end
        ispalette = holder isa Module || holder isa NamedTuple && haskey(holder, MAGIC_DEFPALETTE_VARNAME)
        ispalette || return FacePathError(mod, path, depth - 1)
    end
    palette = definedpalette(holder)
    face = if !isnothing(palette) get(palette.base, last(names), nothing) end
    @something(face, FacePathError(mod, path, length(names) - 1))
end

# The palette that a module or named palette defines, if any
function definedpalette(holder)
    if holder isa Module && isdefined(holder, MAGIC_DEFPALETTE_VARNAME)
        getglobal(holder, MAGIC_DEFPALETTE_VARNAME)
    elseif holder isa NamedTuple
        get(holder, MAGIC_DEFPALETTE_VARNAME, nothing)
    end
end

function similarface(mod::Module, name::Symbol)
    function editdistance(a::String, b::String)
        row = collect(0:length(b))
        for (i, ca) in enumerate(a)
            diag, row[1] = row[1], i
            for (j, cb) in enumerate(b)
                diag, row[j+1] = row[j+1], min(row[j+1] + 1, row[j] + 1, diag + (ca != cb))
            end
        end
        last(row)
    end
    candidates = collect(Symbol, keys(STANDARD_FACES))
    isdefined(mod, MAGIC_DEFPALETTE_VARNAME) && append!(candidates, keys(getglobal(mod, MAGIC_DEFPALETTE_VARNAME).base))
    isdefined(mod, MAGIC_USEPALETTE_VARNAME) && # Not those that are ambiguous
        append!(candidates, (name for (name, face) in getglobal(mod, MAGIC_USEPALETTE_VARNAME).faces if !isnothing(face)))
    target = String(name)
    distance, best = minimum(c -> (editdistance(String(c), target), c), candidates)
    if distance <= max(1, length(target) ÷ 3) best end
end

lookmakeface(mod::Module, name::Symbol) = @something(paletteface(mod, name), lookmakeface(name))

function lookmakeface(name::Symbol, use::Bool = true)
    @something(get(FACES.pool, name, nothing),
               mkunregisteredface(name, use))
end

function mkunregisteredface(name::Symbol, use::Bool)
    @lock FACES.lock begin
        registered = get(FACES.pool, name, nothing) # The name may have been registered since the unlocked check
        isnothing(registered) || return registered
        existing = get(FACES.unregistered, name, nothing)
        !isnothing(existing) && (!use || getfield(existing, :f).height == UNDEF_INUSE_HEIGHT_FLAG) &&
            return existing
        uface = Face(FaceDef(
            WeakNothing(), WeakNothing(), WeakNothing(), WeakNothing(),
            ifelse(use, UNDEF_INUSE_HEIGHT_FLAG, UNDEF_CUSTOM_HEIGHT_FLAG),
            weaknothing(UInt8), weaknothing(UInt8), weaknothing(UInt8),
            weaknothing(UInt8), weaknothing(UInt8), Memory{Face}()))
        if !isnothing(existing)
            # 'Upgrade' a customisation-only face to an in-use face
            register_displace!(existing, uface, name)
            relayer!(uface)
        end
        FACES.unregistered[name] = uface
        FACES.names[uface] = name
        uface
    end
end

function facename(mod::Module, face::Face)
    if isdefined(mod, MAGIC_DEFPALETTE_VARNAME)
        for (name, f) in pairs(getglobal(mod, MAGIC_DEFPALETTE_VARNAME).base)
            f === face && return name
        end
    end
    if isdefined(mod, MAGIC_USEPALETTE_VARNAME)
        visible = [name for (name, f) in getglobal(mod, MAGIC_USEPALETTE_VARNAME).faces
                   if f === face && paletteface(mod, name) === face] # Not shadowed by the module's own
        isempty(visible) || return argmin(name -> ('.' in String(name), name), visible) # Short names first
    end
    get(FACES.names, face, nothing)
end

# Macros

"""
    face"<name>" -> Face

Obtain the `Face` identified in the current scope by a certain name.

Basic faces are available, as well as any pulled in with [`@usepalette`](@ref) or
defined with [`@defpalette`](@ref), which take their place where they share a name.

!!! compat "StyledStrings 1.14"
    This macro was introduced in the v1.14 release of StyledStrings/Julia.
    It supplants the previous use of `Symbol`s to name faces.
"""
macro face_str(name::String)
    isempty(name) && return Face()
    face = if '.' in name
        pathface(__module__, name)
    else
        @something(faceref(__module__, Symbol(name)), UnknownFaceError(__module__, Symbol(name)))
    end
    face isa Exception && throw(face)
    face
end

"""
    @defpalette begin ... end

Define a palette for the current module. The faces named by this palette can then be used with [`face""`](@ref @face_str),
and are preferentially used over any defined by [`@usepalette`](@ref).

Within a `@defpalette` block, faces (referenced as foreground, background,
underline, or inherit attributes) should be referred to as variables, without
any decoration. For instance, `blue` should be used over `face"blue"`. Colours
may also be given as literals, and any value as a `\$(...)` expression.

A face can also have light and dark variants, written as `name.light` and
`name.dark`. A variant is layered over the base face when the terminal has a
light or dark background. It may refer to other faces, but not to its own base.

Every face referred to must be known when the palette is defined: defined in the
palette, pulled in with `@usepalette`, or a standard face. Faces of other
palettes can also be referred to by their module path, as `Module.name`.

Cyclic dependencies between faces (e.g. two faces inheriting from each other)
are not possible, but the order of declaration is automatically determined.

# Examples

```julia
@defpalette begin
    important = Face(weight = :bold, inherit = warning)
    topic = Face(foreground = blue)
    heading = Face(foreground = important, background = 0xf0f0f0)
    heading.dark = Face(background = 0x303030)
end
```
"""
macro defpalette(pargs::Any...)
    nsmodule = __module__
    namespace = ""
    # Apply keyword arguments
    decls = collect(pargs)
    for (i, decl) in Iterators.reverse(enumerate(decls))
        if Meta.isexpr(decl, :(=), 2)
            key, val = decl.args
            if key == :namespace
                namespace = if val isa QuoteNode
                    String(val.value)
                elseif Meta.isexpr(val, :call) && first(val.args) == :Face
                    continue
                else
                    nsval = Core.eval(__module__, val)
                    nsval isa Module || throw(ArgumentError("Invalid @defpalette argument `$decl`, namespace must be a Symbol or Module."))
                    nsmodule = nsval
                    "" # Derived from the module's path below
                end
            else
                throw(ArgumentError("Invalid @defpalette argument `$decl`."))
            end
            deleteat!(decls, i)
        end
    end
    # Determine namespace
    if isempty(namespace)
        parents = Module[nsmodule]
        while parentmodule(first(parents)) != first(parents)
            pushfirst!(parents, parentmodule(first(parents)))
        end
        namespace = join(map(String ∘ nameof, parents), '.')
    end
    # Optional variable name
    varname = if !isempty(decls) && first(decls) isa Symbol
        var = first(decls)
        deleteat!(decls, 1)
        namespace *= '.' * String(var)
        var
    end
    # Unwrap block
    if length(decls) == 1 && Meta.isexpr(decls[1], :block)
        decls = decls[1].args
    end
    # Parse declarations
    parsed = Dict{@NamedTuple{name::Symbol, theme::Symbol}, @NamedTuple{i::Int, args::Vector{Pair{Symbol, Any}}, deps::Vector{Symbol}, line::Union{LineNumberNode, Nothing}}}()
    lastline = nothing
    isname(x) = x isa Symbol && x !== :nothing
    isliteral(x) = x === :nothing || x isa Unsigned || x isa AbstractString
    for (i, decl) in enumerate(decls)
        if decl isa LineNumberNode
            lastline = decl
            continue
        end
        Meta.isexpr(decl, :(=), 2) || throw(ArgumentError("Invalid @defpalette argument `$decl`, should be of the form `name = Face(...)`."))
        name, theme = if decl.args[1] isa Symbol
            decl.args[1], :base
        elseif Meta.isexpr(decl.args[1], :(.), 2)
            n, t = decl.args[1].args
            n isa Symbol || throw(ArgumentError("Invalid @defpalette argument `$decl`, name (`$n`) must be a Symbol."))
            t isa QuoteNode || throw(ArgumentError("Invalid @defpalette argument `$decl`, theme (`$t`) must be a Symbol."))
            n, t.value
        end
        theme ∈ (:base, :light, :dark) || throw(ArgumentError("Invalid @defpalette argument `$decl`, specifies theme '$theme' but theme must be light or dark."))
        haskey(parsed, (; name, theme)) && throw(ArgumentError("Duplicate @defpalette face declaration `$name$(ifelse(theme == :base, "", ".$theme"))`."))
        facecall = decl.args[2]
        Meta.isexpr(facecall, :call) && facecall.args[1] == :Face || throw(ArgumentError("Invalid @defpalette argument $decl, value (`$facecall`) must be a `Face(...)` expression."))
        faceargs = Pair{Symbol, Any}[]
        deps = Symbol[]
        for arg in facecall.args[2:end]
            Meta.isexpr(arg, :kw, 2) || throw(ArgumentError("Invalid Face argument `$arg`."))
            k, v = arg.args
            written = string(Expr(:(=), k, v))
            k ∈ FACE_KEYWORDS || throw(ArgumentError(
                "Invalid Face argument `$written`, as `$k` is not one of $(join(FACE_KEYWORDS, ", ", ", or "))."))
            if k == :fg
                k = :foreground
            elseif k == :bg
                k = :background
            end
            if Meta.isexpr(v, :$, 1) # Kept as written, and unwrapped when the value is emitted
                push!(faceargs, k => v)
                continue
            end
            if k ∈ (:foreground, :background)
                isname(v) && push!(deps, v)
                isname(v) || isliteral(v) || Meta.isexpr(v, :., 2) || throw(ArgumentError("Invalid Face argument `$written`, $k color value (`$v`) must be a face name, a color literal, or a `\$(...)` expression."))
            elseif k == :inherit
                if v isa Symbol
                    push!(deps, v)
                elseif Meta.isexpr(v, :., 2)
                elseif Meta.isexpr(v, :vect)
                    for f in v.args
                        Meta.isexpr(f, :., 2) && continue
                        f isa Symbol || throw(ArgumentError("Invalid Face argument `$written`, inherit value (`$f`) must be a variable name."))
                        push!(deps, f)
                    end
                else
                    throw(ArgumentError("Invalid Face argument `$written`, inherit value (`$v`) must be a face name or a vector of face names."))
                end
            elseif k == :underline
                if isname(v)
                    push!(deps, v)
                elseif Meta.isexpr(v, :tuple, 2) && isname(v.args[1])
                    push!(deps, v.args[1])
                elseif v isa Signed
                    throw(ArgumentError("Invalid Face argument `$written`, underline color value (`$v`) must be a face name, a color literal, or a `\$(...)` expression."))
                end
            end
            push!(faceargs, k => v)
        end
        if theme != :base && name in deps
            throw(ArgumentError("The $theme variant of face '$name' refers to '$name'. A variant is layered over its base face, so it cannot refer to it."))
        end
        parsed[(; name, theme)] = (; i, args = faceargs, deps, line = lastline)
    end
    # Find all defined base faces, and prune dependencies to only those
    allnames = Set(name for (; name, theme) in keys(parsed) if theme == :base)
    for (_, info) in parsed
        filter!(dep -> dep in allnames, info.deps)
    end
    # Topologically sort declarations
    faceorder = Symbol[]
    revdeps = Dict(name => Symbol[] for ((; name, theme), _) in parsed if theme == :base)
    for (label, info) in parsed
        label.theme == :base || continue
        if isempty(info.deps)
            push!(faceorder, label.name)
        else
            for dep in info.deps
                push!(revdeps[dep], label.name)
            end
        end
    end
    sort!(faceorder, by = x -> parsed[(; name = x, theme = :base)].i)
    hoistfaces = Dict{Symbol, Symbol}()
    for name in faceorder, rdep in revdeps[name]
        get!(() -> gensym("$(name)_face"), hoistfaces, name)
        depfaces = parsed[(; name = rdep, theme = :base)].deps
        ind = findfirst(==(name), depfaces)::Int
        isempty(deleteat!(depfaces, ind)) && push!(faceorder, rdep)
    end
    length(faceorder) == length(allnames) ||
        throw(ArgumentError("Cyclic face dependencies detected in @defpalette declaration: $(join(setdiff(allnames, faceorder), ", "))."))
    # Faces a theme variant refers to must be hoisted too
    for ((; theme), (; deps)) in parsed
        theme == :base && continue
        foreach(dep -> get!(() -> gensym("$(dep)_face"), hoistfaces, dep), deps)
    end
    # Rewrite arguments
    function faceorlookup(f) # A single method, so the binding is not boxed
        if f === :nothing
            nothing
        elseif f isa Symbol
            ref = @something(get(hoistfaces, f, nothing), faceref(__module__, f), Some(nothing))
            # A palette is built while precompiling, where a face only the registry holds is a copy
            ref isa Union{Symbol, Face} || throw(UnknownFaceError(__module__, f))
            ref
        elseif Meta.isexpr(f, :., 2)
            face = pathface(__module__, string(f))
            face isa Exception && throw(face)
            face
        elseif Meta.isexpr(f, :$, 1)
            f.args[1]
        elseif f isa Unsigned || f isa AbstractString
            f
        else
            throw(ArgumentError("Invalid face reference expression `$f`."))
        end
    end
    for (; args) in values(parsed)
        for (i, (arg, value)) in enumerate(args)
            args[i] = if arg == :foreground
                :foreground => faceorlookup(value)
            elseif arg == :background
                :background => faceorlookup(value)
            elseif arg == :inherit
                :inherit => if Meta.isexpr(value, :vect)
                    Expr(:ref, Face, map(faceorlookup, value.args)...)
                else
                    faceorlookup(value)
                end
            elseif arg == :underline
                if Meta.isexpr(value, :tuple, 2)
                    :underline => Expr(:tuple, faceorlookup(value.args[1]), value.args[2])
                elseif value isa Symbol || value isa Expr
                    :underline => faceorlookup(value)
                else
                    args[i]
                end
            elseif Meta.isexpr(value, :$, 1)
                arg => value.args[1]
            else
                args[i]
            end
        end
    end
    # Create an ordered version of `parsed`
    orderlookup = Dict(name => i for (i, name) in enumerate(faceorder))
    parsedordered = Vector{eltype(parsed)}()
    for (k, v) in parsed
        v = Base.setindex(v, get(orderlookup, k.name, v.i + length(parsed)), :i)
        push!(parsedordered, k => v)
    end
    sort!(parsedordered; by = ((_, v),) -> v.i)
    # Construct final expression
    decls = (base = Union{Expr, LineNumberNode}[],
             light = Union{Expr, LineNumberNode}[],
             dark = Union{Expr, LineNumberNode}[])
    # Copied, as `Face()` is shared and each palette face must be distinct
    newface(args) = Expr(:call, copy, Expr(:call, Face, Expr(:parameters, (Expr(:kw, k, v) for (k, v) in args)...)))
    for ((; name, theme), (; args)) in parsedordered
        # isnothing(line) || push!(decls[theme], line)
        hoistname = get(hoistfaces, name, nothing)
        if theme == :base
            push!(decls.base, Expr(:kw, name, @something(hoistname, newface(args))))
        elseif name ∉ allnames
            throw(ArgumentError("A $theme variant of face '$name' is declared, without a base variant. Consider adding `$name = Face()` to the palette."))
        else
            push!(decls[theme], Expr(:kw, name, newface(args)))
        end
    end
    declsnt = Expr(:parameters, Expr(:kw, :namespace, namespace))
    for (theme, body) in pairs(decls)
        push!(declsnt.args, Expr(:kw, theme, Expr(:tuple, Expr(:parameters, body...))))
    end
    declsnt = Expr(:tuple, declsnt)
    if !isempty(hoistfaces)
        fhoist = Expr[]
        for ((; name, theme), (; args)) in parsedordered
            theme == :base && haskey(hoistfaces, name) || continue
            push!(fhoist, Expr(:(=), hoistfaces[name], newface(args)))
        end
        declsnt = Expr(:let, Expr(:block), Expr(:block, fhoist..., declsnt))
    end
    definition, palette = if isnothing(varname)
        :(const $MAGIC_DEFPALETTE_VARNAME = $declsnt), MAGIC_DEFPALETTE_VARNAME
    else
        :(const $varname = (; $MAGIC_DEFPALETTE_VARNAME = $declsnt)), :($varname.$MAGIC_DEFPALETTE_VARNAME)
    end
    esc(Expr(:toplevel, definition, :($reregister_palette!($palette)), something(varname, MAGIC_DEFPALETTE_VARNAME)))
end

"""
    @usepalette Module, ...
    @usepalette Module: face, face as name, Module as Name, ...

Use the palettes of other modules, as `using` uses their names. The faces of a palette
used whole can then be named with [`face""`](@ref) and in styled markup both directly, as
`accent`, and by the namespace the palette declares, as `ns.accent`. With a list after `:`,
only the listed faces are used, under the names given, and the module's own name stands
for its palette, reached only through that name, as `Name.accent`.

Each use adds to the module's earlier ones. A name that more than one used palette
provides is ambiguous, and an error where it is used: qualify it, or use one of the
palettes under another name. A standard face is used only where no palette provides its name.

# Examples

```julia
@usepalette ColorsA, ColorsB
@usepalette Highlights: Highlights as HL
@usepalette Icons: arrow, check as tick
```
"""
macro usepalette(args::Union{Expr, Symbol}...)
    isempty(args) && throw(ArgumentError("@usepalette needs at least one module, as in `@usepalette Module`."))
    # As with `using`, commas separate uses, grouping them into tuples around any `as`
    all(i -> args[i] === :as || args[i+1] === :as, 1:length(args)-1) ||
        throw(ArgumentError("Uses in @usepalette are separated by commas, as in `@usepalette A, B`."))
    items = mapreduce(arg -> if Meta.isexpr(arg, :tuple) arg.args else Any[arg] end, vcat, args)
    named = Pair{Any, Union{Symbol, Nothing}}[] # Each use or face, with the name `as` gives it
    i = 1
    while i <= length(items)
        item, as = items[i], nothing
        if get(items, i + 1, nothing) === :as
            as = get(items, i + 2, :as) # So a missing name is caught as `as` itself would be
            i += 2
        end
        (item === :as || as === :as || !(as isa Union{Symbol, Nothing})) &&
            throw(ArgumentError("In @usepalette, `as` follows a module or face, and is followed by a name."))
        push!(named, item => as)
        i += 1
    end
    lead = first(named).first
    islist(item) = Meta.isexpr(item, :call, 3) && item.args[1] === :(:)
    astext((item, as)) = if isnothing(as) "$item" else "$item as $as" end
    uses = if islist(lead) # `Module: face, face as name, Module as Name...`
        source, face = lead.args[2], lead.args[3]
        named[1] = face => last(named[1]) # The face, without the `Module:` before it
        all(face isa Symbol for (face, _) in named) ||
            throw(ArgumentError("In @usepalette, the faces after `$source:` are names, as in `@usepalette $source: a, b`."))
        text = "$source: " * join(map(astext, named), ", ")
        modname = if source isa Symbol source else source.args[end].value end # As `using A.B: B` names `B`
        picks = filter(((face, _),) -> face !== modname, named)
        [[:((; text = $text, source = $source, names = $(QuoteNode(something(as, face)))))
          for (face, as) in named if face === modname]; # The palette as a whole, under that name
         if isempty(picks) Expr[] else [:((; text = $text, source = $source, names = $(QuoteNode(picks))))] end]
    else
        any(islist ∘ first, named) &&
            throw(ArgumentError("In @usepalette, a list after `:` follows the only module used, as in `using`."))
        all(isnothing ∘ last, named) ||
            throw(ArgumentError("In @usepalette, a palette is renamed as `using` renames a module, as in `@usepalette Foo: Foo as F`."))
        [:((text = $(astext(use)), source = $(use.first), names = nothing)) for use in named]
    end
    esc(Expr(:toplevel, :(const $MAGIC_USEPALETTE_VARNAME = $usepalette($__module__, $(uses...))), nothing))
end

# Add the faces `uses` make visible to those of earlier uses
function usepalette(mod::Module, uses::NamedTuple...)
    prior = if isdefined(mod, MAGIC_USEPALETTE_VARNAME)
        getglobal(mod, MAGIC_USEPALETTE_VARNAME)
    else
        (texts = String[], faces = Dict{Symbol, Union{Face, Nothing}}())
    end
    faces = copy(prior.faces)
    # A name given to two different faces is ambiguous, held as `nothing`
    visible!(name, face) = faces[name] = if get(faces, name, face) === face face end
    for (; text, source, names) in uses
        palette = @something(definedpalette(source), throw(ArgumentError(
            "`$text` has no palette to use. A palette is defined with `@defpalette`, \
             and a named palette is used as `Module.<palette name>`.")))
        if names isa Vector # The faces listed, each perhaps `as` a name
            for (face, as) in names
                haskey(palette.base, face) || throw(ArgumentError("`$text`: the palette has no face '$face'."))
                visible!(something(as, face), palette.base[face])
            end
        else # Whole, each name bare and namespaced (both bare, without a namespace), or `as` a name
            prefixes = if isnothing(names); ("", palette.namespace) else (String(names),) end
            for (name, face) in pairs(palette.base), prefix in prefixes
                visible!(if isempty(prefix) name else Symbol(prefix, '.', name) end, face)
            end
        end
    end
    (texts = unique!(vcat(prior.texts, [use.text for use in uses])), faces)
end

"""
    @registerpalette [names...]

Register the palette defined in the current module in the global registry, along
with the palettes `names` defined by `@defpalette name ...`.

This should be placed within the `__init__()` function of a module defining a palette.

Use of `@registerpalette` is essential to make the [`@defpalette`](@ref)-defined
faces available for theming and customisation.

# Examples

```julia
@defpalette begin ... end

function __init__()
    @registerpalette
end
```
"""
macro registerpalette(names::Symbol...)
    @noinline register_palette_warn(f, l) =
        @warn "@registerpalette should only be executed during module initialization, within the __init__() function." _file=f _line=l
    @noinline register_palette_missing(f, l) =
        @warn "@registerpalette was called without a corresponding palette defined (by @defpalette)." _file=f _line=l
    gfaces = GlobalRef(@__MODULE__, :FACES)
    file, line = String(__source__.file), __source__.line
    named = [:($register_palette!($(esc(name)).$MAGIC_DEFPALETTE_VARNAME)) for name in names]
    quote
        if !iszero(ccall(:jl_generating_output, Cint, ())) &&
            @noinline (() -> !any(sf -> sf.func === :__init__, stacktrace(backtrace())))()
            $register_palette_warn($file, $line)
        else
            @lock $gfaces.lock begin
                if isdefined($__module__, $(QuoteNode(MAGIC_DEFPALETTE_VARNAME)))
                    $register_palette!(getglobal($__module__, $(QuoteNode(MAGIC_DEFPALETTE_VARNAME))))
                elseif $(isempty(names))
                    $register_palette_missing($file, $line)
                end
                $(named...)
                $emptycache!($gfaces.cache.default)
            end
        end
        nothing
    end
end

# The caller holds `FACES.lock`, and clears the face cache afterwards
function register_palette!(palette::NamedTuple)
    for (name, face) in pairs(palette.base)
        fullname = registrykey("$(palette.namespace).$name")
        old = get(FACES.unregistered, fullname) do
            get(FACES.pool, fullname, nothing) # Left by an earlier evaluation of the module
        end
        old === face || isnothing(old) || register_displace!(old, face, fullname)
        FACES.pool[fullname] = face
        FACES.names[face] = fullname
    end
    for theme in (:light, :dark), (name, variant) in pairs(palette[theme])
        FACES.themes[theme][palette.base[name]] = variant
    end
    foreach(relayer!, values(palette.base))
end

# For a palette evaluated anew outside of precompilation, as by Revise
function reregister_palette!(palette::NamedTuple)
    Base.generating_output() && return
    isredefined = any(pairs(palette.base)) do (name, face)
        registered = get(FACES.pool, registrykey("$(palette.namespace).$name"), nothing)
        !isnothing(registered) && registered !== face
    end
    isredefined || return
    @lock FACES.lock begin
        register_palette!(palette)
        emptycache!(FACES.cache.default)
    end
end

# Replace `old`, a placeholder or an earlier registration of `fullname`, with `new`. Modifications
# and recolourings move to `new`, as do a placeholder's variants. A placeholder `old`, or the
# placeholders an earlier registration `old` displaced, inherit from `new`. The caller holds
# `FACES.lock`, and relayers `new`.
function register_displace!(old::Face, new::Face, fullname::Symbol)
    delete!(FACES.unregistered, fullname)
    delete!(FACES.names, old)
    placeholder = old.f.height ∈ (UNDEF_CUSTOM_HEIGHT_FLAG, UNDEF_INUSE_HEIGHT_FLAG)
    for tables in (FACES.themes, FACES.modifications, (; FACES.recolors)), table in tables
        row = get(table, old, nothing)
        isnothing(row) && continue
        delete!(table, old)
        if placeholder || tables !== FACES.themes
            table[new] = row
        end
    end
    displaced = if placeholder; [old] else Face[p for (p, target) in FACES.displacements if target === old] end
    for face in displaced
        FACES.displacements[face] = new
        relayer!(face)
    end
end
