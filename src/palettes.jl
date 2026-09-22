# This file is a part of Julia. License is MIT: https://julialang.org/license

const MAGIC_DEFPALETTE_VARNAME = Symbol("##styledstrings-defpalette-variable#")
const MAGIC_USEPALETTE_VARNAME = Symbol("##styledstrings-usepalette-variable#")

const var"##styledstrings-defpalette-variable#" =
    (; base = STANDARD_FACES,
     light = (; region = FACES.themes.light[STANDARD_FACES.region]),
     dark = (; region = FACES.themes.dark[STANDARD_FACES.region]))

# A height of 0xff8... is invalid, and so can be used as a flag.
const UNDEF_CUSTOM_HEIGHT_FLAG = 0xff800001
const UNDEF_INUSE_HEIGHT_FLAG = 0xff800002

struct UnknownFaceError <: Exception
    context::Module
    name::Symbol
end

function Base.showerror(io::IO, e::UnknownFaceError)
    print(io, "Unknown face '", e.name, "' in module ", e.context, '.')
    hasdefs = isdefined(e.context, MAGIC_DEFPALETTE_VARNAME)
    if hasdefs
        println(io, " Faces defined in the module:")
        for name in keys(getglobal(e.context, MAGIC_DEFPALETTE_VARNAME).base)
            println(io, "  - ", name)
        end
    end
    if isdefined(e.context, MAGIC_USEPALETTE_VARNAME)
        println(io, " Imported palettes from:")
        for source in getglobal(e.context, MAGIC_USEPALETTE_VARNAME).sources
            println(io, "  - ", source)
        end
    elseif !hasdefs
        print(io, " No faces are defined or imported.")
    end
end

function findface(mod::Module, name::Symbol)
    if isdefined(mod, MAGIC_DEFPALETTE_VARNAME) && haskey(getglobal(mod, MAGIC_DEFPALETTE_VARNAME).base, name)
        getproperty(getglobal(mod, MAGIC_DEFPALETTE_VARNAME).base, name)::Face
    elseif isdefined(mod, MAGIC_USEPALETTE_VARNAME) && haskey(getglobal(mod, MAGIC_USEPALETTE_VARNAME).base, name)
        getproperty(getglobal(mod, MAGIC_USEPALETTE_VARNAME).base, name)::Face
    else
        get(FACES.pool, name, nothing)
    end
end

lookupface(mod::Module, name::Symbol) =
    @something(findface(mod, name), throw(UnknownFaceError(mod, name)))

function lookmakeface(mod::Module, name::Symbol, use::Bool = true)
    lface = findface(mod, name)
    if !isnothing(lface)
        lface
    else
        mkunregisteredface(name, use)
    end
end

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
        for (name, f) in pairs(getglobal(mod, MAGIC_USEPALETTE_VARNAME).base)
            f === face && return name
        end
    end
    get(FACES.names, face, nothing)
end

# Macros

"""
    face"<name>" -> Face

Obtain the `Face` identified in the current scope by a certain name.

Basic faces are always available, as well as any pulled in with
[`@usepalettes!`](@ref) or defined with [`@defpalette!`](@ref).

!!! compat "StyledStrings 1.14"
    This macro was introduced in the v1.14 release of StyledStrings/Julia.
    It supplants the previous use of `Symbol`s to name faces.
"""
macro face_str(name::String)
    isempty(name) && return Face()
    if '.' in name
        components = map(Symbol, eachsplit(name, '.'))
        push!(components, :base, last(components))
        components[end-2] = MAGIC_DEFPALETTE_VARNAME
        esc(foldl((a, b) -> Expr(:., a, QuoteNode(b)), components[2:end]; init = first(components)))
    else
        lookupface(__module__, Symbol(name))
    end
end

"""
    @defpalette! begin ... end

Define a palette for the current module. This faces named by this palette can then be used with [`face""`](@ref @face_str),
and are preferentially used over any defined by [`@usepalettes!`](@ref).

Within a `@defpalette!` block, faces (referenced as foreground, background,
underline, or inherit attributes) should be referred to as variables, without
any decoration. For instance, `blue` should be used over `face"blue"`. Colours
may also be given as literals, and any value as a `\$(...)` expression.

Cyclic dependencies between faces (e.g. two faces inheriting from each other)
are not possible, but the order of declaration is automatically determined.

# Examples

```julia
@defpalette! begin
    important = Face(weight = :bold, inherit = warning)
    topic = Face(foreground = blue)
    heading = Face(foreground = important, background = 0xf0f0f0)
end
```
"""
macro defpalette!(pargs::Any...)
    nsmodule = Ref(__module__)
    namespace = ""
    # Apply keyword arguments
    decls = collect(pargs)
    for (i, decl) in Iterators.reverse(enumerate(decls))
        if Meta.isexpr(decl, :(=), 2)
            key, val = decl.args
            if key == :namespace
                namespace = if val isa String
                    String(val) * '_'
                elseif val isa QuoteNode
                    String(val.value) * '_'
                elseif Meta.isexpr(val, :call) && first(val.args) == :Face
                    continue
                else
                    nsval = Core.eval(__module__, val)
                    nsval isa Module || throw(ArgumentError("Invalid @defpalette! argument `$decl`, namespace must be a String, Symbol, or Module."))
                    nsmodule[] = nsval
                    "" # Derived from the module's path below
                end
            else
                throw(ArgumentError("Invalid @defpalette! argument `$decl`."))
            end
            deleteat!(decls, i)
        end
    end
    # Determine namespace
    if isempty(namespace)
        parents = Module[nsmodule[]]
        while parentmodule(first(parents)) != first(parents)
            pushfirst!(parents, parentmodule(first(parents)))
        end
        namespace = join(map(String ∘ nameof, parents), '_') * '_'
    end
    # Optional variable name
    varname = if !isempty(decls) && first(decls) isa Symbol
        var = first(decls)
        deleteat!(decls, 1)
        namespace *= String(var) * '_'
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
    isliteral(x) = x === :nothing || x isa Integer || x isa AbstractString
    for (i, decl) in enumerate(decls)
        if decl isa LineNumberNode
            lastline = decl
            continue
        end
        Meta.isexpr(decl, :(=), 2) || throw(ArgumentError("Invalid @defpalette! argument `$decl`, should be of the form `name = Face(...)`."))
        name, theme = if decl.args[1] isa Symbol
            decl.args[1], :base
        elseif Meta.isexpr(decl.args[1], :(.), 2)
            n, t = decl.args[1].args
            n isa Symbol || throw(ArgumentError("Invalid @defpalette! argument `$decl`, name (`$n`) must be a Symbol."))
            t isa QuoteNode || throw(ArgumentError("Invalid @defpalette! argument `$decl`, theme (`$t`) must be a Symbol."))
            n, t.value
        end
        theme ∈ (:base, :light, :dark) || throw(ArgumentError("Invalid @defpalette! argument `$decl`, specifies theme '$theme' but theme must be light or dark."))
        haskey(parsed, (; name, theme)) && throw(ArgumentError("Duplicate @defpalette! face declaration `$name$(ifelse(theme == :base, "", ".$theme"))`."))
        facecall = decl.args[2]
        Meta.isexpr(facecall, :call) && facecall.args[1] == :Face || throw(ArgumentError("Invalid @defpalette! argument $decl, value (`$facecall`) must be a `Face(...)` expression."))
        faceargs = Pair{Symbol, Any}[]
        deps = Symbol[]
        for arg in facecall.args[2:end]
            Meta.isexpr(arg, :kw, 2) || throw(ArgumentError("Invalid Face argument `$arg`."))
            k, v = arg.args
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
                isname(v) || isliteral(v) || Meta.isexpr(v, :., 2) || throw(ArgumentError("Invalid Face argument `$arg`, $k color value (`$v`) must be a face name, a color literal, or a `\$(...)` expression."))
            elseif k == :inherit
                if v isa Symbol
                    push!(deps, v)
                elseif Meta.isexpr(v, :., 2)
                elseif Meta.isexpr(v, :vect)
                    for f in v.args
                        Meta.isexpr(f, :., 2) && continue
                        f isa Symbol || throw(ArgumentError("Invalid Face argument `$arg`, inherit value (`$f`) must be a variable name."))
                        push!(deps, f)
                    end
                else
                    throw(ArgumentError("Invalid Face argument `$arg`, inherit value (`$v`) must be a face name or a vector of face names."))
                end
            elseif k == :underline
                if isname(v)
                    push!(deps, v)
                elseif Meta.isexpr(v, :tuple, 2) && isname(v.args[1])
                    push!(deps, v.args[1])
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
        throw(ArgumentError("Cyclic face dependencies detected in @defpalette! declaration: $(join(setdiff(allnames, faceorder), ", "))."))
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
            @something(get(hoistfaces, f, nothing),
                       Expr(:call, GlobalRef(@__MODULE__, :lookmakeface), nsmodule[], QuoteNode(f)))
        elseif Meta.isexpr(f, :., 2)
            Expr(:., Expr(:., Expr(:., f.args[1], QuoteNode(MAGIC_DEFPALETTE_VARNAME)), QuoteNode(:base)), f.args[2])
        elseif Meta.isexpr(f, :$, 1)
            f.args[1]
        elseif f isa Integer || f isa AbstractString
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
                    Expr(:vect, map(faceorlookup, value.args)...)
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
    decls = (names = Union{Expr, LineNumberNode}[],
             base = Union{Expr, LineNumberNode}[],
             light = Union{Expr, LineNumberNode}[],
             dark = Union{Expr, LineNumberNode}[])
    for ((; name, theme), (; args)) in parsedordered
        # isnothing(line) || push!(decls[theme], line)
        hoistname = get(hoistfaces, name, nothing)
        if theme == :base
            fullname = QuoteNode(Symbol(namespace * String(name)))
            push!(decls.names, Expr(:kw, name, fullname))
            if !isnothing(hoistname)
                push!(decls.base, Expr(:kw, name, hoistname))
            elseif isempty(args)
                push!(decls[theme], Expr(:kw, name, copy(Face())))
            else
                push!(decls[theme], Expr(:kw, name, Expr(:call, Face, Expr(:parameters, (Expr(:kw, k, v) for (k, v) in args)...))))
            end
        elseif name ∉ allnames
            throw(ArgumentError("A $theme variant of face '$name' is declared, without a base variant. Consider adding `$name = Face()` to the palette."))
        elseif isempty(args)
            push!(decls[theme], Expr(:kw, name, copy(Face())))
        else
            push!(decls[theme], Expr(:kw, name, Expr(:call, Face, Expr(:parameters, (Expr(:kw, k, v) for (k, v) in args)...))))
        end
    end
    declsnt = Expr(:parameters)
    for (theme, body) in pairs(decls)
        push!(declsnt.args, Expr(:kw, theme, Expr(:tuple, Expr(:parameters, body...))))
    end
    declsnt = Expr(:tuple, declsnt)
    if !isempty(hoistfaces)
        fhoist = Expr[]
        for ((; name, theme), (; args)) in parsedordered
            theme == :base && haskey(hoistfaces, name) || continue
            fexpr = if isempty(args)
                copy(Face())
            else
                Expr(:call, Face, Expr(:parameters, (Expr(:kw, k, v) for (k, v) in args)...))
            end
            push!(fhoist, Expr(:(=), hoistfaces[name], fexpr))
        end
        declsnt = Expr(:let, Expr(:block), Expr(:block, fhoist..., declsnt))
    end
    if isnothing(varname)
        esc(Expr(:toplevel, :(const $MAGIC_DEFPALETTE_VARNAME = $declsnt),
                 :($reregister_palette!($MAGIC_DEFPALETTE_VARNAME)),
                 MAGIC_DEFPALETTE_VARNAME))
    else
        esc(Expr(:toplevel, :(const $varname = (; $MAGIC_DEFPALETTE_VARNAME = $declsnt)),
                 :($reregister_palette!($varname.$MAGIC_DEFPALETTE_VARNAME)),
                 varname))
    end
end

"""
    @usepalettes! Module...

Pull in palettes defined by other modules. The faces of these palettes can then
be used with [`face""`](@ref).

# Examples

```julia
@usepalettes! SourceA SourceB...
```
"""
macro usepalettes!(names::Union{Expr, Symbol}...)
    isempty(names) && throw(ArgumentError("@usepalettes! needs at least one module, as in `@usepalettes! Module`."))
    checks = [:($isdefined($name, $(QuoteNode(MAGIC_DEFPALETTE_VARNAME))) ||
                  $throw($ArgumentError($("`$name` has no palette to use. A palette is defined with `@defpalette!`, \
                                         and a named palette is used as `$name.<palette name>`."))))
              for name in names]
    refs = [Expr(:., name, QuoteNode(MAGIC_DEFPALETTE_VARNAME)) for name in reverse(names)]
    baserefs = [Expr(:., ref, QuoteNode(:base)) for ref in refs]
    lightrefs = [Expr(:., ref, QuoteNode(:light)) for ref in refs]
    darkrefs = [Expr(:., ref, QuoteNode(:dark)) for ref in refs]
    merged = :((sources = [$(names...)],
                base = $merge($(baserefs...)),
                light = $merge($(lightrefs...)),
                dark = $merge($(darkrefs...))))
    esc(Expr(:toplevel, checks..., :(const $MAGIC_USEPALETTE_VARNAME = $merged; nothing)))
end

"""
    @registerpalette! [names...]

Register the palette defined in the current module in the global registry, along
with the palettes `names` defined by `@defpalette! name ...`.

This should be placed within the `__init__()` function of a module defining a palette.

Use of `@registerpalette!` is essential to make the [`@defpalette!`](@ref)-defined
faces available for theming and customisation.

# Examples

```julia
@defpalette! begin ... end

function __init__()
    @registerpalette!
end
```
"""
macro registerpalette!(names::Symbol...)
    @noinline register_palette_warn(f, l) =
        @warn "@registerpalette! should only be executed during module initialization, within the __init__() function." _file=f _line=l
    @noinline register_palette_missing(f, l) =
        @warn "@registerpalette! was called without a corresponding palette defined (by @defpalette!)." _file=f _line=l
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

"""
    register_palette!(palette::NamedTuple)

Add the faces and variants of `palette` to the global registry.

!!! warning
    Assumes that the caller holds `FACES.lock`, and clears the face cache afterwards.
"""
function register_palette!(palette::NamedTuple)
    for (name, face) in pairs(palette.base)
        fullname = palette.names[name]
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

"""
    reregister_palette!(palette::NamedTuple)

Register `palette` again, if an earlier definition of it is registered. This
happens when a palette is evaluated anew outside of precompilation, for
instance by Revise after an edit.
"""
function reregister_palette!(palette::NamedTuple)
    Base.generating_output() && return
    isredefined = any(pairs(palette.names)) do (facename, fullname)
        registered = get(FACES.pool, fullname, nothing)
        !isnothing(registered) && registered !== palette.base[facename]
    end
    isredefined || return
    @lock FACES.lock begin
        register_palette!(palette)
        emptycache!(FACES.cache.default)
    end
end

"""
    register_displace!(old::Face, new::Face, fullname::Symbol)

Replace `old`, a placeholder or an earlier registration of `fullname`, with `new`
in the global face registry. The caller then derives the current definition of
`new` with `relayer!`.

The modifications of `old` move to `new`. So do its variants when `old` is a
placeholder, while the variants of an earlier registration are dropped for those
of the new palette.

An in-use placeholder is recorded in `FACES.displacements`, so that interpolating
it into styled markup yields `new`.

!!! warning
    Assumes that the caller holds `FACES.lock`.
"""
function register_displace!(old::Face, new::Face, fullname::Symbol)
    delete!(FACES.unregistered, fullname)
    delete!(FACES.names, old)
    placeholder = old.f.height ∈ (UNDEF_CUSTOM_HEIGHT_FLAG, UNDEF_INUSE_HEIGHT_FLAG)
    for tables in (FACES.themes, FACES.modifications), table in tables
        row = get(table, old, nothing)
        isnothing(row) && continue
        delete!(table, old)
        if placeholder || tables === FACES.modifications
            table[new] = row
        end
    end
    if old.f.height == UNDEF_INUSE_HEIGHT_FLAG
        FACES.displacements[old] = new
    end
end
