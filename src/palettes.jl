# This file is a part of Julia. License is MIT: https://julialang.org/license

const MAGIC_DEFPALETTE_VARNAME = Symbol("##styledstrings-defpalette-variable#")
const MAGIC_USEPALETTE_VARNAME = Symbol("##styledstrings-usepalette-variable#")

const var"##styledstrings-defpalette-variable#" = (; base = STANDARD_FACES)

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
        existing = get(FACES.unregistered, name, nothing)
        !isnothing(existing) && (!use || getfield(existing, :f).height == UNDEF_INUSE_HEIGHT_FLAG) &&
            return existing
        uface = Face(FaceDef(
            weaknothing(String), ifelse(use, UNDEF_INUSE_HEIGHT_FLAG, UNDEF_CUSTOM_HEIGHT_FLAG),
            weaknothing(Bool), weaknothing(Bool), weaknothing(Symbol), weaknothing(Symbol),
            weaknothing(SimpleColor), weaknothing(SimpleColor), weaknothing(SimpleColor),
            weaknothing(Symbol), Memory{Face}()))
        if !isnothing(existing)
            # 'Upgrade' an customisation-only face to an in-use face
            delete!(FACES.names, existing)
            if haskey(FACES.modifications.base, existing)
                val = FACES.modifications.base[existing]
                delete!(FACES.modifications.base, existing)
                FACES.modifications.base[uface] = val
            end
            if haskey(FACES.modifications.light, existing)
                val = FACES.modifications.light[existing]
                delete!(FACES.modifications.light, existing)
                FACES.modifications.light[uface] = val
            end
            if haskey(FACES.modifications.dark, existing)
                val = FACES.modifications.dark[existing]
                delete!(FACES.modifications.dark, existing)
                FACES.modifications.dark[uface] = val
            end
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
        for (name, f) in pairs(getglobal(mod, MAGIC_USEPALETTE_VARNAME))
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
"""
macro face_str(name::String)
    isempty(name) && return Face()
    face = if '.' in name
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
any decoration. For instance, `blue` should be used over `face"blue"`.

Cyclic dependencies between faces (e.g. two faces inheriting from each other)
are not possible, but the order of declaration is automatically determined.

# Examples

```julia
@defpalette! begin
    important = Face(weight = bold, inherit = warning)
    topic = Face(foreground = blue)
    heading = Face(foreground = important)
end
```
"""
macro defpalette!(pargs::Any...)
    nsmodule = __module__
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
                    if nsval isa Module
                        nsmodule = nsval
                    else
                        throw(ArgumentError("Invalid @defpalette! argument `$decl`, namespace must be a String, Symbol, or Module."))
                    end
                end
            else
                throw(ArgumentError("Invalid @defpalette! argument `$decl`."))
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
        theme == :base || push!(deps, name)
        for arg in facecall.args[2:end]
            Meta.isexpr(arg, :kw, 2) || throw(ArgumentError("Invalid Face argument `$arg`."))
            k, v = arg.args
            if k == :fg
                k = :foreground
            elseif k == :bg
                k = :background
            end
            if Meta.isexpr(v, :$, 1)
                push!(faceargs, k => esc(v.args[1]))
                continue
            end
            if k ∈ (:foreground, :background)
                v isa Symbol && (push!(deps, v); true) ||
                    Meta.isexpr(v, :., 2) || throw(ArgumentError("Invalid Face argument `$arg`, $k color value (`$v`) must be a variable name or a `\$(...)` expression."))
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
                if v isa Symbol
                    push!(deps, v)
                elseif Meta.isexpr(v, :tuple, 2) && v.args[1] isa Symbol
                    push!(deps, v.args[1])
                end
            end
            push!(faceargs, k => v)
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
        depfaces = parsed[(; name = rdep, theme = :base)].deps
        ind = findfirst(==(name), depfaces)::Int
        if isempty(deleteat!(depfaces, ind))
            hoistfaces[name] = gensym("$(name)_face")
            push!(faceorder, rdep)
        end
    end
    length(faceorder) == length(allnames) ||
        throw(ArgumentError("Cyclic face dependencies detected in @defpalette! declaration: $(join(setdiff(allnames, faceorder), ", "))."))
    # Add in variant faces as deps
    for ((; name, theme), (; deps)) in parsed
        theme == :base && continue
        get!(() -> gensym("face"), hoistfaces, name)
    end
    # Rewrite arguments
    faceorlookup(f::Symbol) = @something(get(hoistfaces, f, nothing), Expr(:call, GlobalRef(@__MODULE__, :lookmakeface), nsmodule, QuoteNode(f)))
    function faceorlookup(fe::Expr)
        Meta.isexpr(fe, :., 2) || throw(ArgumentError("Invalid face reference expression `$fe`."))
        Expr(:., Expr(:., Expr(:., fe.args[1], QuoteNode(MAGIC_DEFPALETTE_VARNAME)), QuoteNode(:base)), fe.args[2])
    end
    for (; args) in values(parsed)
        for (i, (arg, value)) in enumerate(args)
            args[i] = if arg == :foreground
                :foreground => faceorlookup(value)
            elseif arg == :background
                :background => faceorlookup(value)
            elseif arg == :inherit
                :inherit => if Meta.isexpr(value, :vect)
                    Expr(:vect, map(v -> faceorlookup(v), value.args)...)
                else
                    faceorlookup(value)
                end
            elseif arg == :underline
                if value isa Symbol || value isa Expr
                    :underline => faceorlookup(value)
                elseif Meta.isexpr(value, :tuple, 2)
                    :underline => Expr(:tuple, faceorlookup(value.args[1]), value.args[2])
                else
                    args[i]
                end
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
    sort!(parsedordered; by = ((k, v),) -> v.i)
    # Construct final expression
    decls = (names = Union{Expr, LineNumberNode}[],
             base = Union{Expr, LineNumberNode}[],
             light = Union{Expr, LineNumberNode}[],
             dark = Union{Expr, LineNumberNode}[])
    for ((; name, theme), (; args, line)) in parsedordered
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
        elseif isnothing(hoistname)
            throw(ArgumentError("A $theme variant of face '$name' is declared, without a base variant. Consider adding `$name = Face()` to the palette."))
        elseif isempty(args)
            push!(decls[theme], Expr(:kw, hoistname, copy(Face())))
        else
            push!(decls[theme], Expr(:kw, hoistname, Expr(:call, Face, Expr(:parameters, (Expr(:kw, k, v) for (k, v) in args)...))))
        end
    end
    declsnt = Expr(:parameters)
    for (theme, body) in pairs(decls)
        push!(declsnt.args, Expr(:kw, theme, Expr(:tuple, Expr(:parameters, body...))))
    end
    declsnt = Expr(:tuple, declsnt)
    if !isempty(hoistfaces)
        fhoist = Expr[]
        for ((; name, theme), (; args, line)) in parsedordered
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
    esc(Expr(:toplevel, if !isnothing(varname)
        :(const $varname = (; $MAGIC_DEFPALETTE_VARNAME = $declsnt))
    else
        :(const $MAGIC_DEFPALETTE_VARNAME = $declsnt)
    end))
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
    refs = [Expr(:., name, QuoteNode(MAGIC_DEFPALETTE_VARNAME)) for name in reverse(names)]
    baserefs = [Expr(:., ref, QuoteNode(:base)) for ref in refs]
    lightrefs = [Expr(:., ref, QuoteNode(:light)) for ref in refs]
    darkrefs = [Expr(:., ref, QuoteNode(:dark)) for ref in refs]
    merged = :((sources = [$(names...)],
                base = $merge($(baserefs...)),
                light = $merge($(lightrefs...)),
                dark = $merge($(darkrefs...))))
    esc(Expr(:toplevel, :(const $MAGIC_USEPALETTE_VARNAME = $merged; nothing)))
end

"""
    @registerpalette!

Register the palette defined in the current module in the global registry.

This should be placed within the `__init__()` function of a module defining a palette.

Use of `@registerpalette!` is essential to make the [`@defpalette!`](@ref)-defined
faces available for theming, remapping, and customisation.

# Examples

```julia
@defpalette! begin ... end

function __init__()
    @registerpalette!
end
```
"""
macro registerpalette!()
    @noinline register_palette_warn(f, l) =
        @warn "@registerpalette! should only be executed during module initialization, within the __init__() function." _file=f _line=l
    @noinline register_palette_missing(f, l) =
        @warn "@registerpalette! was called without a corresponding palette defined (by @defpalette!)." _file=f _line=l
    gfaces = GlobalRef(@__MODULE__, :FACES)
    file, line = String(__source__.file), __source__.line
    quote
        if !iszero(ccall(:jl_generating_output, Cint, ())) &&
            @noinline (() -> !any(sf -> sf.func === :__init__, stacktrace(backtrace())))()
            $register_palette_warn($file, $line)
        elseif isdefined($__module__, $(QuoteNode(MAGIC_DEFPALETTE_VARNAME)))
            palette = getglobal($__module__, $(QuoteNode(MAGIC_DEFPALETTE_VARNAME)))
            @lock $gfaces.lock begin
                for (name, face) in pairs(palette.base)
                    fullname = palette.names[name]
                    unreg = get($gfaces.unregistered, fullname, nothing)
                    isnothing(unreg) || $register_displace!(unreg, face, fullname)
                    $gfaces.pool[fullname] = face
                    $gfaces.names[face] = fullname
                end
                for (orig, face) in pairs(palette.light)
                    $gfaces.themes.light[orig] = face
                end
                for (orig, face) in pairs(palette.dark)
                    $gfaces.themes.dark[orig] = face
                end
            end
        else
            $register_palette_missing($file, $line)
        end
        nothing
    end
end

"""
    register_displace!(unreg::Face, reg::Face, fullname::Symbol)

Displace an unregistered face with a registered one in the global face registry.

!!! warning
    Assumes that the caller holds `FACES.lock`.
"""
function register_displace!(unreg::Face, reg::Face, fullname::Symbol)
    delete!(FACES.unregistered, fullname)
    delete!(FACES.names, unreg)
    for (cat, mods) in pairs(FACES.modifications)
        mface = get(mods, unreg, nothing)
        isnothing(mface) && continue
        delete!(mods, unreg)
        mods[reg] = mface
        if cat ∈ (:base, FACES.current_theme)
            FACES.current.default[reg] = merge(get(FACES.current.default, reg, reg), mface)
        end
    end
    if unreg.f.height == UNDEF_INUSE_HEIGHT_FLAG
        FACES.remapping.default[unreg] = reg
        FACES.remapping[][unreg] = reg
    end
end
