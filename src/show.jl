# This file is a part of Julia. License is MIT: https://julialang.org/license

strface(text::String, face::Face) =
    AnnotatedString{String, StyledValue}(text, @NamedTuple{region::UnitRange{Int}, value::StyledValue}[(1:ncodeunits(text), face)])

# Through a buffer, so the methods below compile once rather than for each type of `io`
const ShowBuffer = IOContext{AnnotatedIOBuffer{StyledValue}}

# Base's printing of types and symbols is in the sysimage for `IOContext{IOBuffer}`, not `ShowBuffer`
showplain(io::ShowBuffer, x) = print(io, sprint(show, x; context = io))

function Base.show(io::IO, mime::MIME"text/plain", x::Union{Face, SimpleColor})
    buf = IOContext(AnnotatedIOBuffer{StyledValue}(), io)
    show(buf, mime, x)
    print(io, read(seekstart(buf.io), AnnotatedString))
end

function Base.show(io::IO, x::Union{Face, SimpleColor})
    buf = IOContext(AnnotatedIOBuffer{StyledValue}(), io)
    show(buf, x)
    print(io, read(seekstart(buf.io), AnnotatedString))
end

function Base.show(io::ShowBuffer, ::MIME"text/plain", color::SimpleColor)
    skiptype = get(io, :typeinfo, nothing) === SimpleColor
    skiptype || showplain(io, SimpleColor)
    skiptype || print(io, '(')
    if get(io, :color, false)::Bool
        print(io, strface("■", Face(foreground=color)), ' ')
    end
    if color.value isa RGBTuple
        print(io, '#')
        bytes2hex(io, color.value)
    else
        name = get(FACES.names, color.value, nothing)
        if !isnothing(name)
            print(io, name)
        else
            print(io, strface("(unregistered)", face"grey"))
        end
    end
    skiptype || print(io, ')')
    nothing
end

function Base.show(io::ShowBuffer, color::SimpleColor)
    showplain(io, SimpleColor)
    print(io, '(')
    if color.value isa RGBTuple
        print(io, "0x")
        bytes2hex(io, color.value)
    else
        show(io, color.value)
    end
    print(io, ')')
end

function Base.show(io::ShowBuffer, ::MIME"text/plain", face::Face)
    mod = get(io, :module, Main)
    name = facename(mod, face)
    cface = get(FACES.current[], face, face)
    function showval(io::IO, value)
        if value isa SimpleColor
            color = value.value
            if color isa RGBTuple
                print(io, "0x")
                bytes2hex(io, color)
            else
                show(io, color)
            end
        else
            showplain(io, value)
        end
    end
    showcolor(io, color) = show(IOContext(io, :typeinfo => SimpleColor),
                                MIME("text/plain"), color)
    function showinherit(io::IO, faces)
        for (i, iface) in enumerate(faces)
            i > 1 && print(io, ", ")
            print(io, something(facename(mod, iface), "(unregistered)"), '(', strface("*", iface), ')')
        end
    end
    function showunderlineval(io::IO, ul, ulstyle::UInt8, iscompact::Bool)
        showulfn = if iscompact showval else showcolor end
        showstyle = if iscompact showplain else print end
        if ulstyle == NO_UNDERLINE
            print(io, "false")
        elseif isweaknothing(ul) && ulstyle == attrbyte(:underline, :straight)
            print(io, "true")
        elseif iscompact && ul === BASE_FACES.foreground # A tuple with no colour, unlike a bare style
            print(io, "(nothing, ", repr(ATTRIBUTES.underlines[ulstyle + 1]), ')')
        elseif isnothingflavour(ul) || ul === BASE_FACES.foreground
            showstyle(io, ATTRIBUTES.underlines[ulstyle + 1])
        elseif ulstyle == attrbyte(:underline, :straight) || isnothingflavour(ulstyle)
            showulfn(io, SimpleColor(ul))
        else
            iscompact && print(io, '(')
            showulfn(io, SimpleColor(ul))
            print(io, ", ")
            showstyle(io, ATTRIBUTES.underlines[ulstyle + 1])
            iscompact && print(io, ')')
        end
    end
    function printfield(io::IO, field::Symbol, oval, cval; isunset::Function = isnothing, valdisplay::Function = print)
        isunset(oval) && isunset(cval) && return
        print(io, '\n', lpad(String(field), 14, ' '), ": ")
        if isunset(cval)
            print(io, styled"{light:unset}")
        else
            valdisplay(io, cval)
        end
        if isunset(oval)
            print(io, styled" {light,grey:(default unset)}")
        elseif oval != cval
            if valdisplay == print
                print(io, styled" {light:(default: $oval)}")
            else
                print(io, styled" {light:(default: }")
                valdisplay(io, oval)
                print(io, styled"{light:)}")
            end
        end
    end
    if get(io, :compact, false)::Bool
        if !isnothing(name)
            show(io, face)
        else
            showplain(io, Face)
            print(io, '(')
            function showattr(isfirst::Bool, attr::Symbol, value)
                isnothing(value) && return isfirst
                isfirst || print(io, ", ")
                print(io, attr, " = ")
                showval(io, value)
                false
            end
            # Each property is read by a constant name, so its type is known
            isfirst = showattr(true, :font, face.font)
            isfirst = showattr(isfirst, :height, face.height)
            isfirst = showattr(isfirst, :strikethrough, face.strikethrough)
            isfirst = showattr(isfirst, :inverse, face.inverse)
            isfirst = showattr(isfirst, :weight, face.weight)
            isfirst = showattr(isfirst, :slant, face.slant)
            isfirst = showattr(isfirst, :foreground, face.foreground)
            isfirst = showattr(isfirst, :background, face.background)
            ul, ulstyle = face.f.underline, face.f.underline_style
            if !(isnothingflavour(ul) && isnothingflavour(ulstyle))
                if isfirst; isfirst = false else print(io, ", ") end
                print(io, "underline = ")
                showunderlineval(io, ul, ulstyle, true)
            end
            if !isempty(face.inherit)
                if isfirst; isfirst = false else print(io, ", ") end
                print(io, "inherit = ")
                show(IOContext(io, :typeinfo => Vector{Face}), face.inherit)
            end
            print(io, ')')
        end
    else
        showplain(io, Face)
        if !isnothing(name)
            print(io, ' ')
            print(io, strface(String(name), Face(weight=:bold)))
        end
        print(io, " (", strface("sample", face), ")")
        printfield(io, :font, face.font, cface.font)
        printfield(io, :height, face.height, cface.height)
        printfield(io, :weight, face.weight, cface.weight)
        printfield(io, :slant, face.slant, cface.slant)
        printfield(io, :foreground, face.foreground, cface.foreground, valdisplay = showcolor)
        printfield(io, :background, face.background, cface.background, valdisplay = showcolor)
        underline(f::Face) = (f.f.underline, f.f.underline_style)
        printfield(io, :underline, underline(face), underline(cface), isunset = isnothingflavour ∘ last,
                   valdisplay = (io, (ul, ulstyle)) -> showunderlineval(io, ul, ulstyle, false))
        printfield(io, :strikethrough, face.strikethrough, cface.strikethrough)
        printfield(io, :inverse, face.inverse, cface.inverse)
        printfield(io, :inherit, face.inherit, cface.inherit, isunset = isempty, valdisplay = showinherit)
    end
end

function Base.show(io::ShowBuffer, face::Face)
    mod = get(io, :module, Main)
    name = facename(mod, face)
    if isnothing(name)
        show(IOContext(io, :compact => true), MIME("text/plain"), face)
    else
        facemacro = getfacemacro(mod)
        if isnothing(facemacro)
            print(io, @__MODULE__, ".face\"")
        else
            strmac = String(facemacro)
            print(io, @view(strmac[2:thisind(strmac, end - 4)]), '"')
        end
        face = get(FACES.current[], face, face)
        print(io, strface(String(name), face))
        print(io, '"')
    end
end

function getfacemacro(mod::Module)
    # Logic taken from `isvisible` (`base/show.jl`)
    stybinding = convert(Core.Binding, GlobalRef(@__MODULE__, Symbol("@face_str")))
    for name in Base.unsorted_names(mod, imported = true, usings = true)
        nstr = String(name)
        startswith(nstr, '@') && endswith(nstr, "_str") || continue
        binding = convert(Core.Binding, GlobalRef(mod, name))
        while true
            binding === stybinding && return name
            partition = Base.lookup_binding_partition(Base.tls_world_age(), binding)
            Base.is_some_explicit_imported(Base.binding_kind(partition)) || break
            binding = Base.partition_restriction(partition)::Core.Binding
        end
        stypartition = Base.lookup_binding_partition(Base.tls_world_age(), stybinding)
        partition = Base.lookup_binding_partition(Base.tls_world_age(), binding)
        if Base.is_defined_const_binding(Base.binding_kind(stypartition)) && Base.is_defined_const_binding(Base.binding_kind(partition))
            stypartition.restriction === partition.restriction && return name
        end
    end
end
