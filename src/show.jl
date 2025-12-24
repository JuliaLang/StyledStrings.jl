# This file is a part of Julia. License is MIT: https://julialang.org/license

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
              lpad(string(r, base=16), 2, '0'),
              lpad(string(g, base=16), 2, '0'),
              lpad(string(b, base=16), 2, '0'))
    else
        name = get(FACES.names, color.value, nothing)
        if !isnothing(name)
            print(io, name)
        else
            print(io, AnnotatedString("(unregistered)", [(region=1:14, label=:face, value=face"grey")]))
        end
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
              lpad(string(r, base=16), 2, '0'),
              lpad(string(g, base=16), 2, '0'),
              lpad(string(b, base=16), 2, '0'))
    else
        show(io, color.value)
    end
    print(io, ')')
end

function Base.show(io::IO, ::MIME"text/plain", face::Face)
    mod = get(io, :module, Main)
    name = facename(mod, face)
    face = get(FACES.current[], face, face) # Get customised face
    function showval(io::IO, value)
        if value isa SimpleColor
            color = value.value
            if color isa RGBTuple
                print(io, "0x",
                      lpad(string(color.r, base=16), 2, '0'),
                      lpad(string(color.g, base=16), 2, '0'),
                      lpad(string(color.b, base=16), 2, '0'))
            else
                show(io, color)
            end
        else
            show(io, value)
        end
    end
    showcolor(io, color) = show(IOContext(io, :typeinfo => SimpleColor),
                                MIME("text/plain"), color)
    function showunderlineval(io::IO, ul, ulstyle, iscompact::Bool)
        showulfn = if iscompact showval else showcolor end
        if isstrongnothing(ulstyle)
            print(io, "false")
        elseif isnothingflavour(ul) && ulstyle == :straight
            print(io, "true")
        elseif isnothingflavour(ul)
            if iscompact
                show(io, ulstyle)
            else
                print(io, ulstyle)
            end
        elseif ulstyle == :straight
            showulfn(io, ul)
        else
            iscompact && print(io, '(')
            showulfn(io, ul)
            print(io, ", ")
            if iscompact
                show(io, ulstyle)
                print(io, ')')
            else
                print(io, ulstyle)
            end
        end
    end
    if get(io, :compact, false)::Bool
        if !isnothing(name)
            show(io, face)
        # elseif get(io, :limit, false)::Bool
        #     show(io, Face)
        #     print(io, AnnotatedString("(sample)", [(region=2:7, label=:face, value=face)]))
        else
            show(io, Face)
            print(io, '(')
            isfirst = true
            for attr in setdiff(propertynames(face), (:inherit,))
                if attr == :underline
                    ul, ulstyle = face.f.underline, face.f.underline_style
                    if isnothingflavour(ul) && isweaknothing(ulstyle)
                        continue
                    else
                        if isfirst; isfirst = false else print(io, ", ") end
                        print(io, "underline = ")
                    end
                    showunderlineval(io, ul, ulstyle, true)
                else
                    value = getproperty(face, attr)
                    isnothing(value) && continue
                    if isfirst; isfirst = false else print(io, ", ") end
                    print(io, attr, " = ")
                    showval(io, value)
                end
            end
            if !isempty(face.inherit)
                if isfirst; isfirst = false else print(io, ", ") end
                print(io, "inherit = ")
                show(IOContext(io, :typeinfo => Memory{Face}), face.inherit)
            end
            print(io, ')')
        end
    else
        show(io, Face)
        if !isnothing(name)
            print(io, ' ')
            print(io, AnnotatedString(String(name), [(region=1:ncodeunits(String(name)), label=:face, value=Face(weight=:bold))]))
        end
        print(io, AnnotatedString(" (sample)", [(region=3:8, label=:face, value=face)]))
        setfields = Pair{Symbol, Any}[]
        isempty(setfields) || print(io, ":")
        fieldnamepad = 14
        for field in (:font, :height, :weight, :slant)
            if !isnothing(getproperty(face, field))
                print(io, '\n', lpad(String(field), fieldnamepad, ' '), ": ",
                      getproperty(face, field))
            end
        end
        for field in (:foreground, :background)
            if !isnothing(getproperty(face, field))
                print(io, '\n', lpad(String(field), fieldnamepad, ' '), ": ")
                showcolor(io, getproperty(face, field))
            end
        end
        if !isweaknothing(face.f.underline_style)
            print(io, '\n', lpad("underline", fieldnamepad, ' '), ": ")
            showunderlineval(io, face.f.underline, face.f.underline_style, false)
        end
        for field in (:strikethrough, :inverse)
            if !isnothing(getproperty(face, field))
                print(io, '\n', lpad(String(field), fieldnamepad, ' '), ": ",
                      getproperty(face, field))
            end
        end
        if !isempty(face.inherit)
            print(io, '\n', lpad("inherit", fieldnamepad, ' '), ": ")
            isfirst = true
            for iface in face.inherit
                if isfirst; isfirst = false else print(io, ", ") end
                iname = something(facename(mod, iface), "(unregistered)")
                print(io, iname, '(', AnnotatedString("*", [(region=1:1, label=:face, value=iface)]), ')')
            end
        end
    end
end

function Base.show(io::IO, face::Face)
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
        face = get(FACES.current[], face, face) # Get customised face
        print(io, AnnotatedString(String(name), [(region=1:ncodeunits(String(name)), label=:face, value=face)]))
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
