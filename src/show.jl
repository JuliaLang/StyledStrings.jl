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
