# This file is a part of Julia. License is MIT: https://julialang.org/license

module Compat

export isnothing, _str_sizehint, takewhile,
    escape_raw_string, peek, parseatom, generating_output

@static if VERSION < v"1.1"
    isnothing(x) = x === nothing
end

@static if VERSION < v"1.2"
    ncodeunits(s::AbstractString) = Base.ncodeunits(s)
    ncodeunits(c::AbstractChar) = Base.ncodeunits(string(c))
else
    import Base.ncodeunits
end

@static if VERSION < v"1.3"
    function _str_sizehint(x)
        if x isa Float64
            return 20
        elseif x isa Float32
            return 12
        elseif x isa String || x isa SubString{String}
            return sizeof(x)
        elseif x isa Char
            return ncodeunits(x)
        else
            return 8
        end
    end
else
    import Base._str_sizehint
end

@static if VERSION < v"1.3"
    function unescape_string(io::IO, s::AbstractString, keep = ())
        a = Iterators.Stateful(s)
        for c in a
            if !isempty(a) && c == '\\'
                c = popfirst!(a)
                if c in keep
                    print(io, '\\', c)
                elseif c == 'x' || c == 'u' || c == 'U'
                    n = k = 0
                    m = c == 'x' ? 2 :
                        c == 'u' ? 4 : 8
                    while (k += 1) <= m && !isempty(a)
                        nc = peek(a)
                        n = '0' <= nc <= '9' ? n<<4 + (nc-'0') :
                            'a' <= nc <= 'f' ? n<<4 + (nc-'a'+10) :
                            'A' <= nc <= 'F' ? n<<4 + (nc-'A'+10) : break
                        popfirst!(a)
                    end
                    if k == 1 || n > 0x10ffff
                        u = m == 4 ? 'u' : 'U'
                        throw(ArgumentError("invalid $(m == 2 ? "hex (\\x)" :
                                            "unicode (\\$u)") escape sequence"))
                    end
                    if m == 2 # \x escape sequence
                        write(io, UInt8(n))
                    else
                        print(io, Char(n))
                    end
                elseif '0' <= c <= '7'
                    k = 1
                    n = c-'0'
                    while (k += 1) <= 3 && !isempty(a)
                        c = peek(a)
                        n = ('0' <= c <= '7') ? n<<3 + c-'0' : break
                        popfirst!(a)
                    end
                    if n > 255
                        throw(ArgumentError("octal escape sequence out of range"))
                    end
                    write(io, UInt8(n))
                else
                    print(io, c == 'a' ? '\a' :
                            c == 'b' ? '\b' :
                            c == 't' ? '\t' :
                            c == 'n' ? '\n' :
                            c == 'v' ? '\v' :
                            c == 'f' ? '\f' :
                            c == 'r' ? '\r' :
                            c == 'e' ? '\e' :
                            (c == '\\' || c == '"') ? c :
                            throw(ArgumentError("invalid escape sequence \\$c")))
                end
            else
                print(io, c)
            end
        end
    end
    unescape_string(s::AbstractString, keep = ()) =
        sprint(unescape_string, s, keep; sizehint=lastindex(s))
end

@static if VERSION < v"1.4"
    function takewhile(f::Function, itr)
        taken = Vector{eltype(itr)}()
        next = iterate(itr)
        while !isnothing(next) && ((item, state) = next) |> first |> f
            push!(taken, item)
            next = iterate(itr, state)
        end
        taken
    end
    function escape_raw_string(io::IO, str::AbstractString, delim::Char='"')
        total = 0
        escapes = 0
        for c in str
            if c == '\\'
                escapes += 1
            else
                if c == delim
                    # if one or more backslashes are followed by
                    # a double quote then escape all backslashes
                    # and the double quote
                    escapes += 1
                    total += escapes
                    while escapes > 0
                        write(io, '\\')
                        escapes -= 1
                    end
                end
                escapes = 0
            end
            write(io, c)
        end
        # also escape any trailing backslashes,
        # so they do not affect the closing quote
        total += escapes
        while escapes > 0
            write(io, '\\')
            escapes -= 1
        end
        total
    end
    function escape_raw_string(str::AbstractString, delim::Char='"')
        total = escape_raw_string(devnull, str, delim) # check whether the string even needs to be copied and how much to allocate for it
        return total == 0 ? str : sprint(escape_raw_string, str, delim; sizehint = sizeof(str) + total)
    end
else
    const takewhile = Iterators.takewhile
    const escape_raw_string = Base.escape_raw_string
end

@static if VERSION < v"1.5"
    function peek(s::Iterators.Stateful, sentinel=nothing)
        ns = s.nextvalstate
        return ns !== nothing ? ns[1] : sentinel
    end
end

@static if VERSION < v"1.6"
    function parseatom(text::AbstractString, pos::Integer)
        Meta.parse(text, pos, greedy = false)
    end
else
    const parseatom = Meta.parseatom
end

@static if VERSION < v"1.11"
    function generating_output(incremental::Union{Bool,Nothing}=nothing)
        ccall(:jl_generating_output, Cint, ()) == 0 && return false
        if incremental !== nothing
            Base.JLOptions().incremental == incremental || return false
        end
        return true
    end
else
    const generating_output = Base.generating_output
end

end
