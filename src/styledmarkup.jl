# This file is a part of Julia. License is MIT: https://julialang.org/license

module StyledMarkup

using Base: AnnotatedString, annotatedstring
using ..StyledStrings: Face, SimpleColor

export @styled_str, styled

struct State
    content::String         # the (unescaped) input string
    bytes::Vector{UInt8}    # bytes of `content`
    s::Iterators.Stateful   # (index, char) interator of `content`
    mod::Union{Module, Nothing} # the context to evaluate in (when part of a macro)
    parts::Vector{Any}      # the final result
    active_styles::Vector{  # unterminated batches of styles, [(source_pos, start, style), ...]
        Vector{Tuple{Int, Int, Union{Symbol, Expr, Pair{Symbol, Any}}}}}
    pending_styles::Vector{ # terminated styles that have yet to be applied, [(range, style), ...]
        Tuple{UnitRange{Int}, Union{Symbol, Expr, Pair{Symbol, Any}}}}
    offset::Ref{Int}        # drift in the `content` index as structures are absorbed
    point::Ref{Int}         # current index in `content`
    escape::Ref{Bool}       # whether the last char was an escape char
    interpolated::Ref{Bool} # whether any string interpolation occurs
    errors::Vector          # any errors raised during parsing
end

function State(content::AbstractString, mod::Union{Module, Nothing}=nothing)
    State(content, Vector{UInt8}(content), # content, bytes
          Iterators.Stateful(pairs(content)), mod, # s, eval
          Any[], # parts
          Vector{Tuple{Int, Int, Union{Symbol, Expr, Pair{Symbol, Any}}}}[], # active_styles
          Tuple{UnitRange{Int}, Union{Symbol, Expr, Pair{Symbol, Any}}}[], # pending_styles
          Ref(0), Ref(1), # offset, point
          Ref(false), Ref(false), # escape, interpolated
          NamedTuple{(:message, :position, :hint), # errors
                     Tuple{AnnotatedString{String}, <:Union{Int, Nothing}, String}}[])
end

const VALID_WEIGHTS = ("thin", "extralight", "light", "semilight", "normal",
                       "medium", "semibold", "bold", "extrabold", "black")
const VALID_SLANTS = ("italic", "oblique", "normal")
const VALID_UNDERLINE_STYLES = ("straight", "double", "curly", "dotted", "dashed")
const VALID_COLORNAMES =
    ("black", "red", "green", "yellow", "blue", "magenta", "cyan", "white",
     "grey", "gray", "bright_black", "bright_red", "bright_green", "bright_yellow",
     "bright_blue", "bright_magenta", "bright_cyan", "bright_white")

isnextchar(state::State, c::Char) =
    !isempty(state.s) && last(peek(state.s)) == c

isnextchar(state::State, cs::NTuple{N, Char}) where {N} =
    !isempty(state.s) && last(peek(state.s)) ∈ cs

ismacro(state::State) = !isnothing(state.mod)

function styerr!(state::State, message, position::Union{Nothing, Int}=nothing, hint::String="around here")
    if !isnothing(position) && position < 0
        position = prevind(
            state.content,
            if !isempty(state.s)
                first(peek(state.s))
            else
                lastindex(state.content)
            end,
            -position)
    end
    push!(state.errors, (; message=AnnotatedString(message), position, hint))
    nothing
end

hygienic_eval(state::State, expr) =
    Core.eval(state.mod, Expr(:var"hygienic-scope", expr, @__MODULE__))

function addpart!(state::State, stop::Int)
    if state.point[] > stop+state.offset[]+ncodeunits(state.content[stop])-1
        return state.point[] = nextind(state.content, stop) + state.offset[]
    end
    str = String(state.bytes[
        state.point[]:stop+state.offset[]+ncodeunits(state.content[stop])-1])
    sty_type, tupl = if ismacro(state)
        Expr, (a, b) -> Expr(:tuple, a, b)
    else
        Tuple{UnitRange{Int}, Pair{Symbol, Any}}, (a, b) -> (a, b)
    end
    push!(state.parts,
            if isempty(state.pending_styles) && isempty(state.active_styles)
                str
            else
                styles = sty_type[]
                relevant_styles = Iterators.filter(
                    (_, start, _)::Tuple -> start <= stop + state.offset[] + 1,
                    Iterators.flatten(state.active_styles))
                for (_, start, annot) in relevant_styles
                    range = (start - state.point[]):(stop - state.point[] + state.offset[] + 1)
                    push!(styles, tupl(range, annot))
                end
                sort!(state.pending_styles, by = first)
                for (range, annot) in state.pending_styles
                    if !isempty(range)
                        push!(styles, tupl(range .- state.point[], annot))
                    end
                end
                empty!(state.pending_styles)
                if isempty(styles)
                    str
                elseif !ismacro(state)
                    AnnotatedString(str, styles)
                else
                    :(AnnotatedString($str, $(Expr(:vect, styles...))))
                end
            end)
    state.point[] = nextind(state.content, stop) + state.offset[]
end

function addpart!(state::State, start::Int, expr, stop::Int)
    if state.point[] < start
        addpart!(state, start)
    end
    if isempty(state.active_styles)
        push!(state.parts, expr)
    else
        str = gensym("str")
        len = gensym("len")
        annots = Expr(:vect, [
            Expr(:tuple, Expr(:call, UnitRange, 1, len), annot)
            for annot in last.(Iterators.flatten(state.active_styles))]...)
        if isempty(annots.args)
            push!(state.parts, :(AnnotatedString(string($expr))))
        else
            push!(state.parts,
                :(let $str = string($expr)
                        $len = ncodeunits($str)
                        AnnotatedString($str, $annots)
                    end))
        end
        map!.((i, _, annot)::Tuple -> (i, stop + state.offset[] + 1, annot),
                state.active_styles, state.active_styles)
    end
end

function escaped!(state::State, i::Int, char::Char)
    if char in ('{', '}', '\\') || (char == '$' && ismacro(state))
        deleteat!(state.bytes, i + state.offset[] - 1)
        state.offset[] -= ncodeunits('\\')
    elseif char ∈ ('\n', '\r') && !isempty(state.s)
        skipped = 0
        if char == '\r' && isnextchar(state, '\n')
            popfirst!(state.s)
            skipped += 1
        end
        while isnextchar(state, (' ', '\t'))
            popfirst!(state.s)
            skipped += 1
        end
        deleteat!(state.bytes, i+state.offset[]-1:i+skipped+state.offset[])
        state.offset[] -= skipped + ncodeunits("\\\n")
    end
    state.escape[] = false
end

function interpolated!(state::State, i::Int, _)
    expr, nexti = readexpr!(state, i + ncodeunits('$'))
    deleteat!(state.bytes, i + state.offset[])
    state.offset[] -= ncodeunits('$')
    addpart!(state, i, esc(expr), nexti)
    state.point[] = nexti + state.offset[]
    state.interpolated[] = true
end

function readexpr!(state::State, pos::Int)
    if isempty(state.s)
        styerr!(state,
                AnnotatedString("Identifier or parenthesised expression expected after \$ in string",
                                [(55:55, :face => :warning)]),
                -1, "right here")
        return "", pos
    end
    expr, nextpos = Meta.parseatom(state.content, pos)
    nchars = length(state.content[pos:prevind(state.content, nextpos)])
    for _ in 1:nchars
        isempty(state.s) && break
        popfirst!(state.s)
    end
    expr, nextpos
end

readexpr!(state) = readexpr!(state, first(popfirst!(state.s)) + 1)

function skipwhitespace!(state::State)
    while isnextchar(state, (' ', '\t', '\n'))
        popfirst!(state.s)
    end
end

function begin_style!(state::State, i::Int, char::Char)
    hasvalue = false
    newstyles = Vector{Tuple{Int, Int, Union{Symbol, Expr, Pair{Symbol, Any}}}}()
    while read_annotation!(state, i, char, newstyles) end
    push!(state.active_styles, newstyles)
    # Adjust bytes/offset based on how much the index
    # has been incremented in the processing of the
    # style declaration(s).
    if !isempty(state.s)
        nexti = first(peek(state.s))
        deleteat!(state.bytes, i+state.offset[]:nexti+state.offset[]-1)
        state.offset[] -= nexti - i
    end
end

function end_style!(state::State, i::Int, char::Char)
    # Close off most recent active style
    for (_, start, annot) in pop!(state.active_styles)
        push!(state.pending_styles, (start:i+state.offset[], annot))
    end
    deleteat!(state.bytes, i + state.offset[])
    state.offset[] -= ncodeunits('}')
end

function read_annotation!(state::State, i::Int, char::Char, newstyles)
    skipwhitespace!(state)
    if isempty(state.s)
        isempty(newstyles) &&
            styerr!(state, "Incomplete annotation declaration", prevind(state.content, i), "starts here")
        return false
    end
    isempty(state.s) && return false
    nextchar = last(peek(state.s))
    if nextchar == ':'
        popfirst!(state.s)
        return false
    elseif nextchar == '('
        read_inlineface!(state, i, char, newstyles)
    else
        read_face_or_keyval!(state, i, char, newstyles)
    end
    isempty(state.s) && return false
    nextchar = last(peek(state.s))
    if nextchar == ','
        popfirst!(state.s)
        true
    elseif nextchar == ':'
        true
    elseif nextchar ∈ (' ', '\t', '\n')
        skipwhitespace!(state)
        true
    else
        styerr!(state, "Malformed styled string construct", -1)
        false
    end
end

function read_inlineface!(state::State, i::Int, char::Char, newstyles)
    # Substructure parsing helper functions
    function readalph!(state, lastchar)
        Iterators.takewhile(
            c -> 'a' <= (lastchar = last(c)) <= 'z', state.s) |>
                collect .|> last |> String, lastchar
    end
    function readsymbol!(state, lastchar)
        Iterators.takewhile(
            c -> (lastchar = last(c)) ∉ (' ', '\t', '\n', ',', ')'), state.s) |>
                collect .|> last |> String, lastchar
    end
    function parsecolor(color::String)
        if color == "nothing"
        elseif startswith(color, '#') && length(color) == 7
            tryparse(SimpleColor, color)
        elseif startswith(color, "0x") && length(color) == 8
            tryparse(SimpleColor, '#' * color[3:end])
        elseif color ∈ VALID_COLORNAMES
            SimpleColor(Symbol(color))
        else
            valid_options = join(VALID_COLORNAMES, ", ", ", or ")
            styerr!(state,
                    AnnotatedString("Invalid named color '$color' (should be $valid_options)",
                                    [(22:21+ncodeunits(color), :face => :warning),
                                        (24+ncodeunits(color):35+ncodeunits(color)+ncodeunits(valid_options),
                                        :face => :light)]),
                    -length(color) - 2)
            SimpleColor(Symbol(color))
        end
    end
    function nextnonwhitespace!(state, lastchar)
        if lastchar ∈ (' ', '\t', '\n')
            skipwhitespace!(state)
            _, lastchar = popfirst!(state.s)
        end
        lastchar
    end
    function read_underline!(state, lastchar)
        if isnextchar(state, '(')
            ustart, _ = popfirst!(state.s)
            skipwhitespace!(state)
            ucolor_str, ucolor = if isnextchar(state, ',')
                lastchar = last(popfirst!(state.s))
                "", nothing
            else
                word, lastchar = readsymbol!(state, lastchar)
                word, parsecolor(word)
            end
            lastchar = nextnonwhitespace!(state, lastchar)
            if !isempty(state.s) && lastchar == ','
                skipwhitespace!(state)
                ustyle, lastchar = readalph!(state, lastchar)
                lastchar = nextnonwhitespace!(state, lastchar)
                if lastchar == ')'
                    lastchar = last(popfirst!(state.s))
                else
                    styerr!(state, "Malformed underline value, should be (<color>, <style>)",
                            -first(something(peek(state.s), (ustart+2, '.'))) + ustart - 1)
                end
            else
                styerr!(state, "Malformed underline value, should be (<color>, <style>)",
                        -first(something(peek(state.s), (ustart+2, '.'))) + ustart - 1)
                ustyle = "straight"
            end
            if ustyle ∉ VALID_UNDERLINE_STYLES
                valid_options = join(VALID_UNDERLINE_STYLES, ", ", ", or ")
                styerr!(state,
                        AnnotatedString("Invalid underline style '$ustyle' (should be $valid_options)",
                                        [(26:25+ncodeunits(ustyle), :face => :warning)
                                            (28+ncodeunits(ustyle):39+ncodeunits(ustyle)+ncodeunits(valid_options),
                                            :face => :light)]),
                        -length(ustyle) - 3)
            end
            if ismacro(state)
                Expr(:tuple, ucolor, QuoteNode(Symbol(ustyle)))
            else
                (ucolor, Symbol(ustyle))
            end
        else
            word, lastchar = readsymbol!(state, lastchar)
            if word ∈ ("", "nothing")
            elseif word == "true"
                true
            elseif word == "false"
                false
            else
                parsecolor(word)
            end
        end, lastchar
    end
    function read_inherit!(state, lastchar)
        inherit = Symbol[]
        if isnextchar(state, ':')
            popfirst!(state.s)
            facename, lastchar = readsymbol!(state, lastchar)
            push!(inherit, Symbol(facename))
        elseif isnextchar(state, '[')
            popfirst!(state.s)
            readvec = true
            while readvec
                skipwhitespace!(state)
                nextchar = last(peek(state.s))
                if nextchar == ':'
                    popfirst!(state.s)
                elseif nextchar == ']'
                    popfirst!(state.s)
                    lastchar = last(popfirst!(state.s))
                    break
                end
                facename = Iterators.takewhile(
                    c -> (lastchar = last(c)) ∉ (',', ']', ')'), state.s) |>
                        collect .|> last |> String
                push!(inherit, Symbol(rstrip(facename)))
                if lastchar != ','
                    break
                end
            end
        else
            facename = Iterators.takewhile(
                c -> (lastchar = last(c)) ∉ (',', ']', ')'), state.s) |>
                    collect .|> last |> String
            push!(inherit, Symbol(rstrip(facename)))
        end
        inherit, lastchar
    end
    # Tentatively initiate the parsing
    popfirst!(state.s)
    lastchar = '('
    skipwhitespace!(state)
    if isnextchar(state, ')')
        # We've hit the empty-construct special case
        popfirst!(state.s)
        return
    end
    # Get on with the parsing now
    kwargs = if ismacro(state) Expr[] else Pair{Symbol, Any}[] end
    needseval = false
    while !isempty(state.s) && lastchar != ')'
        skipwhitespace!(state)
        str_key, lastchar = readalph!(state, lastchar)
        # If we have actually reached the end but there is a trailing
        # comma/whitespace, we find out here and abort.
        isempty(str_key) && lastchar == ')' && break
        key = Symbol(str_key)
        # Check for '=', and skip over surrounding whitespace
        if lastchar != '='
            skipwhitespace!(state)
            if isempty(state.s) || last(peek(state.s)) != '='
                styerr!(state, "Keyword argument has no value", -3)
                break
            end
            popfirst!(state.s)
        end
        skipwhitespace!(state)
        isempty(state.s) && break
        # Aliases
        key == :fg && (key = :foreground)
        key == :bg && (key = :background)
        # Parse value
        val = if isnextchar(state, '$')
            expr, _ = readexpr!(state)
            lastchar = last(popfirst!(state.s))
            state.interpolated[] = true
            needseval = true
            esc(expr)
        elseif key == :face
            if isnextchar(state, '"')
                readexpr!(state, first(peek(state.s))) |> first |> esc
            else
                Iterators.takewhile(
                    c -> (lastchar = last(c)) ∉ (',', ')'), state.s) |>
                        collect .|> last |> String
            end
        elseif key == :height
            if isnextchar(state, ('.', '0':'9'...))
                num = readexpr!(state, first(peek(state.s))) |> first
                lastchar = last(popfirst!(state.s))
                ifelse(num isa Number, num, nothing)
            else
                invalid, lastchar = readsymbol!(state, lastchar)
                styerr!(state, AnnotatedString("Invalid height '$invalid', should be a natural number or positive float",
                                                [(17:16+ncodeunits(string(invalid)), :face => :warning)]),
                        -3)
            end
        elseif key ∈ (:weight, :slant)
            v, lastchar = readalph!(state, lastchar)
            if key == :weight && v ∉ VALID_WEIGHTS
                valid_options = join(VALID_WEIGHTS, ", ", ", or ")
                styerr!(state, AnnotatedString("Invalid weight '$v' (should be $valid_options)",
                                                [(17:16+ncodeunits(v), :face => :warning),
                                                (19+ncodeunits(v):30+ncodeunits(v)+ncodeunits(valid_options),
                                                    :face => :light)]),
                        -3)
            elseif key == :slant && v ∉ VALID_SLANTS
                valid_options = join(VALID_SLANTS, ", ", ", or ")
                styerr!(state, AnnotatedString("Invalid slant '$v' (should be $valid_options)",
                                                [(16:15+ncodeunits(v), :face => :warning),
                                                (18+ncodeunits(v):29+ncodeunits(v)+ncodeunits(valid_options),
                                                    :face => :light)]),
                        -3)
            end
            Symbol(v) |> if ismacro(state) QuoteNode else identity end
        elseif key ∈ (:foreground, :background)
            color, lastchar = readsymbol!(state, lastchar)
            parsecolor(color)
        elseif key ∈ (:strikethrough, :inverse)
            flag, lastchar = readalph!(state, lastchar)
            if flag == "true"
                true
            elseif flag == "false"
                false
            end
        elseif key == :underline
            ul, lastchar = read_underline!(state, lastchar)
            ul
        elseif key == :inherit
            inherit, lastchar = read_inherit!(state, lastchar)
            inherit
        else
            styerr!(state, AnnotatedString("Uses unrecognised face key '$key'",
                                            [(29:28+ncodeunits(String(key)), :face => :warning)]),
                    -length(str_key) - 2)
        end
        if ismacro(state) && !any(k -> first(k.args) == key, kwargs)
            push!(kwargs, Expr(:kw, key, val))
        elseif !ismacro(state) && !any(kw -> first(kw) == key, kwargs)
            push!(kwargs, key => val)
        else
            styerr!(state, AnnotatedString("Contains repeated face key '$key'",
                                            [(29:28+ncodeunits(String(key)), :face => :warning)]),
                    -length(str_key) - 2)
        end
        isempty(state.s) && styerr!(state, "Incomplete inline face declaration", -1)
        skipwhitespace!(state)
        !isnextchar(state, ',') || break
    end
    face = Expr(:call, Face, kwargs...)
    push!(newstyles,
          (i, i + state.offset[] + 1,
           if !ismacro(state)
               Pair{Symbol, Any}(:face, Face(; NamedTuple(kwargs)...))
           elseif needseval
               :(Pair{Symbol, Any}(:face, $face))
           else
               Pair{Symbol, Any}(:face, hygienic_eval(state, face))
           end))
end

function read_face_or_keyval!(state::State, i::Int, char::Char, newstyles)
    function read_curlywrapped!(state)
        popfirst!(state.s) # first '{'
        chars = Char[]
        escaped = false
        while !isempty(state.s)
            _, c = popfirst!(state.s)
            if escaped && (c ∈ ('\\', '{', '}') || (c == '$' && ismacro(state)))
                push!(chars, c)
                escaped = false
            elseif escaped
                push!(chars, '\\', c)
                escaped = false
            elseif c == '\\'
                escaped = true
            elseif c == '}'
                break
            else
                push!(chars, c)
            end
        end
        String(chars)
    end
    # this isn't the 'last' char yet, but it will be
    key = if ismacro(state) && last(peek(state.s)) == '$'
        expr, _ = readexpr!(state)
        state.interpolated[] = true
        needseval = true
        esc(expr)
    else
        chars = Char[]
        while (next = peek(state.s)) |> !isnothing && last(next) ∉ (',', '=', ':')
            popfirst!(state.s)
            push!(chars, last(next))
        end
        String(chars)
    end
    skipwhitespace!(state)
    if isempty(state.s)
    elseif last(peek(state.s)) == '='
        popfirst!(state.s)
        if isnextchar(state, (' ', '\t', '\n'))
            skipwhitespace!(state)
        end
        nextchar = if !isempty(state.s) last(peek(state.s)) else '\0' end
        value = if isempty(state.s) ""
        elseif nextchar == '{'
            read_curlywrapped!(state)
        elseif ismacro(state) && nextchar == '$'
            expr, _ = readexpr!(state)
            state.interpolated[] = true
            needseval = true
            esc(expr)
        else
            chars = Char[]
            while (next = peek(state.s)) |> !isnothing && last(next) ∉ (',', ':')
                popfirst!(state.s)
                push!(chars, last(next))
            end
            String(chars)
        end
        push!(newstyles,
                (i, i + state.offset[] + ncodeunits('{'),
                if key isa String && !(value isa Symbol || value isa Expr)
                    Pair{Symbol, Any}(Symbol(key), value)
                elseif key isa Expr || key isa Symbol
                    :(Pair{Symbol, Any}($key, $value))
                else
                    :(Pair{Symbol, Any}(
                        $(QuoteNode(Symbol(key))), $value))
                end))
    elseif key !== ""
        push!(newstyles,
                (i, i + state.offset[] + ncodeunits('{'),
                if key isa Symbol || key isa Expr
                    :(Pair{Symbol, Any}(:face, $key))
                else # Face symbol
                    Pair{Symbol, Any}(:face, Symbol(key))
                end))
    end
    if isempty(state.s) || last(peek(state.s)) ∉ (' ', '\t', '\n', ',', ':')
        styerr!(state, "Incomplete annotation declaration", prevind(state.content, i), "starts here")
    end
end

function run_state_machine!(state::State)
    # Run the state machine
    for (i, char) in state.s
        if char == '\\'
            state.escape[] = true
        elseif state.escape[]
            escaped!(state, i, char)
        elseif ismacro(state) && char == '$'
            interpolated!(state, i, char)
        elseif char == '{'
            begin_style!(state, i, char)
        elseif char == '}'
            if !isempty(state.active_styles)
                end_style!(state, i, char)
            else
                styerr!(state, "Contains extraneous style terminations", -2, "right here")
            end
        end
    end
    # Ensure that any trailing unstyled content is added
    if state.point[] <= lastindex(state.content) + state.offset[]
        addpart!(state, lastindex(state.content))
    end
    for incomplete in Iterators.flatten(state.active_styles)
        styerr!(state, AnnotatedString("Unterminated annotation (missing closing '}')",
                                        [(43:43, :face => :warning)]),
                prevind(state.content, first(incomplete)), "starts here")
    end
end

"""
    @styled_str -> AnnotatedString

Construct a styled string. Within the string, `{<specs>:<content>}` structures
apply the formatting to `<content>`, according to the list of comma-separated
specifications `<specs>`. Each spec can either take the form of a face name,
an inline face specification, or a `key=value` pair. The value must be wrapped
by `{...}` should it contain any of the characters `,=:{}`.

String interpolation with `\$` functions in the same way as regular strings,
except quotes need to be escaped. Faces, keys, and values can also be
interpolated with `\$`.

# Example

```julia
styled"The {bold:{italic:quick} {(foreground=#cd853f):brown} fox} jumped over \
the {link={https://en.wikipedia.org/wiki/Laziness}:lazy} dog"
```

# Extended help

This macro can be described by the following EBNF grammar:

```ebnf
styledstring = { styled | interpolated | escaped | plain } ;

specialchar = '{' | '}' | '\$' | '\\\"' ;
anychar = [\\u0-\\u1fffff] ;
plain = { anychar - specialchar } ;
escaped = '\\\\', specialchar ;

interpolated = '\$', ? expr ? | '\$(', ? expr ?, ')' ;

styled = '{', ws, annotations, ':', content, '}' ;
content = { interpolated | plain | escaped | styled } ;
annotations = annotation | annotations, ws, ',', ws, annotation ;
annotation = face | inlineface | keyvalue ;
ws = { ' ' | '\\t' | '\\n' } ; (* whitespace *)

face = facename | interpolated ;
facename = [A-Za-z0-9_]+ ;

inlineface = '(', ws, [ faceprop ], { ws, ',', faceprop }, ws, ')' ;
faceprop = [a-z]+, ws, '=', ws, ( [^,)]+ | interpolated) ;

keyvalue = key, ws, '=', ws, value ;
key = ( [^\${}=,:], [^=,:]* ) | interpolated ;
value = simplevalue | curlybraced | interpolated ;
curlybraced = '{' { escaped | plain } '}' ;
simplevalue = [^\${},:], [^,:]* ;
```

An extra stipulation not encoded in the above grammar is that `plain` should be
a valid input to [`unescape_string`](@ref), with `specialchar` kept.

The above grammar for `inlineface` is simplified, as the actual implementation
is a bit more sophisticated. The full behaviour is given below.

```ebnf
faceprop = ( 'face', ws, '=', ws, ( ? string ? | interpolated ) ) |
           ( 'height', ws, '=', ws, ( ? number ? | interpolated ) ) |
           ( 'weight', ws, '=', ws, ( symbol | interpolated ) ) |
           ( 'slant', ws, '=', ws, ( symbol | interpolated ) ) |
           ( ( 'foreground' | 'fg' | 'background' | 'bg' ),
               ws, '=', ws, ( simplecolor | interpolated ) ) |
           ( 'underline', ws, '=', ws, ( underline | interpolated ) ) |
           ( 'strikethrough', ws, '=', ws, ( bool | interpolated ) ) |
           ( 'inverse', ws, '=', ws, ( bool | interpolated ) ) |
           ( 'inherit', ws, '=', ws, ( inherit | interpolated ) ) ;

nothing = 'nothing' ;
bool = 'true' | 'false' ;
symbol = [^ ,)]+ ;
hexcolor = ('#' | '0x'), [0-9a-f]{6} ;
simplecolor = hexcolor | symbol | nothing ;

underline = nothing | bool | simplecolor | underlinestyled;
underlinestyled = '(', ws, ('' | nothing | simplecolor), ws,
                  ',', ws, symbol, ws ')' ;

inherit = ( '[', inheritval, { ',', inheritval }, ']' ) | inheritval;
inheritval = ws, ':'?, symbol ;
```
"""
macro styled_str(raw_content::String)
    # First undo the transforms of the parser, (assuming the input was entered
    # with single `styled""` markers so this transform is unambiguously
    # reversible and not as `@styled_str "."` or `styled"""."""`), since the
    # `unescape_string` transforms will be a superset of those transforms
    content = unescape_string(Base.escape_raw_string(raw_content),
                              ('{', '}', '$', '\n', '\r'))
    state = State(content, __module__)
    run_state_machine!(state)
    if !isempty(state.errors)
        throw(MalformedStylingMacro(state.content, state.errors))
    elseif state.interpolated[]
        :(annotatedstring($(state.parts...)))
    else
        annotatedstring(map(Base.Fix1(hygienic_eval, state), state.parts)...) |> Base.annotatedstring_optimize!
    end
end

"""
    styled(content::AbstractString) -> AnnotatedString

Construct a styled string. Within the string, `{<specs>:<content>}` structures
apply the formatting to `<content>`, according to the list of comma-separated
specifications `<specs>`. Each spec can either take the form of a face name,
an inline face specification, or a `key=value` pair. The value must be wrapped
by `{...}` should it contain any of the characters `,=:{}`.

This is a functional equivalent of the [`@styled_str`](@ref) macro, just without
interpolation capabilities.
"""
function styled(content::AbstractString)
    state = State(content)
    run_state_machine!(state)
    if !isempty(state.errors)
        throw(MalformedStylingMacro(state.content, state.errors))
    else
        annotatedstring(state.parts...) |> Base.annotatedstring_optimize!
    end
end

struct MalformedStylingMacro <: Exception
    raw::String
    problems::Vector{NamedTuple{(:message, :position, :hint), Tuple{AnnotatedString{String}, <:Union{Int, Nothing}, String}}}
end

function Base.showerror(io::IO, err::MalformedStylingMacro)
    # We would do "{error:┌ ERROR\:} MalformedStylingMacro" but this is already going
    # to be prefixed with "ERROR: LoadError", so it's better not to.
    println(io, "MalformedStylingMacro")
    for (; message, position, hint) in err.problems
        posinfo = if isnothing(position)
            println(io, styled"{error:│} $message.")
        else
            infowidth = displaysize(stderr)[2] ÷ 3
            j = clamp(12 * round(Int, position / 12),
                        firstindex(err.raw):lastindex(err.raw))
            start = if j <= infowidth firstindex(err.raw) else
                max(prevind(err.raw, j, infowidth), firstindex(err.raw))
            end
            stop = if ncodeunits(err.raw) - j <= infowidth lastindex(err.raw) else
                min(nextind(err.raw, j, infowidth), lastindex(err.raw))
            end
            window = Base.escape_string(err.raw[start:stop])
            begin_ellipsis = ifelse(start == firstindex(err.raw), "", "…")
            end_ellipsis = ifelse(stop == lastindex(err.raw), "", "…")
            npre = ncodeunits(Base.escape_string(err.raw[start:position]))
            println(io, styled"{error:│} $message:\n\
                           {error:│}  {bright_green:\"{shadow:$begin_ellipsis}$window{shadow:$end_ellipsis}\"}\n\
                           {error:│}  $(' '^(1+textwidth(begin_ellipsis)+npre)){info:╰─╴$hint}")
        end
    end
    pluralissues = length(err.problems) > 1
    println(io, styled"{error:┕} {light,italic:$(length(err.problems)) $(ifelse(pluralissues, \"issues\", \"issue\"))}")
end

end
