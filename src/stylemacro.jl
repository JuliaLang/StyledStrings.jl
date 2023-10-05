# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
    @styled_str -> TaggedString

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
style"The {bold:{italic:quick} {(foreground=#cd853f):brown} fox} jumped over \
the {link={https://en.wikipedia.org/wiki/Laziness}:lazy} dog"
```

# Extended help

This macro can be described by the following EBNF grammar:

```ebnf
styledstring = { styled | interpolated | escaped | plain } ;

specialchar = '{' | '}' | '\$' | '\\\"' ;
anychar = [\\u0-\\u1fffff] ;
almostplain = { anychar - specialchar } ;
plain = { anychar - specialchar } ;
escaped = '\\\\', specialchar ;

interpolated = '\$', ? expr ? | '\$(', ? expr ?, ')' ;

styled = '{', ws, properties, ':', content, '}' ;
content = { interpolated | colonescaped | almostplain | styled } ;
colonescaped = escaped | '\\\\:' ;
almostplain = { plain - ':' } ;
properties = property | properties, ws, ',', ws, property ;
property = face | inlineface | keyvalue ;
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
underlinestyled = '(', whitespace, ('' | nothing | simplecolor), whitespace,
                  ',', whitespace, symbol, whitespace ')' ;

inherit = ( '[', inheritval, { ',', inheritval }, ']' ) | inheritval;
inheritval = whitespace, ':'?, symbol ;
```
"""
macro styled_str(raw_content::String)
    #------------------
    # Helper functions
    #------------------

    # If this were a module, I'd define the following struct.

    #= struct State
    content::String         # the (unescaped) input string
    bytes::Vector{UInt8}    # bytes of `content`
    s::Iterators.Stateful   # (index, char) interator of `content`
    parts::Vector{Any}      # the final result
    active_styles::Vector{  # unterminated batches of styles
        Vector{Tuple{Int, Union{Symbol, Expr, Pair{Symbol, Any}}}}}
    pending_styles::Vector{ # terminated styles that have yet to be applied
        Tuple{UnitRange{Int}, Union{Symbol, Expr, Pair{Symbol, Any}}}}
    offset::Ref{Int}        # drift in the `content` index as structures are absorbed
    point::Ref{Int}         # current index in `content`
    escape::Ref{Bool}       # whether the last char was an escape char
    interpolated::Ref{Bool} # whether any string interpolation occurs
    end =#

    # Instead we'll just use a `NamedTuple`
    state = let content = unescape_string(raw_content, ('{', '}', '$', '\n'))
        (; content, bytes = Vector{UInt8}(content),
         s = Iterators.Stateful(zip(eachindex(content), content)),
         parts = Any[],
         active_styles = Vector{Tuple{Int, Union{Symbol, Expr, Pair{Symbol, Any}}}}[],
         pending_styles = Tuple{UnitRange{Int}, Union{Symbol, Expr, Pair{Symbol, Any}}}[],
         offset = Ref(0), point = Ref(1), escape = Ref(false), interpolated = Ref(false))
    end

    # Value restrictions we can check against
    valid_weights = ("thin", "extralight", "light", "semilight", "normal",
                     "medium", "semibold", "bold", "extrabold", "black")
    valid_slants = ("italic", "oblique", "normal")
    valid_underline_styles = ("straight", "double", "curly", "dotted", "dashed")
    valid_colornames =
        ("black", "red", "green", "yellow", "blue", "magenta", "cyan", "white",
         "grey", "gray", "bright_black", "bright_red", "bright_green", "bright_yellow",
         "bright_blue", "bright_magenta", "bright_cyan", "bright_white")

    function stywarn(state, message::String)
        println(stderr, "WARNING: Styled string macro in module ", __module__,
                " at ", something(__source__.file, ""),
                ':', string(__source__.line),
                something(if !isempty(state.s)
                              i, chr = peek(state.s)
                              " (just before '$chr' [$i])"
                          end, ""),
                ", ", message, '.')
    end

    function addpart!(state, stop::Int)
        if state.point[] > stop+state.offset[]+ncodeunits(state.content[stop])-1
            return state.point[] = nextind(state.content, stop) + state.offset[]
        end
        str = String(state.bytes[
            state.point[]:stop+state.offset[]+ncodeunits(state.content[stop])-1])
        push!(state.parts,
              if isempty(state.pending_styles) && isempty(state.active_styles)
                  str
              else
                  styles = Expr[]
                  relevant_styles = Iterators.filter(
                      (start, _)::Tuple -> start <= stop + state.offset[] + 1,
                      Iterators.flatten(state.active_styles))
                  for (start, prop) in relevant_styles
                      range = (start - state.point[]):(stop - state.point[] + state.offset[] + 1)
                      push!(styles, Expr(:tuple, range, prop))
                  end
                  sort!(state.pending_styles, by = first)
                  for (range, prop) in state.pending_styles
                      if !isempty(range)
                          push!(styles, Expr(:tuple, range .- state.point[], prop))
                      end
                  end
                  empty!(state.pending_styles)
                  if isempty(styles)
                      str
                  else
                      :($TaggedString($str, $(Expr(:vect, styles...))))
                  end
              end)
        state.point[] = nextind(state.content, stop) + state.offset[]
    end

    function addpart!(state, start::Int, expr, stop::Int)
        if state.point[] < start
            addpart!(state, start)
        end
        if isempty(state.active_styles)
            push!(state.parts, expr)
        else
            str = gensym("str")
            len = gensym("len")
            tags = Expr(:vect, [
                Expr(:tuple, Expr(:call, UnitRange, 1, len), tag)
                for tag in last.(Iterators.flatten(state.active_styles))]...)
            push!(state.parts,
                  :(let $str = string($expr)
                        $len = ncodeunits($str)
                        $TaggedString($str, $tags)
                    end))
            map!.((_, prop)::Tuple -> (stop + state.offset[] + 1, prop),
                  state.active_styles, state.active_styles)
        end
    end

    function escaped!(state, i, char)
        if char in ('{', '}', '$', '\\')
            deleteat!(state.bytes, i + state.offset[] - 1)
            state.offset[] -= ncodeunits('\\')
        elseif char == '\n'
            skipped = 0
            while last(peek(state.s)) ∈ (' ', '\t')
                popfirst!(state.s)
                skipped += 1
            end
            deleteat!(state.bytes, i+state.offset[]-1:i+skipped+state.offset[])
            state.offset[] -= skipped + ncodeunits("\\\n")
        end
        state.escape[] = false
    end

    function interpolated!(state, i, _)
        expr, nexti = readexpr!(state, i + ncodeunits('$'))
        deleteat!(state.bytes, i + state.offset[])
        state.offset[] -= ncodeunits('$')
        addpart!(state, i, expr, nexti)
        state.point[] = nexti + state.offset[]
        state.interpolated[] = true
    end

    function readexpr!(state, pos::Int)
        expr, nextpos = Meta.parseatom(state.content, pos)
        nchars = length(state.content[pos:prevind(state.content, nextpos)])
        for _ in 1:min(length(state.s), nchars)
            popfirst!(state.s)
        end
        expr, nextpos
    end

    readexpr!(state) = readexpr!(state, first(popfirst!(state.s)) + 1)

    function skipwhitespace!(state)
        isempty(state.s) && return
        while last(peek(state.s)) ∈ (' ', '\t', '\n')
            popfirst!(state.s)
        end
    end

    function begin_style!(state, i, char)
        hasvalue = false
        newstyles = Vector{Tuple{Int, Union{Symbol, Expr, Pair{Symbol, Any}}}}()
        while read_property!(state, i, char, newstyles) end
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

    function end_style!(state, i, char)
        # Close off most recent active style
        for (start, prop) in pop!(state.active_styles)
            push!(state.pending_styles, (start:i+state.offset[], prop))
        end
        deleteat!(state.bytes, i + state.offset[])
        state.offset[] -= ncodeunits('}')
    end

    function read_property!(state, i, char, newstyles)
        skipwhitespace!(state)
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
            stywarn(state, "malformed styled string construct")
            false
        end
    end

    function read_inlineface!(state, i, char, newstyles)
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
            else
                color ∈ valid_colornames ||
                    stywarn(state, "unrecognised named color '$color' (should be $(join(valid_colornames, ", ", ", or ")))")
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
            if last(peek(state.s)) == '('
                popfirst!(state.s)
                skipwhitespace!(state)
                ucolor_str, ucolor = if last(peek(state.s)) == ','
                    lastchar = last(popfirst!(state.s))
                    "", nothing
                else
                    word, lastchar = readsymbol!(state, lastchar)
                    word, parsecolor(word)
                end
                lastchar = nextnonwhitespace!(state, lastchar)
                if !isempty(state) && lastchar == ','
                    skipwhitespace!(state)
                    ustyle, lastchar = readalph!(state, lastchar)
                    lastchar = nextnonwhitespace!(state, lastchar)
                    if lastchar == ')'
                        lastchar = last(popfirst!(state.s))
                    else
                        stywarn(state, "malformed underline value, should be (<color>, <style>)")
                    end
                else
                    stywarn(state, "malformed underline value, should be (<color>, <style>)")
                    ustyle = "straight"
                end
                ustyle ∈ valid_underline_styles ||
                    stywarn(state, "unrecognised underline style '$ustyle' (should be $(join(valid_underline_styles, ", ", ", or ")))")
                Expr(:tuple, ucolor, QuoteNode(Symbol(ustyle)))
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
            if last(peek(state.s)) == ':'
                popfirst!(state.s)
                facename, lastchar = readsymbol!(state, lastchar)
                push!(inherit, Symbol(facename))
            elseif last(peek(state.s)) == '['
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
        # Get on with the parsing now
        popfirst!(state.s)
        kwargs = Expr[]
        needseval = false
        lastchar = '('
        while !isempty(state.s) && lastchar != ')'
            skipwhitespace!(state)
            str_key, lastchar = readalph!(state, lastchar)
            key = Symbol(str_key)
            # Check for '=', and skip over surrounding whitespace
            if lastchar != '='
                skipwhitespace!(state)
                isempty(state.s) && break
                last(peek(state.s)) == '=' || break
                popfirst!(state.s)
            end
            skipwhitespace!(state)
            isempty(state.s) && break
            # Aliases
            key == :fg && (key = :foreground)
            key == :bg && (key = :background)
            # Parse value
            val = if (nextchar = last(peek(state.s))) == '$'
                expr, _ = readexpr!(state)
                lastchar = last(popfirst!(state.s))
                state.interpolated[] = true
                needseval = true
                expr
            elseif key == :face
                if nextchar == '"'
                    first(readexpr!(state, first(peek(state.s))))
                else
                    Iterators.takewhile(
                        c -> (lastchar = last(c)) ∉ (',', ')'), state.s) |>
                            collect .|> last |> String
                end
            elseif key == :height
                if nextchar == '.' || nextchar ∈ '0':'9'
                    num = first(readexpr!(state, first(peek(state.s))))
                    lastchar = last(popfirst!(state.s))
                    ifelse(num isa Number, num, nothing)
                else
                    invalid, lastchar = readsymbol!(state, lastchar)
                    stywarn(state, "invalid height $invalid, should be a natural number or positive float")
                end
            elseif key ∈ (:weight, :slant)
                v, lastchar = readalph!(state, lastchar)
                if key == :weight && v ∉ valid_weights
                    stywarn(state, "unrecognised weight '$v' (should be $(join(valid_weights, ", ", ", or ")))")
                elseif key == :slant && v ∉ valid_slants
                    stywarn(state, "unrecognised slant '$v' (should be $(join(valid_slants, ", ", ", or ")))")
                end
                Symbol(v) |> QuoteNode
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
                stywarn(state, "uses unrecognised face key '$key'")
            end
            if !any(k -> first(k.args) == key, kwargs)
                push!(kwargs, Expr(:kw, key, val))
            else
                stywarn(state, "contains repeated face key '$key'")
            end
            isempty(state.s) || last(peek(state.s)) != ',' || break
        end
        face = Expr(:call, Face, kwargs...)
        push!(newstyles,
              (i + state.offset[] + 1,
               if needseval
                   :(Pair{Symbol, Any}(:face, $face))
               else
                   Pair{Symbol, Any}(:face, eval(face))
               end))
    end

    function read_face_or_keyval!(state, i, char, newstyles)
        function read_curlywrapped!(state)
            popfirst!(state.s) # first '{'
            chars = Char[]
            escaped = false
            while !isempty(state.s)
                _, c = popfirst!(state.s)
                if escaped && c ∈ ('\\', '{', '}', '$')
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
        key = if last(peek(state.s)) == '$'
            expr, _ = readexpr!(state)
            state.interpolated[] = true
            needseval = true
            expr
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
            nextchar = last(peek(state.s))
            if nextchar ∈ (' ', '\t', '\n')
                skipwhitespace!(state)
                nextchar = last(peek(state.s))
            end
            value = if isempty(state.s) ""
            elseif nextchar == '{'
                read_curlywrapped!(state)
            elseif nextchar == '$'
                expr, _ = readexpr!(state)
                state.interpolated[] = true
                needseval = true
                expr
            else
                chars = Char[]
                while (next = peek(state.s)) |> !isnothing && last(next) ∉ (',', ':')
                    popfirst!(state.s)
                    push!(chars, last(next))
                end
                String(chars)
            end
            push!(newstyles,
                  (i + state.offset[] + ncodeunits('{'),
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
                  (i + state.offset[] + ncodeunits('{'),
                   if key isa Symbol || key isa Expr
                       :(Pair{Symbol, Any}(:face, $key))
                   else # Face symbol
                       Pair{Symbol, Any}(:face, Symbol(key))
                   end))
        end
    end

    function run_state_machine!(state)
        # Run the state machine
        for (i, char) in state.s
            if char == '\\'
                state.escape[] = true
            elseif state.escape[]
                escaped!(state, i, char)
            elseif char == '$'
                interpolated!(state, i, char)
            elseif char == '{'
                begin_style!(state, i, char)
            elseif char == '}' && !isempty(state.active_styles)
                end_style!(state, i, char)
            end
        end
        # Ensure that any trailing unstyled content is added
        if state.point[] <= lastindex(state.content) + state.offset[]
            addpart!(state, lastindex(state.content))
        end
        if !isempty(state.active_styles)
            stywarn(state, "contains unterminated styled constructs")
        end
    end

    #------------------
    # The actual body of the macro
    #------------------

    run_state_machine!(state)
    if state.interpolated[]
        :($(Base.taggedstring)($(state.parts...))) |> esc
    else
        Base.taggedstring(map(eval, state.parts)...) |> Base.taggedstring_optimize!
    end
end
