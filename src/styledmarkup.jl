# This file is a part of Julia. License is MIT: https://julialang.org/license

# When updating any part of the parsing code in this file, it is highly
# recommended to also check and/or update `../test/fuzz.jl`.

"""
    StyledMarkup

A sub-module of `StyledStrings` that specifically deals with parsing styled
markup strings. To this end, two entrypoints are provided:

- The [`styled""`](@ref @styled_str) string macro, which is generally preferred.
- They [`styled`](@ref styled) function, which allows for use with runtime-provided strings,
  when needed.

Overall, this module essentially functions as a state machine with a few extra
niceties (like detailed error reporting) sprinkled on top. The overall design
can be largely summed up with the following diagram:

```text
╭String─────────╮
│ Styled markup │
╰──────┬────────╯
       │╭╴[module]
       ││
      ╭┴┴State─╮
      ╰┬───────╯
       │
 ╭╴run_state_machine!╶╮
 │              ╭─────┼─╼ escaped!
 │ Apply rules: │     │
 │  "\\\\" ▶──────╯ ╭───┼─╼[interpolated!] ──▶ readexpr!, addpart!
 │  "\$" ▶────────╯   │
 │  "{"  ▶────────────┼─╼ begin_style! ──▶ read_annotation!
 │  "}"  ▶─────╮      │                     ├─╼ read_inlineface! [readexpr!]
 │             ╰──────┼─╼ end_style!        ╰─╼ read_face_or_keyval!
 │ addpart!(...)      │
 ╰╌╌╌╌╌┬╌╌╌╌╌╌╌╌╌╌╌╌╌╌╯
       │
       ▼
     Result
```

Of course, as usual, the devil is in the details.
"""
module StyledMarkup

using Base: AnnotatedString, annotations, annotatedstring
using ..StyledStrings: Face, SimpleColor

export @styled_str, styled

"""
    State

A struct representing of the parser state (if you squint, a state monad even).

To create the initial state, use the constructor:
    State(content::AbstractString, mod::Union{Module, Nothing}=nothing) -> State

Its fields are as follows:
- `content::String`, the (unescaped) input string
- `bytes::Vector{UInt8}`, the codeunits of `content`. This is a `Vector{UInt8}` instead of a
  `CodeUnits{UInt8}` because we need to be able to modify the array, for instance when erasing
  escape characters.
- `s::Iterators.Stateful`, an `(index, char)` iterator of `content`
- `mod::Union{Module, Nothing}`, the (optional) context with which to evaluate inline
  expressions in. This should be provided iff the styled markup comes from a macro invocation.
- `parts::Vector{Any}`, the result of the parsing, a list of elements that when passed to
  `annotatedstring` produce the styled markup string. The types of its values are highly diverse,
  hence the `Any` element type.
- `active_styles::Vector{Vector{Tuple{Int, Int, Union{Symbol, Expr, Tuple{Symbol, Any}}}}}}`,
  A list of batches of styles that have yet to be applied to any content. Entries of a batch
  consist of `(source_position, start_position, style)` tuples, where `style` may be just
  a symbol (referring to a face), a `Tuple{Symbol, Any}` annotation, or an `Expr` that evaluates
  to a valid annotation (when `mod` is set).
- `pending_styles::Vector{Tuple{UnitRange{Int}, Union{Symbol, Expr, Tuple{Symbol, Any}}}}`,
  A list of styles that have been terminated, and so are known to occur over a certain range,
  but have yet to be applied.
- `offset::Int`, a record of the between the `content` index and the index in the resulting
  styled string, as markup structures are absorbed.
- `point::Int`, the current index in `content`.
- `escape::Bool`, whether the last character seen was an escape character.
- `interpolations::Int`, how many interpolated values have been seen. Knowing whether or not
  anything needs to be evaluated allows the resulting string to be computed at macroexpansion time,
  when possible, and knowing how many allows for some micro-optimisations.
- `errors::Vector`, any errors raised during parsing. We collect them instead of immediately throwing
  so that we can list as many issues as possible at once, instead of forcing the author of the invalid
  styled markup to resolve each issue one at a time. This is expected to be populated by invocations of
  `styerr!`.
"""
mutable struct State
    const content::String
    const bytes::Vector{UInt8}
    const s::Iterators.Stateful
    const mod::Union{Module, Nothing}
    const parts::Vector{Any}
    const active_styles::Vector{Vector{Tuple{Int, Int, Union{Symbol, Expr, Tuple{Symbol, Any}}}}}
    const pending_styles::Vector{Tuple{UnitRange{Int}, Union{Symbol, Expr, Tuple{Symbol, Any}}}}
    offset::Int
    point::Int
    escape::Bool
    interpolations::Int
    const errors::Vector
end

function State(content::AbstractString, mod::Union{Module, Nothing}=nothing)
    State(content, Vector{UInt8}(content), # content, bytes
          Iterators.Stateful(pairs(content)), mod, # s, eval
          Any[], # parts
          Vector{Tuple{Int, Int, Union{Symbol, Expr, Tuple{Symbol, Any}}}}[], # active_styles
          Tuple{UnitRange{Int}, Union{Symbol, Expr, Tuple{Symbol, Any}}}[], # pending_styles
          0, 1, # offset, point
          false, 0, # escape, interpolations
          NamedTuple{(:message, :position, :hint), # errors
                     Tuple{AnnotatedString{String}, <:Union{Int, Nothing}, String}}[])
end

const VALID_FACE_ATTRS = ("font", "foreground", "fg", "background", "bg",
                          "height", "weight", "slant", "underline",
                          "strikethrough", "inverse", "inherit")
const LIKELY_FUTURE_FACE_ATTRS = (:shape, :style, :variant, :features, :alpha)
const VALID_WEIGHTS = ("thin", "extralight", "light", "semilight", "normal",
                       "medium", "semibold", "bold", "extrabold", "black")
const VALID_SLANTS = ("italic", "oblique", "normal")
const VALID_UNDERLINE_STYLES = ("straight", "double", "curly", "dotted", "dashed")

"""
    isnextchar(state::State, char::Char) -> Bool
    isnextchar(state::State, chars::NTuple{N, Char}) -> Bool

Check if `state` has a next character, and if so whether it is `char` or one of `chars`.
"""
function isnextchar end

isnextchar(state::State, c::Char) =
    !isempty(state.s) && last(peek(state.s)) == c

isnextchar(state::State, cs::NTuple{N, Char}) where {N} =
    !isempty(state.s) && last(peek(state.s)) ∈ cs

"""
    ismacro(state::State) -> Bool

Check whether `state` is indicated to come from a macro invocation,
according to whether `state.mod` is set or not.

While this function is rather trivial, it clarifies the intent when used instead
of just checking `state.mod`.
"""
ismacro(state::State) = !isnothing(state.mod)

"""
    styerr!(state::State, message::AbstractString, position::Union{Nothing, Int}=nothing, hint::String="around here")

Register an error in `state` based on erroneous content at or around `position`
(if known, and with a certain `hint` as to the location), with the nature of the
error given by `message`.
"""
function styerr!(state::State, message::AbstractString, position::Union{Nothing, Int}=nothing, hint::String="around here")
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

"""
    hygienic_eval(state::State, expr)

Evaluate `expr` within the scope of `state`'s module.
This replicates part of the behind-the-scenes behaviour of macro expansion, we
just need to manually invoke it due to the particularities around dealing with
code from a foreign module that we parse ourselves.
"""
hygienic_eval(state::State, expr) =
    Core.eval(state.mod, Expr(:var"hygienic-scope", expr, @__MODULE__))

"""
    addpart!(state::State, stop::Int)

Create a new part from `state.point` to `stop`, applying all pending styles.

This consumes all the content between `state.point` and  `stop`, and shifts
`state.point` to be the index after `stop`.
"""
function addpart!(state::State, stop::Int)
    if state.point > stop+state.offset+ncodeunits(state.content[stop])-1
        return state.point = nextind(state.content, stop) + state.offset
    end
    str = String(state.bytes[
        state.point:stop+state.offset+ncodeunits(state.content[stop])-1])
    sty_type, tupl = if ismacro(state)
        Expr, (r, lv) -> :(merge((; region=$r), NamedTuple{(:label, :value), Tuple{Symbol, Any}}($lv)))
    else
        @NamedTuple{region::UnitRange{Int}, label::Symbol, value::Any}, (r, (l, v)) -> (; region=r, label=l, value=v)
    end
    push!(state.parts,
            if isempty(state.pending_styles) && isempty(state.active_styles)
                str
            else
                styles = sty_type[]
                # Turn active styles into pending styles
                relevant_styles = Iterators.filter(
                    (_, start, _)::Tuple -> start <= stop + state.offset + 1,
                    Iterators.flatten(state.active_styles))
                for (_, start, annot) in relevant_styles
                    pushfirst!(state.pending_styles, (start:stop+state.offset+1, annot))
                end
                # Order the pending styles by specificity
                sort!(state.pending_styles, by = (r -> (first(r), -last(r))) ∘ first) # Prioritise the most specific styles
                for (range, annot) in state.pending_styles
                    if !isempty(range)
                        adjrange = (first(range) - state.point):(last(range) - state.point)
                        push!(styles, tupl(adjrange, annot))
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
    state.point = nextind(state.content, stop) + state.offset
end

"""
    addpart!(state::State, start::Int, expr, stop::Int)

Create a new part based on (the eventual evaluation of) `expr`, running from
`start` to `stop`, taking the currently active styles into account.
"""
function addpart!(state::State, start::Int, expr, stop::Int)
    if state.point < start
        addpart!(state, start)
    end
    if isempty(state.active_styles)
        push!(state.parts, expr)
    else
        str = gensym("str")
        len = gensym("len")
        annots = Expr(:vect, [
            :(NamedTuple{(:region, :label, :value), Tuple{UnitRange{Int}, Symbol, Any}}(
                (1:$len, $annot...)))
            for annot in
                map(last,
                    (Iterators.flatten(
                        map(reverse, state.active_styles))))]...)
        if isempty(annots.args)
            push!(state.parts, :(AnnotatedString(string($expr))))
        else
            push!(state.parts,
                :(let $str = string($expr)
                      $len = ncodeunits($str) # Used in `annots`.
                      if Base._isannotated($str) && !isempty($str)
                          AnnotatedString(String($str), vcat($annots, annotations($str)))
                      else
                          if isempty($str)
                              AnnotatedString("")
                          else
                              AnnotatedString($str, $annots)
                          end
                      end
                  end))
        end
        map!.((i, _, annot)::Tuple -> (i, stop + state.offset + 1, annot),
                state.active_styles, state.active_styles)
    end
end

"""
    escaped!(state::State, i::Int, char::Char)

Parse the escaped character `char`, at index `i`, into `state`
"""
function escaped!(state::State, i::Int, char::Char)
    if char in ('{', '}', '\\') || (char == '$' && ismacro(state))
        deleteat!(state.bytes, i + state.offset - 1)
        state.offset -= ncodeunits('\\')
    elseif char ∈ ('\n', '\r')
        skipped = 0
        if char == '\r' && isnextchar(state, '\n')
            popfirst!(state.s)
            skipped += 1
        end
        while isnextchar(state, (' ', '\t'))
            popfirst!(state.s)
            skipped += 1
        end
        deleteat!(state.bytes, i+state.offset-1:i+skipped+state.offset)
        state.offset -= skipped + ncodeunits("\\\n")
    end
    state.escape = false
end

"""
    interpolated!(state::State, i::Int, _)

Interpolate the expression starting at `i`, and add it as a part to `state`.
"""
function interpolated!(state::State, i::Int, _)
    expr, nexti = readexpr!(state, i + ncodeunits('$'))
    deleteat!(state.bytes, i + state.offset)
    state.offset -= ncodeunits('$')
    addpart!(state, i, esc(expr), nexti)
    state.point = nexti + state.offset
    state.interpolations += 1
end

"""
    readexpr!(state::State, pos::Int = first(popfirst!(state.s)) + 1)

Read the expression starting at `pos` in `state.content`, and consume `state.s`
as appropriate to align the iterator to the end of the expression.
"""
function readexpr!(state::State, pos::Int = first(popfirst!(state.s)) + 1)
    if isempty(state.s)
        styerr!(state,
                AnnotatedString("Identifier or parenthesised expression expected after \$ in string",
                                [(55:55, :face, :warning)]),
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

"""
    skipwhitespace!(state::State)

Skip forwards all space, tab, and newline characters in `state.s`
"""
function skipwhitespace!(state::State)
    while isnextchar(state, (' ', '\t', '\n', '\r'))
        popfirst!(state.s)
    end
end

"""
    read_while!(f::Function, state::Base.Stateful, lastchar::Char)

Read `state` until `f(::Char)` is `false`.

Given a `Stateful` that iterates `(_, char::Char)` pairs, and a predicate
`f(::Char)::Bool`, return `(str, lastchar)`, where `str::String` contains all the
`char` for which `f(char) == true`, and `lastchar` the last `char` element seen,
or the input `lastchar` there are no elements of `state`.

# Examples

```julia-repl
julia> s = Base.Stateful(pairs("abc"));

julia> read_while!(isnumeric, s, 'w')
("", 'a')

julia> first(s) # s is mutated
2 => 'b'

julia> read_while!(isascii, Base.Stateful(pairs("123Σω")), 'k')
("123", 'Σ')

julia> read_while!(isascii, Base.Stateful(pairs("abcde")), 'α')
("abcde", 'e')

julia> read_while!(isascii , Base.Stateful(pairs("")), 'k')
("", 'k')
```
"""
function read_while!(f::Function, state::Base.Stateful, lastchar::Char)
    v = Char[]
    for (_, char::Char) in state
        lastchar = char
        if f(char)
            push!(v, char)
        else
            break
        end
    end
    if isempty(v) "" else String(v) end, lastchar
end

"""
    begin_style!(state::State, i::Int, char::Char)

Parse the style declaration beginning at `i` (`char`) with `read_annotation!`,
and register it in the active styles list.
"""
function begin_style!(state::State, i::Int, char::Char)
    hasvalue = false
    newstyles = Vector{Tuple{Int, Int, Union{Symbol, Expr, Tuple{Symbol, Any}}}}()
    while read_annotation!(state, i, char, newstyles) end
    push!(state.active_styles, reverse!(newstyles))
    # Adjust bytes/offset based on how much the index
    # has been incremented in the processing of the
    # style declaration(s).
    if !isempty(state.s)
        nexti = first(peek(state.s))
        deleteat!(state.bytes, i+state.offset:nexti+state.offset-1)
        state.offset -= nexti - i
    end
end

"""
    end_style!(state::State, i::Int, char::Char)

Close of the most recent active style in `state`, making it a pending style.
"""
function end_style!(state::State, i::Int, char::Char)
    for (_, start, annot) in pop!(state.active_styles)
        pushfirst!(state.pending_styles, (start:i+state.offset, annot))
    end
    deleteat!(state.bytes, i + state.offset)
    state.offset -= ncodeunits('}')
end

"""
    read_annotation!(state::State, i::Int, char::Char, newstyles::Vector) -> Bool

Read the annotations at `i` (`char`), and push the style read to `newstyles`.

This skips whitespace and checks what the next character in `state.s` is,
detects the form of the annotation, and parses it using the appropriate
specialised function like so:
- `:`, end of annotation, do nothing
- `(`, inline face declaration, use `read_inlineface!`
- otherwise, use `read_face_or_keyval!`

After parsing the annotation, returns a boolean value signifying whether there
is an immediately subsequent annotation to be read.
"""
function read_annotation!(state::State, i::Int, char::Char, newstyles::Vector)
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
    elseif nextchar ∈ (' ', '\t', '\n', '\r')
        skipwhitespace!(state)
        true
    else
        styerr!(state, "Malformed styled string construct", -1)
        false
    end
end

"""
    read_inlineface!(state::State, i::Int, char::Char, newstyles)

Read an inline face declaration from `state`, at position `i` (`char`), and add
it to `newstyles`.
"""
function read_inlineface!(state::State, i::Int, char::Char, newstyles)
    # Substructure parsing helper functions
    readalph!(state, lastchar) = read_while!(c -> 'a' <= c <= 'z', state.s, lastchar)
    readsymbol!(state, lastchar) = read_while!(∉((' ', '\t', '\n', '\r', ',', ')')), state.s, lastchar)
    function parsecolor(color::String)
        if color == "nothing"
        elseif startswith(color, '#') && length(color) == 7
            tryparse(SimpleColor, color)
        elseif startswith(color, "0x") && length(color) == 8
            tryparse(SimpleColor, '#' * color[3:end])
        else
            SimpleColor(Symbol(color))
        end
    end
    function nextnonwhitespace!(state, lastchar)
        if lastchar ∈ (' ', '\t', '\n', '\r')
            skipwhitespace!(state)
            _, lastchar = popfirst!(state.s)
        end
        lastchar
    end
    function read_underline!(state, lastchar, needseval)
        if isnextchar(state, '(')
            ustart, _ = popfirst!(state.s)
            skipwhitespace!(state)
            ucolor = if isnextchar(state, ',')
                lastchar = last(popfirst!(state.s))
                nothing
            elseif isnextchar(state, '$') && ismacro(state)
                expr, _ = readexpr!(state)
                lastchar = last(popfirst!(state.s))
                state.interpolations += 1
                needseval = true
                esc(expr)
            else
                word, lastchar = readsymbol!(state, lastchar)
                parsecolor(word)
            end
            lastchar = nextnonwhitespace!(state, lastchar)
            ustyle = :straight
            if !isempty(state.s) && lastchar == ','
                skipwhitespace!(state)
                if isnextchar(state, '$') && ismacro(state)
                    expr, _ = readexpr!(state)
                    lastchar = last(popfirst!(state.s))
                    state.interpolations += 1
                    needseval = true
                    ustyle = esc(expr)
                else
                    ustyle_word, lastchar = readalph!(state, lastchar)
                    if ustyle_word ∉ VALID_UNDERLINE_STYLES
                        valid_options = join(VALID_UNDERLINE_STYLES, ", ", ", or ")
                        styerr!(state,
                                AnnotatedString("Invalid underline style '$ustyle_word' (should be $valid_options)",
                                                [(26:25+ncodeunits(ustyle_word), :face, :warning)
                                                 (28+ncodeunits(ustyle_word):39+ncodeunits(ustyle_word)+ncodeunits(valid_options),
                                                  :face, :light)]),
                                -length(ustyle_word) - 3)
                    end
                    ustyle = Symbol(ustyle_word)
                    lastchar = nextnonwhitespace!(state, lastchar)
                end
                if lastchar == ')'
                    lastchar = last(popfirst!(state.s))
                else
                    styerr!(state, "Malformed underline value, should be (<color>, <style>)",
                            -first(something(peek(state.s), (ustart+2, '.'))) + ustart - 1)
                end
            else
                styerr!(state, "Malformed underline value, should be (<color>, <style>)",
                        -first(something(peek(state.s), (ustart+2, '.'))) + ustart - 1)
            end
            if ismacro(state)
                Expr(:tuple, ucolor, if ustyle isa Symbol QuoteNode(ustyle) else ustyle end)
            else
                (ucolor, ustyle)
            end
        else
            word, lastchar = readsymbol!(state, lastchar)
            if word ∈ ("", "nothing")
            elseif word == "true"
                true
            elseif word == "false"
                false
            elseif word ∈ VALID_UNDERLINE_STYLES
                Symbol(word) |> if ismacro(state) QuoteNode else identity end
            else
                parsecolor(word)
            end
        end, lastchar, needseval
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
                facename, lastchar = read_while!(∉((',', ']', ')')), state.s, lastchar)
                push!(inherit, Symbol(rstrip(facename)))
                if lastchar != ','
                    break
                end
            end
        else
            facename, lastchar = read_while!(∉((',', ']', ')')), state.s, lastchar)
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
        val = if ismacro(state) && isnextchar(state, '$')
            expr, _ = readexpr!(state)
            lastchar = last(popfirst!(state.s))
            state.interpolations += 1
            needseval = true
            esc(expr)
        elseif key == :font
            if isnextchar(state, '"')
                readexpr!(state, first(peek(state.s))) |> first
            else
                str, lastchar = read_while!(∉((',', ')')), state.s, lastchar)
                str
            end
        elseif key == :height
            if isnextchar(state, ('.', '0':'9'...))
                num = readexpr!(state, first(peek(state.s))) |> first
                lastchar = last(popfirst!(state.s))
                ifelse(num isa Number, num, nothing)
            else
                invalid, lastchar = readsymbol!(state, lastchar)
                styerr!(state, AnnotatedString("Invalid height '$invalid', should be a natural number or positive float",
                                                [(17:16+ncodeunits(string(invalid)), :face, :warning)]),
                        -3)
            end
        elseif key ∈ (:weight, :slant)
            v, lastchar = readalph!(state, lastchar)
            if key == :weight && v ∉ VALID_WEIGHTS
                valid_options = join(VALID_WEIGHTS, ", ", ", or ")
                styerr!(state, AnnotatedString("Invalid weight '$v' (should be $valid_options)",
                                                [(17:16+ncodeunits(v), :face, :warning),
                                                 (19+ncodeunits(v):30+ncodeunits(v)+ncodeunits(valid_options),
                                                  :face, :light)]),
                        -3)
            elseif key == :slant && v ∉ VALID_SLANTS
                valid_options = join(VALID_SLANTS, ", ", ", or ")
                styerr!(state, AnnotatedString("Invalid slant '$v' (should be $valid_options)",
                                                [(16:15+ncodeunits(v), :face, :warning),
                                                 (18+ncodeunits(v):29+ncodeunits(v)+ncodeunits(valid_options),
                                                  :face, :light)]),
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
            ul, lastchar, needseval = read_underline!(state, lastchar, needseval)
            ul
        elseif key == :inherit
            inherit, lastchar = read_inherit!(state, lastchar)
            inherit
        elseif key in LIKELY_FUTURE_FACE_ATTRS
            # Do nothing so that if the face keys are populated in the future,
            # they only soft-error (i.e. do nothing). This is somewhat
            # consistent with the way the `Faces(...)` constructor works.
        else
            styerr!(state, AnnotatedString(
                "Uses unrecognised face key '$key'. Recognised keys are: $(join(VALID_FACE_ATTRS, ", ", ", and "))",
                [(29:28+ncodeunits(String(key)), :face, :warning)]),
                    -length(str_key) - 2)
        end
        if ismacro(state) && !any(k -> first(k.args) == key, kwargs)
            push!(kwargs, Expr(:kw, key, val))
        elseif !ismacro(state) && !any(kw -> first(kw) == key, kwargs)
            push!(kwargs, key => val)
        else
            styerr!(state, AnnotatedString("Contains repeated face key '$key'",
                                            [(29:28+ncodeunits(String(key)), :face, :warning)]),
                    -length(str_key) - 2)
        end
        isempty(state.s) && styerr!(state, "Incomplete inline face declaration", -1)
        skipwhitespace!(state)
        if lastchar ∈ (',', ')')
        elseif isnextchar(state, (',', ')'))
            _, lastchar= popfirst!(state.s)
        else
            break
        end
    end
    face = Expr(:call, Face, kwargs...)
    push!(newstyles,
          (i, i + state.offset + 1,
           if !ismacro(state)
               :face, Face(; NamedTuple(kwargs)...)
           elseif needseval
               :((:face, $face))
           else
               :face, hygienic_eval(state, face)
           end))
end

"""
    read_face_or_keyval!(state::State, i::Int, char::Char, newstyles)

Read an inline face or key-value pair from `state` at position `i` (`char`), and
add it to `newstyles`.
"""
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
        state.interpolations += 1
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
        skipwhitespace!(state)
        nextchar = if !isempty(state.s) last(peek(state.s)) else '\0' end
        value = if isempty(state.s) ""
        elseif nextchar == '{'
            read_curlywrapped!(state)
        elseif ismacro(state) && nextchar == '$'
            expr, _ = readexpr!(state)
            state.interpolations += 1
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
                (i, i + state.offset + ncodeunits('{'),
                if key isa String && !(value isa Symbol || value isa Expr)
                    Symbol(key), value
                elseif key isa Expr || key isa Symbol
                    :(($key, $value))
                else
                    :(($(QuoteNode(Symbol(key))), $value))
                end))
    elseif key !== ""
        push!(newstyles,
                (i, i + state.offset + ncodeunits('{'),
                if key isa Symbol || key isa Expr
                    :((:face, $key))
                else # Face symbol
                    :face, Symbol(key)
                end))
    end
    if isempty(state.s) || last(peek(state.s)) ∉ (' ', '\t', '\n', '\r', ',', ':')
        styerr!(state, "Incomplete annotation declaration", prevind(state.content, i), "starts here")
    end
end

"""
    run_state_machine!(state::State)

Iterate through `state.s`, applying the parsing rules for the top-level of
syntax and calling the relevant specialised functions.

Upon completion, `state.s` should be fully consumed and `state.parts` fully
populated (along with `state.errors`).
"""
function run_state_machine!(state::State)
    # Run the state machine
    for (i, char) in state.s
        if char == '\\'
            state.escape = true
        elseif state.escape
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
    if state.point <= lastindex(state.content) + state.offset
        addpart!(state, lastindex(state.content))
    end
    for incomplete in Iterators.flatten(state.active_styles)
        styerr!(state, AnnotatedString("Unterminated annotation (missing closing '}')",
                                        [(43:43, :face, :warning)]),
                prevind(state.content, first(incomplete)), "starts here")
    end
end

"""
    annotatedstring_optimize!(str::AnnotatedString)

Merge contiguous identical annotations in `str`.
"""
function annotatedstring_optimize!(s::AnnotatedString)
    length(s.annotations) <= 1 && return s
    last_seen = Dict{Tuple{Symbol, Any}, Int}()
    i = 1
    while i <= length(s.annotations)
        ann = s.annotations[i]
        prev = get(last_seen, (ann.label, ann.value), 0)
        if prev > 0
            lregion = s.annotations[prev].region
            if last(lregion) + 1 == first(ann.region)
                s.annotations[prev] =
                    merge(s.annotations[prev], (; region=first(lregion):last(ann.region)))
                deleteat!(s.annotations, i)
            else
                delete!(last_seen, (ann.label, ann.value))
            end
        else
            last_seen[(ann.label, ann.value)] = i
            i += 1
        end
    end
    s
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
key = ( [^\\0\${}=,:], [^\\0=,:]* ) | interpolated ;
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
underlinestyled = '(', ws, ('' | nothing | simplecolor | interpolated), ws,
                  ',', ws, ( symbol | interpolated ), ws ')' ;

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
    elseif state.interpolations == 1 && length(state.parts) == 1
        :(annotatedstring($(first(state.parts))))
    elseif !iszero(state.interpolations)
        :(annotatedstring($(state.parts...)) |> annotatedstring_optimize!)
    else
        annotatedstring(map(Base.Fix1(hygienic_eval, state), state.parts)...) |> annotatedstring_optimize!
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
        annotatedstring(state.parts...) |> annotatedstring_optimize!
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
