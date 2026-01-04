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
using ..StyledStrings: FACES, STANDARD_FACES, Face, SimpleColor,
    lookupface, lookmakeface, MAGIC_DEFPALETTE_VARNAME, MAGIC_USEPALETTE_VARNAME

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
- `strict::Bool`, whether faces must be known ahead of time
- `parts::Vector{Any}`, the result of the parsing, a list of elements that when passed to
  `annotatedstring` produce the styled markup string. The types of its values are highly diverse,
  hence the `Any` element type.
- `active_styles::Vector{Vector{Tuple{Int, Int, Any, Any}}}}`,
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
mutable struct State{O}
    # Input
    const content::String
    const bytes::Vector{UInt8}
    const s::Iterators.Stateful
    const strict::Bool
    # State
    offset::Int
    point::Int
    escape::Bool
    interpcount::Int
    faceonly::Bool
    # Output
    const activestyles::Vector{@NamedTuple{
        source::Int,
        inds::Vector{Int}}}
    const lastseen::Dict{Tuple{Symbol, Any}, Int}
    const out::O
    const errors::Vector
end

mutable struct MacroOutput
    const mod::Module
    const lets::Vector{Union{Expr, LineNumberNode}}
    const strs::Vector{Union{String, Symbol, Expr}}
    const rfaces::Dict{Symbol, Symbol}
    const annots::Vector{@NamedTuple{
        region::UnitRange{Int},
        loff::Union{Symbol, Nothing},
        roff::Union{Symbol, Nothing},
        label::Union{Symbol, QuoteNode, Expr},
        value::Any}}
    const interpolations::Vector{@NamedTuple{
        startpos::Int,
        startoff::Union{Symbol, Nothing},
        annotidx::Int,
        var::Symbol}}
    dynoff::Union{Symbol, Nothing}
    avtype::Union{Symbol, Nothing}
end

struct FnOutput
    strs::Vector{String}
    annots::Vector{@NamedTuple{
        region::UnitRange{Int},
        label::Symbol,
        value::Any}}
    remapping::IdDict{Face, Face}
    rfaces::Dict{String, Face}
end

function State(content::AbstractString, mod::Union{Module, Nothing}=nothing)
    strict = if mod isa Module
        isdefined(mod, MAGIC_DEFPALETTE_VARNAME) ||
            isdefined(mod, MAGIC_USEPALETTE_VARNAME)
    else
        false
    end
    output = if isnothing(mod)
        FnOutput([], [], FACES.remapping[], Dict())
    else
        MacroOutput(mod, [], [], Dict(), [], [], nothing, nothing)
    end
    State(content, Vector{UInt8}(content), # content, bytes
          Iterators.Stateful(pairs(content)), strict, # s, strict
          # Any[], # parts
          0, 1, false, 0, true, # offset, point, escape, interpcount, faceonly
          # Vector{Tuple{Int, Int, Any, Any}}[], # active_styles
          # Tuple{UnitRange{Int}, Any, Any}[], # pending_styles
          @NamedTuple{source::Int, inds::Vector{Int}}[], # activestyles
          Dict{Tuple{Symbol, Any}, Int}(), # lastseen
          output, # out
          NamedTuple{(:message, :position, :hint), # errors
                     Tuple{AnnotatedString{String, Face}, <:Union{Int, Nothing}, String}}[])
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
according to its output type.

While this function is rather trivial, it clarifies the intent when used.
"""
ismacro(state::State{O}) where {O} = O == MacroOutput

"""
    offadd!(state::State{MacroOutput}, delta::Union{Int, Symbol, Expr})

Adjust the dynamic offset in `state.out.dynoff` by `delta`.
"""
function offadd!(state::State{MacroOutput}, delta::Union{Int, Symbol, Expr})
    tag = if Meta.isexpr(delta, :call) && delta.args[1] == :ncodeunits && delta.args[2] isa Symbol
        chopsuffix(String(delta.args[2]), "_str")
    else
        string(1 + parse(Int, chopprefix(String(something(state.out.dynoff, :offset_0)), "offset_")))
    end
    newoff = Symbol("offset_" * tag)
    push!(state.out.lets, if isnothing(state.out.dynoff)
              :($newoff = $delta)
          else
              :($newoff = $(state.out.dynoff) + $delta)
          end)
    state.out.dynoff = newoff
end

"""
    annotpromote!(state::State{MacroOutput}, newvaltype::Union{Symbol, Expr})

Promote the annotation value type in `state.out` to include `newvaltype`.
"""
function annotpromote!(state::State{MacroOutput}, newvaltype::Union{Symbol, Expr})
    newavtype = Symbol("avtype_" * string(1 + parse(Int, chopprefix(String(something(state.out.avtype, :avtype_0)), "avtype_"))))
    push!(state.out.lets, :($newavtype = promote_type_3u($(something(state.out.avtype, :Face)), $newvaltype)))
    state.out.avtype = newavtype
end

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
    push!(state.errors, (; message=AnnotatedString{String, Face}(message), position, hint))
    nothing
end

"""
    hygienic_eval(state::State, expr)

Evaluate `expr` within the scope of `state`'s module.
This replicates part of the behind-the-scenes behaviour of macro expansion, we
just need to manually invoke it due to the particularities around dealing with
code from a foreign module that we parse ourselves.
"""
hygienic_eval(state::State{MacroOutput}, expr) =
    Core.eval(state.out.mod, Expr(:var"hygienic-scope", expr, @__MODULE__))

"""
    escaped!(state::State, i::Int, char::Char)

Parse the escaped character `char`, at index `i`, into `state`
"""
function escaped!(state::State, i::Int, char::Char)
    if char in ('{', '}', '\\') || (ismacro(state) && char == '$')
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
function interpolated!(state::State{MacroOutput}, i::Int, _)
    # Add any preceding content
    if state.point < i + state.offset
        cstr = String(state.bytes[state.point:i+state.offset-1])
        push!(state.out.strs, cstr)
    end
    startpos = i + state.offset
    # Pull out the expression/variable
    expr, nexti = readexpr!(state, i + ncodeunits('$'))
    deleteat!(state.bytes, (i + state.offset):(nexti + state.offset - 1))
    state.offset -= nexti - i
    state.point = nexti + state.offset
    ivar = if expr isa Symbol
        expr
    else
        isym = gensym("interp_$(length(state.out.interpolations) + 1)")
        push!(state.out.lets, esc(:($isym = $expr)))
        isym
    end
    reref = if expr isa Symbol
        any(==(expr), Iterators.map(((; var),) -> var, state.out.interpolations))
    else
        false
    end
    # Insert the interpolation, and update state
    istr = Symbol("$(ivar)_str")
    reref || push!(state.out.lets, :($istr = string($(esc(ivar)))))
    push!(state.out.strs, istr)
    push!(state.out.interpolations,
          (; startpos = startpos,
             startoff = state.out.dynoff,
             annotidx = lastindex(state.out.annots) + 1,
             var = ivar))
    offadd!(state, :(ncodeunits($istr)))
    state.interpcount += 1
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
                                [(55:55, :face, FACES.pool[:warning])]),
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
    push!(state.activestyles, (source = i, inds = Int[]))
    while read_annotation!(state, i, char) end
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
    if isempty(state.activestyles)
        styerr!(state, "Contains extraneous style terminations", -2, "right here")
        return
    end
    _, inds = pop!(state.activestyles)
    for ind in inds
        annot = state.out.annots[ind]
        start = first(annot.region)
        annot = Base.setindex(annot, start:(i + state.offset - 1), :region)
        if state.out isa MacroOutput
            annot = Base.setindex(annot, state.out.dynoff, :roff)
        end
        state.out.annots[ind] = annot
    end
    deleteat!(state.bytes, i + state.offset)
    state.offset -= ncodeunits('}')
end

"""
    read_annotation!(state::State, i::Int, char::Char) -> Bool

Read the annotations at `i` (`char`), and add them to `state.out`.

This skips whitespace and checks what the next character in `state.s` is,
detects the form of the annotation, and parses it using the appropriate
specialised function like so:
- `:`, end of annotation, do nothing
- `(`, inline face declaration, use `read_inlineface!`
- otherwise, use `read_face_or_keyval!`

After parsing the annotation, returns a boolean value signifying whether there
is an immediately subsequent annotation to be read.
"""
function read_annotation!(state::State, i::Int, char::Char)
    skipwhitespace!(state)
    isempty(state.s) && return false
    nextchar = last(peek(state.s))
    if nextchar == ':'
        popfirst!(state.s)
        return false
    elseif nextchar == '('
        read_inlineface!(state, i, char)
    else
        read_face_or_keyval!(state, i, char)
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
    read_inlineface!(state::State, i::Int, char::Char)

Read an inline face declaration from `state`, at position `i` (`char`), and add
it to `state.out`.
"""
function read_inlineface!(state::State, i::Int, char::Char)
    # Substructure parsing helper functions
    readalph!(state, lastchar) = read_while!(c -> 'a' <= c <= 'z', state.s, lastchar)
    readsymbol!(state, lastchar) = read_while!(∉((' ', '\t', '\n', '\r', ',', ')')), state.s, lastchar)
    function parsecolor(color::String)
        if color == "nothing"
        elseif startswith(color, '#') && length(color) == 7
            tryparse(SimpleColor, color)
        elseif startswith(color, "0x") && length(color) == 8
            tryparse(SimpleColor, '#' * color[3:end])
        elseif !ismacro(state)
            SimpleColor(get(FACES.pool, Symbol(color), STANDARD_FACES.default))
        else
            resolveface(state, color)
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
                state.interpcount += 1
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
                    state.interpcount += 1
                    needseval = true
                    ustyle = esc(expr)
                else
                    ustyle_word, lastchar = readalph!(state, lastchar)
                    if ustyle_word ∉ VALID_UNDERLINE_STYLES
                        valid_options = join(VALID_UNDERLINE_STYLES, ", ", ", or ")
                        styerr!(state,
                                AnnotatedString("Invalid underline style '$ustyle_word' (should be $valid_options)",
                                                [(26:25+ncodeunits(ustyle_word), :face, FACES.pool[:warning])
                                                 (28+ncodeunits(ustyle_word):39+ncodeunits(ustyle_word)+ncodeunits(valid_options),
                                                  :face, FACES.pool[:light])]),
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
        inherit = if ismacro(state)
            Union{Face, Expr}[]
        else
            Face[]
        end
        if isnextchar(state, ':')
            popfirst!(state.s)
            facename, lastchar = readsymbol!(state, lastchar)
            Base.depwarn("Using symbols to refer to faces is deprecated as of v1.14. Use direct names and palettes instead.", Symbol("@styled_str"))
            push!(inherit, resolveface(state, facename))
        elseif isnextchar(state, '[')
            popfirst!(state.s)
            readvec = true
            while readvec
                skipwhitespace!(state)
                nextchar = last(peek(state.s))
                if nextchar == ':'
                    Base.depwarn("Using symbols to refer to faces is deprecated as of v1.14. Use direct names and palettes instead.", Symbol("@styled_str"))
                    popfirst!(state.s)
                elseif nextchar == ']'
                    popfirst!(state.s)
                    lastchar = last(popfirst!(state.s))
                    break
                end
                facename, lastchar = read_while!(∉((',', ']', ')')), state.s, lastchar)
                push!(inherit, resolveface(state, String(rstrip(facename))))
                if lastchar != ','
                    break
                end
            end
        else
            facename, lastchar = read_while!(∉((',', ']', ')')), state.s, lastchar)
            push!(inherit, resolveface(state, String(rstrip(facename))))
        end
        if ismacro(state)
            Expr(:vect, inherit...)
        else
            inherit
        end, lastchar
    end
    # Tentatively initiate the parsing
    popfirst!(state.s)
    lastchar = '('
    skipwhitespace!(state)
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
            state.interpcount += 1
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
                                                [(17:16+ncodeunits(string(invalid)), :face, FACES.pool[:warning])]),
                        -3)
            end
        elseif key ∈ (:weight, :slant)
            v, lastchar = readalph!(state, lastchar)
            if key == :weight && v ∉ VALID_WEIGHTS
                valid_options = join(VALID_WEIGHTS, ", ", ", or ")
                styerr!(state, AnnotatedString("Invalid weight '$v' (should be $valid_options)",
                                                [(17:16+ncodeunits(v), :face, FACES.pool[:warning]),
                                                 (19+ncodeunits(v):30+ncodeunits(v)+ncodeunits(valid_options),
                                                  :face, FACES.pool[:light])]),
                        -3)
            elseif key == :slant && v ∉ VALID_SLANTS
                valid_options = join(VALID_SLANTS, ", ", ", or ")
                styerr!(state, AnnotatedString("Invalid slant '$v' (should be $valid_options)",
                                                [(16:15+ncodeunits(v), :face, FACES.pool[:warning]),
                                                 (18+ncodeunits(v):29+ncodeunits(v)+ncodeunits(valid_options),
                                                  :face, FACES.pool[:light])]),
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
                [(29:28+ncodeunits(String(key)), :face, FACES.pool[:warning])]),
                    -length(str_key) - 2)
        end
        let key = key # Avoid boxing from closure capture
            if ismacro(state) && !any(k -> first(k.args) == key, kwargs)
                push!(kwargs, Expr(:kw, key, val))
            elseif !ismacro(state) && !any(kw -> first(kw) == key, kwargs)
                push!(kwargs, key => val)
            else
                styerr!(state, AnnotatedString("Contains repeated face key '$key'",
                                                [(29:28+ncodeunits(String(key)), :face, FACES.pool[:warning])]),
                        -length(str_key) - 2)
            end
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
    faceval = if ismacro(state)
        faceex = Expr(:call, Face, kwargs...)
        if needseval
            faceex
        else
            hygienic_eval(state, faceex)
        end
    else
        Face(; NamedTuple(kwargs)...)
    end
    addannot!(state, i,
              if ismacro(state) QuoteNode(:face) else :face end,
              faceval)
end

"""
    resolveface(state::State, facename::String)

Determine the face referred to by `facename`, according to `state`.

Returns a `Face`, or when `state` is from a macro invocation an
`Expr` that evaluates to a `Face` may be returned.
"""
function resolveface(state::State, facename::String)
    ismacro(state) || return lookmakeface(Symbol(facename))
    if '.' in facename
        components = map(Symbol, eachsplit(facename, '.'))
        push!(components, :base, last(components))
        components[end-2] = MAGIC_DEFPALETTE_VARNAME
        esc(foldl((a, b) -> Expr(:., a, QuoteNode(b)), components[2:end]; init = first(components)))
    elseif state.strict
        lookupface(state.out.mod, Symbol(facename))
    else
        Expr(:call, lookmakeface, state.out.mod, QuoteNode(Symbol(facename)))
    end
end

"""
    addannot!(state::State, i::Int, label, value)

Create a new annotation in `state.out` at position `i`, with `label` and `value`.

This also checks whether an identical annotation already exists that
is adjacent to the current position, and if so extends its region.
"""
function addannot!(state::State, i::Int, label, value)
    region = (i + state.offset):typemax(Int)
    styannot = if ismacro(state)
        (region = region,
         loff = state.out.dynoff,
         roff = nothing,
         label = if label isa String
             QuoteNode(Symbol(label))
         else
             label
         end,
         value = value)
    else
        (region = region,
         label = Symbol(label),
         value = value)
    end
    for (a, annot) in Iterators.reverse(enumerate(state.out.annots))
        first(annot.region) < i + state.offset && break
        if annot.label == styannot.label && annot.value == styannot.value && (!ismacro(state) || annot.loff == styannot.loff)
            push!(state.activestyles[end].inds, a)
            return
        end
    end
    push!(state.out.annots, styannot)
    push!(state.activestyles[end].inds, lastindex(state.out.annots))
end

"""
    read_face_or_keyval!(state::State, i::Int, char::Char)

Read an inline face or key-value pair from `state` at position `i` (`char`), and
add it to `state.out`.
"""
function read_face_or_keyval!(state::State, i::Int, char::Char)
    function read_curlywrapped!(state)
        popfirst!(state.s) # first '{'
        buffer = Char[]
        escaped = false
        while !isempty(state.s)
            _, c = popfirst!(state.s)
            if escaped && (c ∈ ('\\', '{', '}') || (ismacro(state) && c == '$'))
                push!(buffer, c)
                escaped = false
            elseif escaped
                push!(buffer, '\\', c)
                escaped = false
            elseif c == '\\'
                escaped = true
            elseif c == '}'
                break
            else
                push!(buffer, c)
            end
        end
        String(buffer)
    end
    # this isn't the 'last' char yet, but it will be
    key = if ismacro(state) && last(peek(state.s)) == '$'
        expr, _ = readexpr!(state)
        state.interpcount += 1
        needseval = true
        if expr isa Symbol
            esc(expr)
        else
            kvar = Symbol("key_$(length(state.out.lets) + 1)")
            push!(state.out.lets, :($kvar = $(esc(expr))))
            kvar
        end
    else
        chars = Char[]
        while (next = peek(state.s)) |> !isnothing && last(next) ∉ (',', '=', ':', ' ', '\t', '\n', '\r')
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
        value = if isempty(state.s)
            "" # An error will be raised later for an incomplete declaration
        elseif ismacro(state) && nextchar == '$'
            expr, _ = readexpr!(state)
            state.interpcount += 1
            needseval = true
            vvar = if expr isa Symbol
                esc(expr)
            else
                vsym = Symbol("value_$(state.interpcount)")
                push!(state.out.lets, :($vsym = $(esc(expr))))
                vsym
            end
            annotpromote!(state, :(typeof($vvar)))
            vvar
        elseif nextchar == '{'
            read_curlywrapped!(state)
        else
            chars = Char[]
            while (next = peek(state.s)) |> !isnothing && last(next) ∉ (',', ':')
                popfirst!(state.s)
                push!(chars, last(next))
            end
            String(chars)
        end
        state.faceonly = false
        if ismacro(state) && value isa String
            annotpromote!(state, :String)
        end
        addannot!(state, i, key, value)
    elseif key !== ""
        face = if !ismacro(state)
            rface = get(state.out.rfaces, key, nothing)
            if !isnothing(rface)
                rface
            else
                fref = get(FACES.pool, Symbol(replace(key, '.' => '_')), Face())
                state.out.rfaces[key] = if fref !== Face()
                    get(state.out.remapping, fref, fref)
                else
                    fref
                end
            end
        elseif haskey(state.out.rfaces, Symbol(key))
            state.out.rfaces[Symbol(key)]
        else
            fval = if key isa Symbol || key isa Expr
                fivar = Symbol("iface_$(length(state.out.lets) + 1)")
                push!(state.out.lets, :($fivar = $interpface($key, $(state.strict))))
                fivar
            else
                resolveface(state, key)
            end
            fvar = if key isa String
                Symbol("face_$key")
            else
                Symbol("face_$(length(state.out.lets) + 1)")
            end
            isempty(state.out.rfaces) && push!(state.out.lets, :(faceremap = FACES.remapping[]))
            push!(state.out.lets, :($fvar = get(faceremap, $fval, $fval)))
            state.out.rfaces[Symbol(key)] = fvar
            fvar
        end
        addannot!(state, i,
                  if ismacro(state) QuoteNode(:face) else :face end,
                  face)
    end
    if isempty(state.s) || last(peek(state.s)) ∉ (' ', '\t', '\n', '\r', ',', ':')
        styerr!(state, "Incomplete annotation declaration", prevind(state.content, i), "starts here")
    end
end

"""
    promote_type_3u(A::Type, B::Type) -> Type

Promote types `A` and `B`, producing a `Union` of up to 3 types before `Any`.
"""
function promote_type_3u(A::Type, B::Type)
    AB = promote_type(A, B)
    if AB == Any
        AuB = Union{A, B}
        Base.unionlen(AuB) <= 3 && return AuB
    end
    AB
end

"""
    interpface(face, strict::Bool) -> Face

Resolve an interpolated `face` from styled markup.
"""
function interpface end

interpface(face::Face, ::Bool) = face

function interpface(face::Symbol, strict::Bool)
    Base.depwarn("Using symbols to refer to faces is deprecated as of v1.14. Use direct names and palettes instead.", Symbol("@styled_str"))
    if strict
        lookupface(Main, face)
    else
        lookmakeface(Main, face)
    end
end

interpface(face, ::Bool) = throw(ArgumentError("Face interpolation must evaluate to a Symbol or Face"))

"""
    run_state_machine!(state::State)

Iterate through `state.s`, applying the parsing rules for the top-level of
syntax and calling the relevant specialised functions.

Upon completion, `state.s` should be fully consumed and `state.out` fully
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
            end_style!(state, i, char)
        end
    end
    # Ensure that any trailing content is added
    if state.point <= lastindex(state.content) + state.offset
        push!(state.out.strs, String(state.bytes[
            state.point:(lastindex(state.content) + state.offset)]))
    end
    for (; source) in state.activestyles
        styerr!(state, AnnotatedString("Incomplete annotation (missing closing '}')",
                                        [(41:41, :face, FACES.pool[:warning])]),
                prevind(state.content, source), "starts here")
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
    spliceinterps!(state::State, avar::Symbol) -> Symbol

Splice the interpolated annotations in `state.out.interpolations` into
`avar`, returning the new variable name containing the spliced annotations.
"""
function spliceinterps!(state::State{MacroOutput}, avar::Symbol)
    interps = state.out.interpolations
    newavar = :interp_annots
    isempty(interps) && return avar
    push!(state.out.lets, :(interp_annot_count = 0))
    avars = Symbol[]
    avtype0 = something(state.out.avtype, :Face)
    varinstances = Dict{Symbol, Vector{Int}}()
    for (i, (; var)) in enumerate(interps)
        push!(get!(() -> Int[], varinstances, var), i)
    end
    for (i, (; annotidx, var)) in enumerate(interps)
        iavar = Symbol("annot_$var")
        push!(avars, iavar)
        insts = varinstances[var]
        i == first(insts) || continue
        newavtype = Symbol("avtype_$var")
        annlen = if length(insts) == 1
            :(length($iavar))
        else
            :($(length(insts)) * length($iavar))
        end
        append!(state.out.lets,
                (quote
                    local $iavar, $newavtype
                    if hasmethod(annotations, (typeof($(esc(var))),))
                        $iavar = annotations($(esc(var)))
                        interp_annot_count += $annlen
                        $newavtype = promote_type_3u($(something(state.out.avtype, :Face)), typeofval(eltype($iavar)))
                    else
                        $iavar = $(@NamedTuple{region::UnitRange{Int}, label::Symbol, value::Face}[])
                        $newavtype = $(something(state.out.avtype, :Face))
                    end
                end).args)
        state.out.avtype = newavtype
    end
    iexprs = Union{Expr, LineNumberNode}[]
    avtype = :(NamedTuple{(:region, :label, :value), Tuple{UnitRange{Int}, Symbol, $(something(state.out.avtype, :Face))}})
    # Special case: a single interpolation that starts at position 1
    if length(interps) == 1 && first(interps).startpos == 1 && first(interps).annotidx == lastindex(state.out.annots) + 1
        append!(state.out.lets,
                (quote
                     $newavar = if $avtype0 == $(something(state.out.avtype, :Face))
                         $avar
                     else
                         Vector{$avtype}($avar)
                     end
                     iszero(interp_annot_count) ||
                         append!($newavar, $(first(avars)))
                 end).args)
    else
        # We now know we need to adjust the annotation regions / deal with multiple annotations
        push!(iexprs, :($newavar = Vector{$avtype}(undef, length($avar) + interp_annot_count)))
        push!(iexprs, :(annot_offset = 0))
        if first(interps).annotidx == 2
            push!(iexprs, :($newavar[1] = $avar[1]))
        elseif first(interps).annotidx > 1
            push!(iexprs,
                  :(copyto!($newavar,
                            1,
                            $avar,
                            1,
                            $(first(interps).annotidx - 1))))
        end
        lastannotidx = first(interps).annotidx
        for ((; startpos, startoff, annotidx, var), annots) in zip(interps, avars)
            if lastannotidx != annotidx
                push!(iexprs, if annotidx - lastannotidx == 1
                          :($newavar[$lastannotidx + annot_offset] = $avar[$(annotidx - 1)])
                      else
                          :(copyto!($newavar,
                                    $lastannotidx + annot_offset,
                                    $avar,
                                    $lastannotidx,
                                    $(annotidx - lastannotidx)))
                      end)
            end
            push!(iexprs,
                  :(if !isempty($annots)
                        ioffset = $(if isnothing(startoff)
                                       startpos - 1
                                   else
                                       :($(startpos - 1) + $startoff)
                                   end)
                        for i in eachindex($annots)
                            anni = $annots[i]
                            $newavar[$(annotidx - 1) + annot_offset + i] =
                                (; region = (first(anni.region) + ioffset):(last(anni.region) + ioffset),
                                   label = anni.label,
                                   value = anni.value)
                        end
                        annot_offset += length($annots)
                    end))
            lastannotidx = annotidx
        end
        if last(interps).annotidx < lastindex(state.out.annots) + 1
            push!(iexprs,
                  :(copyto!($newavar,
                            $(last(interps).annotidx + 1) + annot_offset,
                            $avar,
                            $(last(interps).annotidx + 1))))
        end
        push!(state.out.lets,
              Expr(:local, newavar),
              :(if !iszero(interp_annot_count)
                    $(iexprs...)
                else
                    $newavar = $avar
                end))
    end
    newavar
end

typeofval(::Type{@NamedTuple{region::UnitRange{Int}, label::Symbol, value::T}}) where {T} = T

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
    isempty(state.errors) || throw(MalformedStylingMacro(state.content, state.errors))
    astr = if length(state.out.strs) == 1
        first(state.out.strs)
    else
        :(string($(state.out.strs...)))
    end
    annots = map(state.out.annots) do (; region, label, value, loff, roff)
        start, stop = first(region), last(region)
        if !isnothing(loff)
            start = :($start + $loff)
        end
        if !isnothing(roff)
            stop = :($stop + $roff)
        end
        Expr(:tuple, Expr(:parameters,
                          Expr(:kw, :region, Expr(:call, :(:), start, stop)),
                          Expr(:kw, :label, label),
                          Expr(:kw, :value, value)))
    end
    atype = :(NamedTuple{(:region, :label, :value), Tuple{UnitRange{Int}, Symbol, $(something(state.out.avtype, :Face))}})
    avec = :($atype[$(annots...)])
    aexp = :($AnnotatedString($astr, $avec))
    if !isempty(state.out.interpolations)
        push!(state.out.lets, :(annots = $avec))
        newavar = spliceinterps!(state, :annots)
        aexp = :($AnnotatedString($astr, $newavar))
    end
    Expr(:let, Expr(:block), Expr(:block, state.out.lets..., aexp))
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
        AnnotatedString(join(state.out.strs), state.out.annots)
    end
end

struct MalformedStylingMacro <: Exception
    raw::String
    problems::Vector{NamedTuple{(:message, :position, :hint), Tuple{AnnotatedString{String, Face}, <:Union{Int, Nothing}, String}}}
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
