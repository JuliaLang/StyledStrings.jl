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

using Base: AnnotatedString, AnnotatedChar, annotations, annotatedstring
using ..StyledStrings: FACES, Face, SimpleColor, Link, StyledValue,
    ATTRIBUTES, FACE_KEYWORDS,
    lookupface, lookmakeface, nameface, similarface, isambiguous, registrykey, heightbits,
    UnknownFaceError, UNDEF_CUSTOM_HEIGHT_FLAG, UNDEF_INUSE_HEIGHT_FLAG,
    MAGIC_DEFPALETTE_VARNAME, MAGIC_USEPALETTE_VARNAME

export @styled_str, styled

"""
    State

A struct representing of the parser state (if you squint, a state monad even).

To create the initial state, use the constructor:
    State(content::AbstractString, mod::Union{Module, Nothing}=nothing) -> State

The module `mod` should be provided iff the styled markup comes from a macro
invocation. Its fields are as follows:
- `content::String`, the (unescaped) input string
- `bytes::Vector{UInt8}`, the codeunits of `content`. This is a `Vector{UInt8}` instead of a
  `CodeUnits{UInt8}` because we need to be able to modify the array, for instance when erasing
  escape characters.
- `s::Iterators.Stateful`, an `(index, char)` iterator of `content`
- `strict::Bool`, whether faces must be known ahead of time, as in a module with a palette
- `offset::Int`, the difference between the `content` index and the index in the resulting
  styled string, as markup structures are absorbed.
- `point::Int`, the current index in `content`.
- `activestyles::Vector{@NamedTuple{source::Int, inds::Vector{Int}}}`, the styled regions
  that are still open: the position of each region's markup, and the indices in `out` of
  its annotations.
- `out::O`, the result of the parsing. A macro invocation produces a `MacroOutput`, the
  code that builds the string. Otherwise it is an `FnOutput`, the strings and annotations.
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
    # Output
    const activestyles::Vector{@NamedTuple{
        source::Int,
        inds::Vector{Int}}}
    const out::O
    const errors::Vector
end

mutable struct MacroOutput
    const mod::Module
    const lets::Vector{Union{Expr, LineNumberNode}}
    const binds::Vector{Pair{Union{Symbol, Expr}, Expr}} # The user's expressions, bound first so that their assignments reach the caller
    const strs::Vector{Union{String, Symbol, Expr}}
    const rfaces::Dict{Symbol, Union{Face, Symbol}}
    const annots::Vector{@NamedTuple{
        region::UnitRange{Int},
        loff::Union{Symbol, Nothing},
        roff::Union{Symbol, Nothing},
        value::Any}}
    const interpolations::Vector{@NamedTuple{
        startpos::Int,
        startoff::Union{Symbol, Nothing},
        annotidx::Int,
        var::Symbol}}
    dynoff::Union{Symbol, Nothing}
    avtype::Union{Symbol, Type}
end

struct FnOutput
    strs::Vector{String}
    annots::Vector{@NamedTuple{
        region::UnitRange{Int},
        value::StyledValue}}
    skipothers::Bool # Skip `key=value` annotations other than faces and links, rather than reject them
end

function State(content::AbstractString, mod::Union{Module, Nothing}=nothing; skipothers::Bool = false)
    str = String(content)
    strict = !isnothing(mod) && (isdefined(mod, MAGIC_DEFPALETTE_VARNAME) || isdefined(mod, MAGIC_USEPALETTE_VARNAME))
    output = if isnothing(mod)
        FnOutput([], [], skipothers)
    else
        MacroOutput(mod, [], [], [], Dict(), [], [], nothing, StyledValue)
    end
    State(str, Vector{UInt8}(str), # content, bytes
          Iterators.Stateful(pairs(str)), strict, # s, strict
          0, 1, # offset, point
          @NamedTuple{source::Int, inds::Vector{Int}}[], # activestyles
          output, # out
          NamedTuple{(:message, :position, :hint), # errors
                     Tuple{AnnotatedString{String, StyledValue}, Int, String}}[])
end

const LIKELY_FUTURE_FACE_ATTRS = (:shape, :style, :variant, :features, :alpha)
const VALID_WEIGHTS = map(String, ATTRIBUTES.weights)
const VALID_SLANTS = map(String, ATTRIBUTES.slants)
const VALID_UNDERLINE_STYLES = map(String, ATTRIBUTES.underlines)

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

ismacro(::State{O}) where {O} = O == MacroOutput

"""
    annotpromote!(state::State{MacroOutput}, newvaltype::Union{Symbol, Expr, Type})

Promote the annotation value type in `state.out` to include `newvaltype`.
"""
function annotpromote!(state::State{MacroOutput}, newvaltype::Union{Symbol, Expr, Type})
    newavtype = gensym(:avtype)
    push!(state.out.lets, :($newavtype = promote_type_4u($(state.out.avtype), $newvaltype)))
    state.out.avtype = newavtype
end

"""
    styerr!(state::State, message::AbstractString, position::Int, hint::String="around here")

Register an error in `state` based on erroneous content at or around `position`
(with a certain `hint` as to the location), with the nature of the error given
by `message`.
"""
function styerr!(state::State, message::AbstractString, position::Int, hint::String="around here")
    if position < 0
        position = prevind(
            state.content,
            if !isempty(state.s)
                first(peek(state.s))
            else
                lastindex(state.content)
            end,
            -position)
    end
    push!(state.errors, (; message=AnnotatedString{String, StyledValue}(message), position, hint))
    nothing
end

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
end

"""
    interpolated!(state::State, i::Int)

Interpolate the expression starting at `i`, and add it as a part to `state`.
"""
function interpolated!(state::State{MacroOutput}, i::Int)
    if state.point < i + state.offset
        cstr = String(state.bytes[state.point:i+state.offset-1])
        push!(state.out.strs, cstr)
    end
    startpos = i + state.offset
    expr, nexti = readexpr!(state, i + ncodeunits('$'))
    deleteat!(state.bytes, (i + state.offset):(nexti + state.offset - 1))
    state.offset -= nexti - i
    state.point = nexti + state.offset
    ivar, reref = if expr isa Symbol
        expr, any(interp -> interp.var === expr, state.out.interpolations)
    else
        isym = gensym(:interp)
        push!(state.out.binds, esc(isym) => esc(expr))
        isym, false
    end
    istr = Symbol("$(ivar)_str")
    # `string` is the identity on an `AnnotatedString`
    reref || push!(state.out.lets, :($istr = String(string($(esc(ivar))))))
    push!(state.out.strs, istr)
    push!(state.out.interpolations,
          (; startpos = startpos,
             startoff = state.out.dynoff,
             annotidx = lastindex(state.out.annots) + 1,
             var = ivar))
    # Each interpolation shifts the regions after it by its length, only known when the string is built
    newoff = gensym(:offset)
    push!(state.out.lets, if isnothing(state.out.dynoff)
              :($newoff = ncodeunits($istr))
          else
              :($newoff = $(state.out.dynoff) + ncodeunits($istr))
          end)
    state.out.dynoff = newoff
end

# Bind the user's `expr` in the bindings of the generated `let`, rather than its body, so that
# assignments within it reach the caller as they would outside the macro
function bindexpr!(state::State, expr)
    expr isa Symbol && return esc(expr) # A variable, with nothing to evaluate
    var = gensym(:interp)
    push!(state.out.binds, var => esc(expr))
    var
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
                                [(55:55, FACES.pool[:warning])]),
                -1, "right here")
        return "", pos
    end
    expr, nextpos = Meta.parseatom(state.content, pos)
    if Meta.isexpr(expr, (:error, :incomplete))
        detail = expr.args[1].detail
        after = if state.content[prevind(state.content, pos)] == '$' " after \$" else "" end # Not for `font=` or `height=`
        if detail isa Base.JuliaSyntax.ParseError && !isempty(detail.diagnostics)
            (; message, first_byte) = first(detail.diagnostics)
            styerr!(state, "Invalid expression$after: $message",
                    prevind(state.content, min(first_byte, lastindex(state.content))), "right here")
        else
            styerr!(state, "Invalid expression$after", prevind(state.content, prevind(state.content, pos)), "starting here")
        end
    end
    nchars = length(state.content[pos:prevind(state.content, nextpos)])
    for _ in 1:nchars
        isempty(state.s) && break
        popfirst!(state.s)
    end
    expr, nextpos
end

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
or the input `lastchar` if there are no elements of `state`.

# Examples

```julia-repl
julia> read_while!(isascii, Base.Stateful(pairs("123Σω")), 'k')
("123", 'Σ')
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
    String(v), lastchar
end

"""
    begin_style!(state::State, i::Int)

Parse the style declaration beginning at `i` with `read_annotation!`,
and register it in the active styles list.
"""
function begin_style!(state::State, i::Int)
    push!(state.activestyles, (source = i, inds = Int[]))
    while read_annotation!(state, i) end
    # Drop the markup from the output
    if !isempty(state.s)
        nexti = first(peek(state.s))
        deleteat!(state.bytes, i+state.offset:nexti+state.offset-1)
        state.offset -= nexti - i
    end
end

"""
    end_style!(state::State, i::Int)

Close of the most recent active style in `state`, making it a pending style.
"""
function end_style!(state::State, i::Int)
    if isempty(state.activestyles)
        styerr!(state, "Contains extraneous style terminations", prevind(state.content, i), "right here")
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
    read_annotation!(state::State, i::Int) -> Bool

Read the annotations at `i`, and add them to `state.out`.

This skips whitespace and checks what the next character in `state.s` is,
detects the form of the annotation, and parses it using the appropriate
specialised function like so:
- `:`, end of annotation, do nothing
- `(`, inline face declaration, use `read_inlineface!`
- otherwise, use `read_face_or_keyval!`

After parsing the annotation, returns a boolean value signifying whether there
is an immediately subsequent annotation to be read.
"""
function read_annotation!(state::State, i::Int)
    skipwhitespace!(state)
    isempty(state.s) && return false
    nextchar = last(peek(state.s))
    if nextchar == ':'
        popfirst!(state.s)
        return false
    elseif nextchar == '('
        read_inlineface!(state, i)
    else
        read_face_or_keyval!(state, i)
    end
    skipwhitespace!(state)
    isempty(state.s) && return false
    nextchar = last(peek(state.s))
    if nextchar == ','
        popfirst!(state.s)
        true
    elseif nextchar == ':'
        true
    else
        styerr!(state, "Expected ',' or ':' after an annotation", prevind(state.content, first(peek(state.s))), "right here")
        false
    end
end

# Whether generated code holds a value known only when the string is built: an interpolation or a lookup
isruntime(x) = x isa Symbol || x isa Expr && (x.head === :escape || x.head === :call && x.args[1] === lookmakeface || any(isruntime, x.args))

"""
    read_inlineface!(state::State, i::Int)

Read an inline face declaration from `state`, at position `i`, and add
it to `state.out`.
"""
function read_inlineface!(state::State, i::Int)
    readalph!(state, lastchar) = read_while!(c -> 'a' <= c <= 'z', state.s, lastchar)
    readsymbol!(state, lastchar) = read_while!(∉((' ', '\t', '\n', '\r', ',', ')')), state.s, lastchar)
    function parsecolor(color::String)
        if color == "nothing"
        elseif startswith(color, '#') || startswith(color, "0x")
            rgb = tryparse(SimpleColor, color)
            isnothing(rgb) && styerr!(state, "Invalid colour '$color', should be #rrggbb or 0xrrggbb", -length(color) - 2)
            rgb
        else
            resolveface(state, color)
        end
    end
    # The highlights follow the length of the "Invalid $what '" prefix
    function invalidchoice!(what::String, word::String, choices)
        valid = join(choices, ", ", ", or ")
        pre, n = ncodeunits("Invalid $what '"), ncodeunits(word)
        styerr!(state, AnnotatedString("Invalid $what '$word' (should be $valid)",
                                       [(pre+1:pre+n, FACES.pool[:warning]),
                                        (pre+n+3:pre+n+14+ncodeunits(valid), FACES.pool[:light])]),
                -length(word) - 2)
    end
    function nextnonwhitespace!(state, lastchar)
        lastchar ∈ (' ', '\t', '\n', '\r') || return lastchar
        skipwhitespace!(state)
        isempty(state.s) && return lastchar
        last(popfirst!(state.s))
    end
    function read_underline!(state, lastchar)
        if isnextchar(state, '(')
            ustart, _ = popfirst!(state.s)
            skipwhitespace!(state)
            ucolor = if isnextchar(state, ',')
                lastchar = last(popfirst!(state.s))
                nothing
            elseif isnextchar(state, '$') && ismacro(state)
                expr, _ = readexpr!(state)
                isempty(state.s) && return nothing, lastchar
                lastchar = last(popfirst!(state.s))
                :($interpattr($(bindexpr!(state, expr)), $(state.out.mod), $(state.strict)))
            else
                word, lastchar = readsymbol!(state, lastchar)
                parsecolor(word)
            end
            lastchar = nextnonwhitespace!(state, lastchar)
            ustyle = QuoteNode(:straight)
            wellformed = !isempty(state.s) && lastchar == ','
            if wellformed
                skipwhitespace!(state)
                if isnextchar(state, '$') && ismacro(state)
                    expr, _ = readexpr!(state)
                    isempty(state.s) && return nothing, lastchar
                    lastchar = last(popfirst!(state.s))
                    ustyle = bindexpr!(state, expr)
                else
                    ustyle_word, lastchar = readalph!(state, lastchar)
                    ustyle_word ∈ VALID_UNDERLINE_STYLES ||
                        invalidchoice!("underline style", ustyle_word, VALID_UNDERLINE_STYLES)
                    ustyle = QuoteNode(Symbol(ustyle_word))
                    lastchar = nextnonwhitespace!(state, lastchar)
                end
                wellformed = lastchar == ')'
            end
            if wellformed
                isempty(state.s) && return nothing, lastchar
                lastchar = last(popfirst!(state.s))
            else
                styerr!(state, "Malformed underline value, should be (<color>, <style>)",
                        -first(something(peek(state.s), (ustart+2, '.'))) + ustart - 1)
            end
            if ismacro(state)
                Expr(:tuple, ucolor, ustyle)
            else
                (ucolor, ustyle.value)
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
        end, lastchar
    end
    function read_inherit!(state, lastchar)
        inherit = if ismacro(state)
            Union{Face, Expr}[]
        else
            Face[]
        end
        if isnextchar(state, '[')
            popfirst!(state.s)
            readvec = true
            while readvec
                skipwhitespace!(state)
                isempty(state.s) && break
                nextchar = last(peek(state.s))
                if nextchar == ':'
                    popfirst!(state.s)
                elseif nextchar == ']'
                    popfirst!(state.s)
                    isempty(state.s) || (lastchar = last(popfirst!(state.s)))
                    break
                end
                facename, lastchar = read_while!(∉((',', ']', ')')), state.s, lastchar)
                push!(inherit, resolveface(state, String(rstrip(facename))))
                if lastchar != ','
                    break
                end
            end
        else
            isnextchar(state, ':') && popfirst!(state.s)
            facename, lastchar = read_while!(∉((',', ']', ')')), state.s, lastchar)
            push!(inherit, resolveface(state, String(rstrip(facename))))
        end
        if ismacro(state)
            Expr(:ref, Face, inherit...)
        else
            inherit
        end, lastchar
    end
    popfirst!(state.s)
    lastchar = '('
    skipwhitespace!(state)
    kwargs = Pair{Symbol, Any}[]
    while !isempty(state.s) && lastchar != ')'
        skipwhitespace!(state)
        keypos = if isempty(state.s) lastindex(state.content) else prevind(state.content, first(peek(state.s))) end
        str_key, lastchar = readalph!(state, lastchar)
        # A trailing comma or whitespace before ')'
        isempty(str_key) && lastchar == ')' && break
        key = Symbol(str_key)
        if lastchar != '='
            skipwhitespace!(state)
            if isempty(state.s) || last(peek(state.s)) != '='
                styerr!(state, "Keyword argument has no value", keypos)
                break
            end
            popfirst!(state.s)
        end
        skipwhitespace!(state)
        isempty(state.s) && break
        key == :fg && (key = :foreground)
        key == :bg && (key = :background)
        val = if ismacro(state) && isnextchar(state, '$')
            expr, _ = readexpr!(state)
            isempty(state.s) || (lastchar = last(popfirst!(state.s)))
            if key ∈ (:foreground, :background) # Look up face names as written ones are
                :($interpattr($(bindexpr!(state, expr)), $(state.out.mod), $(state.strict)))
            elseif key == :inherit
                :($interpface($(bindexpr!(state, expr)), $(state.out.mod), $(state.strict)))
            elseif key == :underline
                :($interpunderline($(bindexpr!(state, expr)), $(state.out.mod), $(state.strict)))
            else
                bindexpr!(state, expr)
            end
        elseif key == :font
            if isnextchar(state, '"')
                readexpr!(state, first(peek(state.s))) |> first
            else
                str, lastchar = read_while!(∉((',', ')')), state.s, lastchar)
                str
            end
        elseif key == :height
            start = first(peek(state.s))
            num = if isnextchar(state, ('.', '0':'9'...)) first(readexpr!(state, start)) end
            rest, lastchar = readsymbol!(state, lastchar) # Empty after a well-formed number
            if isempty(rest) && num isa Real && !isnothing(heightbits(num))
                num
            else
                stop = if isempty(state.s) lastindex(state.content) + 1 else first(peek(state.s)) end
                invalid = rstrip(∈((' ', '\t', '\n', '\r', ',', ')')), state.content[start:prevind(state.content, stop)])
                styerr!(state, AnnotatedString("Invalid height '$invalid', should be a natural number or positive float",
                                                [(17:16+ncodeunits(invalid), FACES.pool[:warning])]),
                        prevind(state.content, start))
            end
        elseif key ∈ (:weight, :slant)
            v, lastchar = readalph!(state, lastchar)
            choices = if key == :weight VALID_WEIGHTS else VALID_SLANTS end
            v ∈ choices || invalidchoice!(str_key, v, choices)
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
            else
                styerr!(state, "Invalid $key value '$flag', should be true or false", -length(flag) - 2)
            end
        elseif key == :underline
            ul, lastchar = read_underline!(state, lastchar)
            ul
        elseif key == :inherit
            inherit, lastchar = read_inherit!(state, lastchar)
            inherit
        elseif key in LIKELY_FUTURE_FACE_ATTRS
            # Keys a later version may add are ignored, not rejected
        else
            styerr!(state, AnnotatedString(
                "Uses unrecognised face key '$key'. Recognised keys are: $(join(FACE_KEYWORDS, ", ", ", and "))",
                [(29:28+ncodeunits(String(key)), FACES.pool[:warning])]),
                    -length(str_key) - 2)
        end
        if any(==(key) ∘ first, kwargs)
            styerr!(state, AnnotatedString("Contains repeated face key '$key'",
                                            [(29:28+ncodeunits(String(key)), FACES.pool[:warning])]),
                    keypos)
        else
            push!(kwargs, key => val)
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
    faceval = if !isempty(state.errors)
        Face() # Already reported; constructing it could throw first
    elseif ismacro(state)
        faceex = Expr(:call, Face, (Expr(:kw, k, v) for (k, v) in kwargs)...)
        if any(isruntime ∘ last, kwargs)
            faceex
        else # As a macro's result would be evaluated, in the caller's module
            Core.eval(state.out.mod, Expr(:var"hygienic-scope", faceex, @__MODULE__))
        end
    else
        Face(; kwargs...)
    end
    addannot!(state, i, faceval)
end

"""
    resolveface(state::State, facename::String)

Determine the face referred to by `facename`, according to `state`.

Returns a `Face`, or when `state` is from a macro invocation an
`Expr` that evaluates to a `Face` may be returned.
"""
function resolveface(state::State, facename::String)
    function nameerror!(message)
        here = if isempty(state.s) lastindex(state.content) else first(peek(state.s)) end
        start = if isempty(facename) # Where the name should be: the terminator just read
            prevind(state.content, here)
        else
            first(something(findprev(facename, state.content, here), here:here))
        end
        styerr!(state, message, prevind(state.content, start))
        Face()
    end
    all(Base.isidentifier, eachsplit(facename, '.')) ||
        return nameerror!(if isempty(facename) "Missing face name" else "Invalid face name '$facename'" end)
    ismacro(state) || return lookmakeface(registrykey(facename))
    face = nameface(state.out.mod, facename, state.strict)
    face isa Exception || return face
    nameerror!(if face isa UnknownFaceError && !isambiguous(state.out.mod, face.name)
        suggestion = similarface(state.out.mod, face.name)
        hint = if isnothing(suggestion) "" else " (did you mean '$suggestion'?)" end
        "Unknown face '$facename'$hint"
    else
        String(chopsuffix(sprint(showerror, face), ".")) # The report adds its own colon
    end)
end

"""
    addannot!(state::State, i::Int, value)

Create a new annotation in `state.out` at position `i`, with `value`.

An identical annotation opened at the same position (as in `{red:{red:x}}`) is
reused instead, provided it is the most recent one: any annotation added since
takes precedence over it.
"""
function addannot!(state::State, i::Int, value)
    region = (i + state.offset):typemax(Int)
    annots = state.out.annots
    styannot = eltype(annots)(if ismacro(state) # As stored, so that one comparison serves every value
        (; region, loff = state.out.dynoff, roff = nothing, value)
    else
        (; region, value)
    end)
    isreused = !isempty(annots) && last(annots) == styannot &&
        (!(styannot.value isa Face) || last(annots).value === styannot.value)
    isreused || push!(annots, styannot)
    push!(state.activestyles[end].inds, lastindex(annots))
end

"""
    read_face_or_keyval!(state::State, i::Int)

Read an inline face or key-value pair from `state` at position `i`, and
add it to `state.out`.
"""
function read_face_or_keyval!(state::State, i::Int)
    function read_curlywrapped!(state)
        popfirst!(state.s)
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
    function readuntil!(stops)
        chars = Char[]
        while (next = peek(state.s)) |> !isnothing && last(next) ∉ stops
            popfirst!(state.s)
            push!(chars, last(next))
        end
        String(chars)
    end
    keystart = first(peek(state.s)) # Called with input left to read
    key = if ismacro(state) && isnextchar(state, '$')
        bindexpr!(state, first(readexpr!(state)))
    else
        readuntil!((',', '=', ':', '}', ' ', '\t', '\n', '\r'))
    end
    skipwhitespace!(state)
    if isempty(state.s)
    elseif last(peek(state.s)) == '='
        key isa String && !Base.isidentifier(key) &&
            styerr!(state, if isempty(key) "Missing key before '='" else "Invalid key '$key'" end,
                    prevind(state.content, keystart), "right here")
        popfirst!(state.s)
        skipwhitespace!(state)
        value = if isempty(state.s)
            "" # An error will be raised later for an incomplete declaration
        elseif ismacro(state) && isnextchar(state, '$')
            bound = bindexpr!(state, first(readexpr!(state)))
            if key == "face"
                :($interpfaceannot($bound, $(state.out.mod), $(state.strict)))
            elseif key == "link"
                :($convert($Link, $bound))
            else
                bound
            end
        elseif isnextchar(state, '{')
            read_curlywrapped!(state)
        else
            readuntil!((',', ':', '}'))
        end
        if key == "face" && value isa String
            return styerr!(state, "A face is written by name, as in {red:…}, or interpolated, as in {face=\$f:…}",
                           -length(value) - 1)
        elseif key == "link" && value isa String
            value = Link(value)
        elseif !ismacro(state)
            state.out.skipothers || isempty(key) ||
                styerr!(state, "Only faces and links are annotations in `styled(…)`; '$key' needs the \
                                `styled\"…\"` macro, or `skipothers = true` to skip it", prevind(state.content, keystart))
            return
        elseif key isa String && key ∉ ("face", "link") # A property, as an untyped annotation
            value = :($(Pair{Symbol, Any})($(QuoteNode(Symbol(key))), $value))
            annotpromote!(state, Pair{Symbol, Any})
        else # Evaluated once: an interpolated face or link, or the value of an interpolated key
            vsym = gensym(:value)
            push!(state.out.lets, :($vsym = $(if key isa String
                                                  value
                                              else
                                                  :($interpkeyvalue($key, $value, $(state.out.mod), $(state.strict)))
                                              end)))
            annotpromote!(state, :(typeof($vsym)))
            value = vsym
        end
        addannot!(state, i, value)
    elseif key !== ""
        face = if !ismacro(state)
            resolveface(state, key)
        elseif haskey(state.out.rfaces, Symbol(key))
            state.out.rfaces[Symbol(key)]
        else
            fval = if key isa Symbol || key isa Expr
                :($interpfaceannot($key, $(state.out.mod), $(state.strict)))
            else
                resolveface(state, key)
            end
            state.out.rfaces[Symbol(key)] = if fval isa Expr # Evaluate a runtime lookup once
                fvar = gensym(:face)
                push!(state.out.lets, :($fvar = $fval))
                fvar
            else
                fval
            end
        end
        addannot!(state, i, face)
    end
end

function promote_type_4u(A::Type, B::Type)
    AB = promote_type(A, B)
    if AB == Any
        AuB = Union{A, B}
        Base.unionlen(AuB) <= 4 && return AuB
    end
    AB
end

"""
    interpface(face, mod::Module, strict::Bool) -> Face

Resolve an interpolated `face` from styled markup.
"""
function interpface end

function interpface(face::Face, ::Module, ::Bool)
    face.f.height ∈ (UNDEF_CUSTOM_HEIGHT_FLAG, UNDEF_INUSE_HEIGHT_FLAG) || return face
    while haskey(FACES.displacements, face)
        face = FACES.displacements[face]
    end
    face
end

function interpface(face::Symbol, mod::Module, strict::Bool)
    # Base.depwarn("Using symbols to refer to faces is deprecated as of v1.14. Use direct names and palettes instead.", Symbol("@styled_str"))
    if strict
        lookupface(mod, face)
    else
        lookmakeface(mod, face)
    end
end

interpface(faces::Union{Vector, Tuple}, mod::Module, strict::Bool) = Face[interpface(face, mod, strict) for face in faces]

# As an annotation, an interpolated vector or tuple of faces stacks them, the first taking priority
interpfaceannot(faces::Union{Vector, Tuple}, mod::Module, strict::Bool) = Face(inherit = interpface(faces, mod, strict))
interpfaceannot(face, mod::Module, strict::Bool) = interpface(face, mod, strict)

interpface(face, ::Module, ::Bool) = throw(ArgumentError("Face interpolation must evaluate to a Symbol or Face, not $(typeof(face))"))

interpattr(value::Union{Symbol, Face}, mod::Module, strict::Bool) = interpface(value, mod, strict)
interpattr(value::AbstractString, mod::Module, strict::Bool) =
    if all(Base.isidentifier, eachsplit(value, '.'))
        interpface(registrykey(value), mod, strict)
    else
        parse(SimpleColor, value)
    end
interpattr((color, style)::Tuple{Any, Symbol}, mod::Module, strict::Bool) = (interpattr(color, mod, strict), style)
interpattr(value, ::Module, ::Bool) = value

function interpkeyvalue(key, value, mod::Module, strict::Bool)
    name = Symbol(key)
    if name === :link convert(Link, value)
    elseif name === :face interpfaceannot(value, mod, strict)
    else Pair{Symbol, Any}(name, value) end
end

interpunderline(value::Symbol, mod::Module, strict::Bool) =
    if value in ATTRIBUTES.underlines value else interpface(value, mod, strict) end
interpunderline(value::AbstractString, ::Module, ::Bool) = value # A hex colour, as `Face` requires
interpunderline(value, mod::Module, strict::Bool) = interpattr(value, mod, strict)

"""
    run_state_machine!(state::State)

Iterate through `state.s`, applying the parsing rules for the top-level of
syntax and calling the relevant specialised functions.

Upon completion, `state.s` should be fully consumed and `state.out` fully
populated (along with `state.errors`).
"""
function run_state_machine!(state::State)
    # Run the state machine
    escape = false
    for (i, char) in state.s
        if escape
            escaped!(state, i, char)
            escape = false
        elseif char == '\\'
            escape = true
        elseif ismacro(state) && char == '$'
            interpolated!(state, i)
        elseif char == '{'
            begin_style!(state, i)
        elseif char == '}'
            end_style!(state, i)
        end
    end
    # Ensure that any trailing content is added
    if state.point <= ncodeunits(state.content) + state.offset
        push!(state.out.strs, String(state.bytes[
            state.point:(ncodeunits(state.content) + state.offset)]))
    end
    for (; source) in state.activestyles
        styerr!(state, AnnotatedString("Incomplete annotation (missing closing '}')",
                                        [(41:41, FACES.pool[:warning])]),
                prevind(state.content, source), "starts here")
    end
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
    annsof(var) = Symbol(var, "_anns") # Distinct suffixes keep derived names apart from each other and fixed ones
    for var in unique(interp.var for interp in interps)
        iavar = annsof(var)
        ninsts = count(interp -> interp.var === var, interps)
        newavtype = Symbol(var, "_avtype")
        annlen = if ninsts == 1
            :(length($iavar))
        else
            :($ninsts * length($iavar))
        end
        append!(state.out.lets,
                (quote
                    local $iavar, $newavtype
                    if $(esc(var)) isa Union{AnnotatedString, SubString{<:AnnotatedString}, AnnotatedChar}
                        $iavar = annotations(if $(esc(var)) isa AnnotatedChar annotatedstring($(esc(var))) else $(esc(var)) end)
                        interp_annot_count = interp_annot_count + $annlen
                        $newavtype = promote_type_4u($(state.out.avtype), fieldtype(eltype($iavar), :value))
                    else
                        $iavar = $(@NamedTuple{region::UnitRange{Int}, value::Face}[])
                        $newavtype = $(state.out.avtype)
                    end
                end).args)
        state.out.avtype = newavtype
    end
    iexprs = Union{Expr, LineNumberNode}[]
    avtype = :(NamedTuple{(:region, :value), Tuple{UnitRange{Int}, $(state.out.avtype)}})
    # Special case: a single interpolation that starts at position 1
    if length(interps) == 1 && first(interps).startpos == 1 && first(interps).annotidx == lastindex(state.out.annots) + 1
        append!(state.out.lets,
                (quote
                     $newavar = convert(Vector{$avtype}, $avar)
                     iszero(interp_annot_count) ||
                         append!($newavar, $(annsof(first(interps).var)))
                 end).args)
    else
        # We now know we need to adjust the annotation regions / deal with multiple annotations
        push!(iexprs, :($newavar = Vector{$avtype}(undef, length($avar) + interp_annot_count)))
        push!(iexprs, :(annot_offset = 0))
        if first(interps).annotidx > 1
            push!(iexprs,
                  :(copyto!($newavar,
                            1,
                            $avar,
                            1,
                            $(first(interps).annotidx - 1))))
        end
        lastannotidx = first(interps).annotidx
        for (; startpos, startoff, annotidx, var) in interps
            annots = annsof(var)
            if lastannotidx != annotidx
                push!(iexprs,
                      :(copyto!($newavar,
                                $lastannotidx + annot_offset,
                                $avar,
                                $lastannotidx,
                                $(annotidx - lastannotidx))))
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
                                   value = anni.value)
                        end
                        annot_offset = annot_offset + length($annots)
                    end))
            lastannotidx = annotidx
        end
        if last(interps).annotidx <= lastindex(state.out.annots)
            push!(iexprs,
                  :(copyto!($newavar,
                            $(last(interps).annotidx) + annot_offset,
                            $avar,
                            $(last(interps).annotidx))))
        end
        push!(state.out.lets,
              Expr(:local, newavar),
              :(if !iszero(interp_annot_count)
                    $(iexprs...)
                else
                    $newavar = convert(Vector{$avtype}, $avar)
                end))
    end
    newavar
end

"""
    @styled_str -> AnnotatedString

Construct a styled string. Within the string, `{<specs>:<content>}` structures
apply the formatting to `<content>`, according to the list of comma-separated
specifications `<specs>`. Each spec can either take the form of a face name,
an inline face specification, or a `key=value` pair. The value must be wrapped
by `{...}` should it contain any of the characters `,=:{}`. A `link=` pair annotates
with a [`Link`](@ref), and any other key with a `key => value` `Pair{Symbol, Any}`.

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
ws = { ' ' | '\\t' | '\\n' | '\\r' } ; (* whitespace *)

face = facename | interpolated ;
facename = name, { '.', name } ;
name = ? identifier ? ;

inlineface = '(', ws, [ faceprop ], { ws, ',', faceprop }, ws, ')' ;
faceprop = [a-z]+, ws, '=', ws, ( [^,)]+ | interpolated) ;

keyvalue = ( 'face', ws, '=', ws, interpolated ) | ( key, ws, '=', ws, value ) ;
key = name | interpolated ;
value = simplevalue | curlybraced | interpolated ;
curlybraced = '{' { escaped | plain } '}' ;
simplevalue = [^\${},:], [^,:}]* ;
```

An extra stipulation not encoded in the above grammar is that `plain` should be
a valid input to [`unescape_string`](@ref), with `specialchar` kept.

The above grammar for `inlineface` is simplified, as the actual implementation
is a bit more sophisticated. The full behaviour is given below.

```ebnf
faceprop = ( 'font', ws, '=', ws, ( ? string ? | [^,)]+ | interpolated ) ) |
           ( 'height', ws, '=', ws, ( ? positive number ? | interpolated ) ) |
           ( 'weight', ws, '=', ws, ( weight | interpolated ) ) |
           ( 'slant', ws, '=', ws, ( slant | interpolated ) ) |
           ( ( 'foreground' | 'fg' | 'background' | 'bg' ),
               ws, '=', ws, ( simplecolor | interpolated ) ) |
           ( 'underline', ws, '=', ws, ( underline | interpolated ) ) |
           ( 'strikethrough', ws, '=', ws, ( bool | interpolated ) ) |
           ( 'inverse', ws, '=', ws, ( bool | interpolated ) ) |
           ( 'inherit', ws, '=', ws, ( inherit | interpolated ) ) ;

nothing = 'nothing' ;
bool = 'true' | 'false' ;
weight = 'thin' | 'extralight' | 'light' | 'semilight' | 'normal'
       | 'medium' | 'semibold' | 'bold' | 'extrabold' | 'black' ;
slant = 'italic' | 'oblique' | 'normal' ;
hexcolor = ('#' | '0x'), [0-9A-Fa-f]{6} ;
simplecolor = hexcolor | facename | nothing ;

underline = nothing | bool | underlinestyle | simplecolor | underlinestyled ;
underlinestyle = 'straight' | 'double' | 'curly' | 'dotted' | 'dashed' ;
underlinestyled = '(', ws, ('' | nothing | simplecolor | interpolated), ws,
                  ',', ws, ( underlinestyle | interpolated ), ws ')' ;

inherit = ( '[', inheritval, { ',', inheritval }, ']' ) | inheritval ;
inheritval = ws, [ ':' ], facename, ws ;
```
"""
macro styled_str(raw_content::String)
    # First undo the transforms of the parser, (assuming the input was entered
    # with single `styled""` markers so this transform is unambiguously
    # reversible and not as `@styled_str "."` or `styled"""."""`), since the
    # `unescape_string` transforms will be a superset of those transforms
    content = unescape_string(Base.escape_raw_string(raw_content),
                              ('\\', '{', '}', '$', '\n', '\r'))
    state = State(content, __module__)
    run_state_machine!(state)
    isempty(state.errors) || throw(MalformedStylingMacro(state.content, state.errors))
    astr = if length(state.out.strs) == 1
        first(state.out.strs)
    else
        :(string($(state.out.strs...)))
    end
    annots = map(state.out.annots) do (; region, value, loff, roff)
        start, stop = first(region), last(region)
        if !isnothing(loff)
            start = :($start + $loff)
        end
        if !isnothing(roff)
            stop = :($stop + $roff)
        end
        Expr(:tuple, Expr(:call, :(:), start, stop), value) # Converted to `atype` as the vector is built
    end
    atype = :(NamedTuple{(:region, :value), Tuple{UnitRange{Int}, $(state.out.avtype)}})
    avec = :($atype[$(annots...)])
    aexp = :($AnnotatedString($astr, $avec))
    if !isempty(state.out.interpolations)
        push!(state.out.lets, :(annots = $avec))
        newavar = spliceinterps!(state, :annots)
        aexp = :($AnnotatedString($astr, $newavar))
    end
    # One destructuring binding, as only the first of several is evaluated in the caller's scope,
    # with each expression in a block, as an assignment in a tuple would make it a named tuple
    binding = if isempty(state.out.binds)
        Expr(:block)
    else
        Expr(:(=), Expr(:tuple, first.(state.out.binds)...), Expr(:tuple, (Expr(:block, last(b)) for b in state.out.binds)...))
    end
    Expr(:let, binding, Expr(:block, state.out.lets..., aexp))
end

"""
    styled(content::AbstractString; skipothers::Bool = false) -> AnnotatedString{String, Union{Face, Link}}

Construct a styled string. Within the string, `{<specs>:<content>}` structures
apply the formatting to `<content>`, according to the list of comma-separated
specifications `<specs>`. Each spec can either take the form of a face name,
an inline face specification, or a `key=value` pair. The value must be wrapped
by `{...}` should it contain any of the characters `,=:{}`.

This is a functional equivalent of the [`@styled_str`](@ref) macro, just without
interpolation capabilities. Its annotations are only faces and links (`link=`), so that
its result has a single type, whatever the content. Any other `key=value` pair is an
error, or is skipped when `skipothers` is set.

Face names are looked up in the global registry, as there is no module to find
palettes in. A dotted name is an absolute module path: `Mod.face` is the face
`face` of the palette of the top-level module `Mod`.
"""
function styled(content::AbstractString; skipothers::Bool = false)
    state = State(content; skipothers)
    run_state_machine!(state)
    if !isempty(state.errors)
        throw(MalformedStylingMacro(state.content, state.errors))
    else
        AnnotatedString(join(state.out.strs), state.out.annots)
    end
end

struct MalformedStylingMacro <: Exception
    raw::String
    problems::Vector{NamedTuple{(:message, :position, :hint), Tuple{AnnotatedString{String, StyledValue}, Int, String}}}
end

function Base.showerror(io::IO, err::MalformedStylingMacro)
    # Not styled as an error, as it is already prefixed by "ERROR: LoadError"
    println(io, "MalformedStylingMacro")
    for (; message, position, hint) in err.problems
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
        npre = textwidth(Base.escape_string(err.raw[start:position]))
        println(io, styled"{error:│} $message:\n\
                       {error:│}  {bright_green:\"{shadow:$begin_ellipsis}$window{shadow:$end_ellipsis}\"}\n\
                       {error:│}  $(' '^(1+textwidth(begin_ellipsis)+npre)){info:╰─╴$hint}")
    end
    pluralissues = length(err.problems) > 1
    println(io, styled"{error:┕} {light,italic:$(length(err.problems)) $(ifelse(pluralissues, \"issues\", \"issue\"))}")
end

end
