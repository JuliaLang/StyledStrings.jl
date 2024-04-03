import Base.Unicode: textwidth, lowercase, uppercase, titlecase,
    lowercase, titlecase, uppercasefirst, lowercasefirst

textwidth(s::AnnotatedString) = textwidth(s.string)

lowercase(c::AnnotatedChar) = AnnotatedChar(lowercase(c.char), annotations(c))
uppercase(c::AnnotatedChar) = AnnotatedChar(uppercase(c.char), annotations(c))
titlecase(c::AnnotatedChar) = AnnotatedChar(titlecase(c.char), annotations(c))

uppercase(s::AnnotatedString) = annotated_chartransform(uppercase, s)
lowercase(s::AnnotatedString) = annotated_chartransform(lowercase, s)

# TODO: improve performance characteristics, room for a ~10x improvement.
function titlecase(s::AnnotatedString; wordsep::Function = !isletter, strict::Bool=true)
    initial_state = (; startword = true, state = Ref{Int32}(0),
             c0 = eltype(s)(zero(UInt32)), wordsep = wordsep, strict = strict)
    annotated_chartransform(s, initial_state) do c, state
        if isgraphemebreak!(state.state, state.c0, c) && state.wordsep(c)
            state = Base.setindex(state, true, :startword)
            cnew = c
        else
            cnew = state.startword ? titlecase(c) : state.strict ? lowercase(c) : c
            state = Base.setindex(state, false, :startword)
        end
        state = Base.setindex(state, c, :c0)
        cnew, state
    end
end

# TODO: improve performance characteristics, room for a ~5x improvement.
function uppercasefirst(s::AnnotatedString)
    annotated_chartransform(s, true) do c, state
        if state
            (titlecase(c), false)
        else
            (c, state)
        end
    end
end

# TODO: improve performance characteristics, room for a ~5x improvement.
function lowercasefirst(s::AnnotatedString)
    annotated_chartransform(s, true) do c, state
        if state
            (lowercase(c), false)
        else
            (c, state)
        end
    end
end
