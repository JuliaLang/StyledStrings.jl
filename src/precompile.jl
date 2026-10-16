# In a function, so that the calls are inferred together with known types
function workload()
    colorio = IOContext(IOBuffer(), :color => true)

    print(colorio, styled("{red:a} {(fg=blue):b} {link={https://x}:c}"))
    Base.escape_string(colorio, styled"just a string")
    print(colorio, styled"{face=$(Face(foreground=face\"red\")):with color}")
    show(colorio, MIME("text/plain"), styled"{red:with color}")
    print(colorio, styled"{red:with color}"[1])
    print(colorio, styled"{(underline=(blue,curly)):more styling}")
    print(colorio, styled"{(fg=#102030):rgb colour}")
    show(colorio, MIME("text/html"), styled"{link={https://julialang.org}:a link} with {red:color}")
    print(colorio, SubString(styled"{red:with color}", 1, 4))
    # Strings that hold any values display through code compiled once, as a logged message does
    anyvalued = Base.AnnotatedString{String, Any}(styled"{red:with color} and {link={https://julialang.org}:a link}")
    print(colorio, anyvalued)
    show(colorio, MIME("text/html"), anyvalued)
    Base.CoreLogging.with_logger(Base.CoreLogging.ConsoleLogger(colorio)) do
        @info styled"{red:with color}"
    end

    StyledStrings.Face()
    StyledStrings.Face(height=2)
    merge(StyledStrings.Face(inherit=face"blue"), StyledStrings.Face(foreground=face"white"))
    StyledStrings.Face(height=2) == StyledStrings.Face(height=3)

    show(colorio, MIME("text/plain"), StyledStrings.Face(foreground=face"green"))
    show(colorio, StyledStrings.Face(foreground=face"green"))
    show(colorio, StyledStrings.SimpleColor(face"green"))

    StyledStrings.getface()
    StyledStrings.getface(face"red")
    StyledStrings.getface(styled"{red:red}", 1)

    StyledStrings.withfaces(face"yellow" => StyledStrings.Face(foreground=face"red"), face"green" => face"blue") do
        println(colorio, styled"{yellow:red} and {green:blue} mixed make {magenta:purple}")
    end
end
workload()
