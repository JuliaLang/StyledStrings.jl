colorio = IOContext(IOBuffer(), :color => true)

print(colorio, styled"just a string")
Base.escape_string(colorio, styled"just a string")
print(colorio, styled"{red:with color}")
print(colorio, styled"{face=$(Face(foreground=face\"red\")):with color}")
print(colorio, styled"{(foreground=red):with color}")
show(colorio, MIME("text/plain"), styled"{red:with color}")
show(colorio, MIME("text/html"), styled"{red:with color}")
print(colorio, styled"{red:with color}"[1])
print(colorio, styled"{(underline=(blue,curly)):more styling}")

convert(StyledStrings.SimpleColor, (r = 0x01, g = 0x02, b = 0x03))
convert(StyledStrings.SimpleColor, 0x010203)
convert(StyledStrings.SimpleColor, face"blue")
tryparse(StyledStrings.SimpleColor, "blue")
parse(StyledStrings.SimpleColor, "#010203")

StyledStrings.Face()
StyledStrings.Face(height=2)
merge(StyledStrings.Face(inherit=face"blue"), StyledStrings.Face(foreground=face"white"))
StyledStrings.Face(height=2) == StyledStrings.Face(height=3)

show(colorio, MIME("text/plain"), StyledStrings.Face(foreground=face"green"))
show(colorio, StyledStrings.Face(foreground=face"green"))

StyledStrings.getface()
StyledStrings.getface(face"red")
StyledStrings.getface(styled"{red:red}", 1)

# StyledStrings.addface!(:_precompile => Face(font="precompile"))
# StyledStrings.loadface!(:_precompile => Face(inverse=true))
StyledStrings.loaduserfaces!(Dict{String, Any}("_precompile" =>
    Dict{String, Any}("strikethough" => true)))
# StyledStrings.resetfaces!(:_precompile)
# StyledStrings.resetfaces!()

StyledStrings.withfaces(face"yellow" => StyledStrings.Face(foreground=face"red"), face"green" => face"blue") do
    println(colorio, styled"{yellow:red} and {green:blue} mixed make {magenta:purple}")
end

StyledStrings.HAVE_LOADED_CUSTOMISATIONS[] = false
