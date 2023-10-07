# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test, StyledStrings

@testset "SimpleColor" begin
    @test StyledStrings.SimpleColor(:hey).value == :hey # no error
    @test StyledStrings.SimpleColor(0x01, 0x02, 0x03).value == (r=0x01, g=0x02, b=0x03)
    @test StyledStrings.SimpleColor((r=0x01, g=0x02, b=0x03)).value == (r=0x01, g=0x02, b=0x03)
    @test StyledStrings.SimpleColor(0x010203).value == (r=0x01, g=0x02, b=0x03)
    @test tryparse(StyledStrings.SimpleColor, "hey") == StyledStrings.SimpleColor(:hey)
    @test tryparse(StyledStrings.SimpleColor, "#010203") == StyledStrings.SimpleColor(0x010203)
    @test tryparse(StyledStrings.SimpleColor, "#12345g") === nothing
    @test tryparse(StyledStrings.SimpleColor, "!not a color") === nothing
    @test_throws ArgumentError parse(StyledStrings.SimpleColor, "!not a color")
end

@testset "Faces" begin
    # Construction
    @test StyledStrings.Face() ==
        StyledStrings.Face(nothing, nothing, nothing, nothing, nothing,
             nothing, nothing, nothing, nothing, Symbol[])
    @test StyledStrings.Face(font="font") ==
        StyledStrings.Face("font", nothing, nothing, nothing, nothing,
             nothing, nothing, nothing, nothing, Symbol[])
    @test StyledStrings.Face(height=1) ==
        StyledStrings.Face(nothing, 1, nothing, nothing, nothing,
             nothing, nothing, nothing, nothing, Symbol[])
    @test StyledStrings.Face(weight=:bold) ==
        StyledStrings.Face(nothing, nothing, :bold, nothing, nothing,
             nothing, nothing, nothing, nothing, Symbol[])
    @test StyledStrings.Face(slant=:italic) ==
        StyledStrings.Face(nothing, nothing, nothing, :italic, nothing,
             nothing, nothing, nothing, nothing, Symbol[])
    @test StyledStrings.Face(foreground=StyledStrings.SimpleColor(:red)) ==
        StyledStrings.Face(nothing, nothing, nothing, nothing, StyledStrings.SimpleColor(:red),
             nothing, nothing, nothing, nothing, Symbol[])
    @test StyledStrings.Face(foreground=:red) ==
        StyledStrings.Face(nothing, nothing, nothing, nothing, StyledStrings.SimpleColor(:red),
             nothing, nothing, nothing, nothing, Symbol[])
    @test StyledStrings.Face(foreground=0xff0000) ==
        StyledStrings.Face(nothing, nothing, nothing, nothing, StyledStrings.SimpleColor(0xff0000),
             nothing, nothing, nothing, nothing, Symbol[])
    @test StyledStrings.Face(background=StyledStrings.SimpleColor(:red)) ==
        StyledStrings.Face(nothing, nothing, nothing, nothing, nothing,
             StyledStrings.SimpleColor(:red), nothing, nothing, nothing, Symbol[])
    @test StyledStrings.Face(background=:red) ==
        StyledStrings.Face(nothing, nothing, nothing, nothing, nothing,
             StyledStrings.SimpleColor(:red), nothing, nothing, nothing, Symbol[])
    @test StyledStrings.Face(background=0xff0000) ==
        StyledStrings.Face(nothing, nothing, nothing, nothing, nothing,
             StyledStrings.SimpleColor(0xff0000), nothing, nothing, nothing, Symbol[])
    @test StyledStrings.Face(underline=true) ==
        StyledStrings.Face(nothing, nothing, nothing, nothing, nothing,
             nothing, true, nothing, nothing, Symbol[])
    @test StyledStrings.Face(underline=:red) ==
        StyledStrings.Face(nothing, nothing, nothing, nothing, nothing,
             nothing, StyledStrings.SimpleColor(:red), nothing, nothing, Symbol[])
    @test StyledStrings.Face(underline=(nothing, :curly)) ==
        StyledStrings.Face(nothing, nothing, nothing, nothing, nothing,
             nothing, (nothing, :curly), nothing, nothing, Symbol[])
    @test StyledStrings.Face(underline=(:red, :curly)) ==
        StyledStrings.Face(nothing, nothing, nothing, nothing, nothing,
             nothing, (StyledStrings.SimpleColor(:red), :curly), nothing, nothing, Symbol[])
    @test StyledStrings.Face(strikethrough=true) ==
        StyledStrings.Face(nothing, nothing, nothing, nothing, nothing,
             nothing, nothing, true, nothing, Symbol[])
    @test StyledStrings.Face(inverse=true) ==
        StyledStrings.Face(nothing, nothing, nothing, nothing, nothing,
             nothing, nothing, nothing, true, Symbol[])
    @test StyledStrings.Face(inherit=:singleface) ==
        StyledStrings.Face(nothing, nothing, nothing, nothing, nothing,
             nothing, nothing, nothing, nothing, [:singleface])
    @test StyledStrings.Face(inherit=[:many, :faces]) ==
        StyledStrings.Face(nothing, nothing, nothing, nothing, nothing,
             nothing, nothing, nothing, nothing, [:many, :faces])
    @test StyledStrings.Face() == StyledStrings.Face()
    @test StyledStrings.Face(height=1) == StyledStrings.Face(height=1)
    @test StyledStrings.Face(height=1) != StyledStrings.Face(height=2)
    @test StyledStrings.Face(inherit=:a) != StyledStrings.Face(inherit=:b)
    # Adding a face then resetting
    @test StyledStrings.loadface!(:testface => StyledStrings.Face(font="test")) == StyledStrings.Face(font="test")
    @test get(StyledStrings.FACES.current[], :testface, nothing) == StyledStrings.Face(font="test")
    @test StyledStrings.loadface!(:bold => StyledStrings.Face(weight=:extrabold)) == StyledStrings.Face(weight=:extrabold)
    @test get(StyledStrings.FACES.current[], :bold, nothing) == StyledStrings.Face(weight=:extrabold)
    @test StyledStrings.loadface!(:testface => StyledStrings.Face(height=2.0)) == StyledStrings.Face(font="test", height=2.0)
    @test get(StyledStrings.FACES.current[], :testface, nothing) == StyledStrings.Face(font="test", height=2.0)
    # Loading from TOML (a Dict)
    @test StyledStrings.loaduserfaces!(Dict{String, Any}("anotherface" =>
        Dict{String, Any}("font" => "afont",
                          "height" => 123,
                          "weight" => "semibold",
                          "slant" => "oblique",
                          "foreground" => "green",
                          "background" => "magenta",
                          "underline" => ["blue", "curly"],
                          "strikethrough" => true,
                          "inverse" => true,
                          "inherit" => ["iface"]))) isa Any
    @test get(StyledStrings.FACES.current[], :anotherface, nothing) ==
        StyledStrings.Face(font = "afont", height = 123, weight = :semibold,
             slant = :oblique, foreground = :green, background = :magenta,
             underline = (:blue, :curly), strikethrough = true,
             inverse = true, inherit = [:iface])
    StyledStrings.resetfaces!()
    @test get(StyledStrings.FACES.current[], :bold, nothing) == StyledStrings.Face(weight=:bold)
    @test haskey(StyledStrings.FACES.current[], :testface) == false
    @test haskey(StyledStrings.FACES.current[], :anotherface) == false
    # `withfaces`
    @test StyledStrings.withfaces(() -> get(StyledStrings.FACES.current[], :testface, nothing),
                    :testface => StyledStrings.Face(font="test")) == StyledStrings.Face(font="test")
    @test haskey(StyledStrings.FACES.current[], :testface) == false
    # Basic merging
    let f1 = StyledStrings.Face(height=140, weight=:bold, inherit=[:a])
        f2 = StyledStrings.Face(height=1.5, weight=:light, inherit=[:b])
        f3 = StyledStrings.Face(height=1.2, slant=:italic)
        @test merge(f1, f2, f3) == StyledStrings.Face(height=252, weight=:light, slant=:italic)
        @test merge(f3, f2, f1) == StyledStrings.Face(height=140, weight=:bold, slant=:italic, inherit=[:a])
        @test merge(f3, f1) == StyledStrings.Face(height=140, weight=:bold, slant=:italic, inherit=[:a])
        @test merge(f3, f2) == StyledStrings.Face(height=1.5*1.2, weight=:light, slant=:italic, inherit=[:b])
    end
    # Merging, inheritence, and canonicalisation
    let aface = StyledStrings.Face(font="a", height=1.2)
        bface = StyledStrings.Face(font="b", height=1.1, weight=:light, inherit=:a)
        cface = StyledStrings.Face(font="c", foreground=:red, inherit=:b)
        dface = StyledStrings.Face(font="d", foreground=:blue, weight=:bold)
        eface = StyledStrings.Face(font="e", inherit = [:c, :d])
        fface = StyledStrings.Face(font="f", inherit = [:d, :c])
        StyledStrings.loadface!(:a => aface)
        StyledStrings.loadface!(:b => bface)
        StyledStrings.loadface!(:c => cface)
        StyledStrings.loadface!(:d => dface)
        StyledStrings.loadface!(:e => eface)
        StyledStrings.loadface!(:f => fface)
        @test StyledStrings.getface(:c) == merge(StyledStrings.FACES.current[][:default], aface, bface, cface, StyledStrings.Face())
        @test StyledStrings.getface(:b) == merge(StyledStrings.FACES.current[][:default], aface, bface, StyledStrings.Face())
        @test StyledStrings.getface(:a) == merge(StyledStrings.FACES.current[][:default], aface, StyledStrings.Face())
        @test StyledStrings.getface([:c]) == StyledStrings.getface(:c)
        @test StyledStrings.getface(bface) == StyledStrings.getface(:b)
        @test StyledStrings.getface(cface) == StyledStrings.getface(:c)
        @test StyledStrings.getface([:c, :d]).foreground.value == :blue
        @test StyledStrings.getface([[:c, :d]]).foreground.value == :red
        @test StyledStrings.getface(:e).foreground.value == :red
        @test StyledStrings.getface([:d, :c]).foreground.value == :red
        @test StyledStrings.getface([[:d, :c]]).foreground.value == :blue
        @test StyledStrings.getface(:f).foreground.value == :blue
        StyledStrings.resetfaces!()
    end
end

@testset "Styled string macro" begin
    # Preservation of an unstyled string
    @test styled"some string" == Base.TaggedString("some string")
    # Basic styled constructs
    @test styled"{thing=val:some} string" == Base.TaggedString("some string", [(1:4, :thing => "val")])
    @test styled"some {thing=val:string}" == Base.TaggedString("some string", [(6:11, :thing => "val")])
    @test styled"some {a=1:s}trin{b=2:g}" == Base.TaggedString("some string", [(6:6, :a => "1"), (11:11, :b => "2")])
    @test styled"{thing=val with spaces:some} string" == Base.TaggedString("some string", [(1:4, :thing => "val with spaces")])
    @test styled"{aface:some} string" == Base.TaggedString("some string", [(1:4, :face => :aface)])
    @test styled"{aface,bface:some} string" ==
        Base.TaggedString("some string", [(1:4, :face => :aface), (1:4, :face => :bface)])
    # Inline face attributes
    @test styled"{(slant=italic):some} string" ==
        Base.TaggedString("some string", [(1:4, :face => StyledStrings.Face(slant=:italic))])
    @test styled"{(foreground=magenta,background=#555555):some} string" ==
        Base.TaggedString("some string", [(1:4, :face => StyledStrings.Face(foreground=:magenta, background=0x555555))])
    # Curly bracket escaping
    @test styled"some \{string" == Base.TaggedString("some {string")
    @test styled"some string\}" == Base.TaggedString("some string}")
    @test styled"some \{string\}" == Base.TaggedString("some {string}")
    @test styled"some \{str:ing\}" == Base.TaggedString("some {str:ing}")
    @test styled"some \{{bold:string}\}" == Base.TaggedString("some {string}", [(7:12, :face => :bold)])
    @test styled"some {bold:string \{other\}}" == Base.TaggedString("some string {other}", [(6:19, :face => :bold)])
    # Nesting
    @test styled"{bold:nest{italic:ed st{red:yling}}}" ==
        Base.TaggedString("nested styling", [(1:14, :face => :bold), (5:14, :face => :italic), (10:14, :face => :red)])
    # Production of a `(Base.TaggedString)` value instead of an expression when possible
    @test Base.TaggedString("val") == @macroexpand styled"val"
    @test Base.TaggedString("val", [(1:3, :face => :style)]) == @macroexpand styled"{style:val}"
    # Interpolation
    @test :($(Base.taggedstring)(val)) == @macroexpand styled"$val"
    @test :($(Base.taggedstring)("a", val)) == @macroexpand styled"a$val"
    @test :($(Base.taggedstring)("a", val, "b")) == @macroexpand styled"a$(val)b"
    # @test :($(Base.taggedstring)($(Base.TaggedString)(string(val), $(Pair{Symbol, Any}(:face, :style))))) ==
    #     @macroexpand styled"{style:$val}"
    @test :($(Base.taggedstring)($(Base.TaggedString)("val", [($(1:3), Pair{Symbol, Any}(:face, face))]))) ==
        @macroexpand styled"{$face:val}"
    @test :($(Base.taggedstring)($(Base.TaggedString)("val", [($(1:3), Pair{Symbol, Any}(key, "val"))]))) ==
        @macroexpand styled"{$key=val:val}"
    @test :($(Base.taggedstring)($(Base.TaggedString)("val", [($(1:3), Pair{Symbol, Any}(key, val))]))) ==
        @macroexpand styled"{$key=$val:val}"
    # @test :($(Base.taggedstring)($(Base.TaggedString)(string(val), Pair{Symbol, Any}(key, val)))) ==
    #     @macroexpand styled"{$key=$val:$val}"
    @test :($(Base.taggedstring)($(Base.TaggedString)("val", [($(1:3), Pair{Symbol, Any}(:face, $(StyledStrings.Face)(foreground = color)))]))) ==
        @macroexpand styled"{(foreground=$color):val}"
end
