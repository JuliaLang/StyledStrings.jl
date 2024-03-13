# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test, StyledStrings

import StyledStrings: SimpleColor, Face, styled

@testset "SimpleColor" begin
    @test SimpleColor(:hey).value == :hey # no error
    @test SimpleColor(0x01, 0x02, 0x03).value == (r=0x01, g=0x02, b=0x03)
    @test SimpleColor((r=0x01, g=0x02, b=0x03)).value == (r=0x01, g=0x02, b=0x03)
    @test SimpleColor(0x010203).value == (r=0x01, g=0x02, b=0x03)
    @test tryparse(SimpleColor, "hey") == SimpleColor(:hey)
    @test tryparse(SimpleColor, "#010203") == SimpleColor(0x010203)
    @test tryparse(SimpleColor, "#12345g") === nothing
    @test tryparse(SimpleColor, "!not a color") === nothing
    @test_throws ArgumentError parse(SimpleColor, "!not a color")
end

@testset "Faces" begin
    # Construction
    @test Face() ==
        Face(nothing, nothing, nothing, nothing, nothing,
             nothing, nothing, nothing, nothing, Symbol[])
    @test Face(font="font") ==
        Face("font", nothing, nothing, nothing, nothing,
             nothing, nothing, nothing, nothing, Symbol[])
    @test Face(height=1) ==
        Face(nothing, 1, nothing, nothing, nothing,
             nothing, nothing, nothing, nothing, Symbol[])
    @test Face(weight=:bold) ==
        Face(nothing, nothing, :bold, nothing, nothing,
             nothing, nothing, nothing, nothing, Symbol[])
    @test Face(slant=:italic) ==
        Face(nothing, nothing, nothing, :italic, nothing,
             nothing, nothing, nothing, nothing, Symbol[])
    @test Face(foreground=SimpleColor(:red)) ==
        Face(nothing, nothing, nothing, nothing, SimpleColor(:red),
             nothing, nothing, nothing, nothing, Symbol[])
    @test Face(foreground=:red) ==
        Face(nothing, nothing, nothing, nothing, SimpleColor(:red),
             nothing, nothing, nothing, nothing, Symbol[])
    @test Face(foreground=0xff0000) ==
        Face(nothing, nothing, nothing, nothing, SimpleColor(0xff0000),
             nothing, nothing, nothing, nothing, Symbol[])
    @test Face(background=SimpleColor(:red)) ==
        Face(nothing, nothing, nothing, nothing, nothing,
             SimpleColor(:red), nothing, nothing, nothing, Symbol[])
    @test Face(background=:red) ==
        Face(nothing, nothing, nothing, nothing, nothing,
             SimpleColor(:red), nothing, nothing, nothing, Symbol[])
    @test Face(background=0xff0000) ==
        Face(nothing, nothing, nothing, nothing, nothing,
             SimpleColor(0xff0000), nothing, nothing, nothing, Symbol[])
    @test Face(underline=true) ==
        Face(nothing, nothing, nothing, nothing, nothing,
             nothing, true, nothing, nothing, Symbol[])
    @test Face(underline=:red) ==
        Face(nothing, nothing, nothing, nothing, nothing,
             nothing, SimpleColor(:red), nothing, nothing, Symbol[])
    @test Face(underline=(nothing, :curly)) ==
        Face(nothing, nothing, nothing, nothing, nothing,
             nothing, (nothing, :curly), nothing, nothing, Symbol[])
    @test Face(underline=(:red, :curly)) ==
        Face(nothing, nothing, nothing, nothing, nothing,
             nothing, (SimpleColor(:red), :curly), nothing, nothing, Symbol[])
    @test Face(strikethrough=true) ==
        Face(nothing, nothing, nothing, nothing, nothing,
             nothing, nothing, true, nothing, Symbol[])
    @test Face(inverse=true) ==
        Face(nothing, nothing, nothing, nothing, nothing,
             nothing, nothing, nothing, true, Symbol[])
    @test Face(inherit=:singleface) ==
        Face(nothing, nothing, nothing, nothing, nothing,
             nothing, nothing, nothing, nothing, [:singleface])
    @test Face(inherit=[:many, :faces]) ==
        Face(nothing, nothing, nothing, nothing, nothing,
             nothing, nothing, nothing, nothing, [:many, :faces])
    @test Face() == Face()
    @test Face(height=1) == Face(height=1)
    @test Face(height=1) != Face(height=2)
    @test Face(inherit=:a) != Face(inherit=:b)
    # Adding a face then resetting
    @test StyledStrings.loadface!(:testface => Face(font="test")) == Face(font="test")
    @test get(StyledStrings.FACES.current[], :testface, nothing) == Face(font="test")
    @test StyledStrings.loadface!(:bold => Face(weight=:extrabold)) == Face(weight=:extrabold)
    @test get(StyledStrings.FACES.current[], :bold, nothing) == Face(weight=:extrabold)
    @test StyledStrings.loadface!(:testface => Face(height=2.0)) == Face(font="test", height=2.0)
    @test get(StyledStrings.FACES.current[], :testface, nothing) == Face(font="test", height=2.0)
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
        Face(font = "afont", height = 123, weight = :semibold,
             slant = :oblique, foreground = :green, background = :magenta,
             underline = (:blue, :curly), strikethrough = true,
             inverse = true, inherit = [:iface])
    StyledStrings.resetfaces!()
    @test get(StyledStrings.FACES.current[], :bold, nothing) == Face(weight=:bold)
    @test haskey(StyledStrings.FACES.current[], :testface) == false
    @test haskey(StyledStrings.FACES.current[], :anotherface) == false
    # `withfaces`
    @test StyledStrings.withfaces(() -> get(StyledStrings.FACES.current[], :testface, nothing),
                    :testface => Face(font="test")) == Face(font="test")
    @test haskey(StyledStrings.FACES.current[], :testface) == false
    # Basic merging
    let f1 = Face(height=140, weight=:bold, inherit=[:a])
        f2 = Face(height=1.5, weight=:light, inherit=[:b])
        f3 = Face(height=1.2, slant=:italic)
        @test merge(f1, f2, f3) == Face(height=252, weight=:light, slant=:italic, inherit=[:a]) #\ @test merge(f2, f3) == Face(height=210, weight=:light, slant=:italic, inherit=[:b])
        @test merge(f3, f2, f1) == Face(height=140, weight=:bold, slant=:italic)
        @test merge(f3, f1) == Face(height=140, weight=:bold, slant=:italic)
        @test merge(f3, f2) == Face(height=1.5*1.2, weight=:light, slant=:italic)
    end
    # Merging, inheritence, and canonicalisation
    let aface = Face(font="a", height=1.2)
        bface = Face(font="b", height=1.1, weight=:light, inherit=:a)
        cface = Face(font="c", foreground=:red, inherit=:b)
        dface = Face(font="d", foreground=:blue, weight=:bold)
        eface = Face(font="e", inherit = [:c, :d])
        fface = Face(font="f", inherit = [:d, :c])
        StyledStrings.loadface!(:a => aface)
        StyledStrings.loadface!(:b => bface)
        StyledStrings.loadface!(:c => cface)
        StyledStrings.loadface!(:d => dface)
        StyledStrings.loadface!(:e => eface)
        StyledStrings.loadface!(:f => fface)
        @test StyledStrings.getface(:c) == merge(StyledStrings.FACES.current[][:default], aface, bface, Face(height=120), cface)
        @test StyledStrings.getface(:b) == merge(StyledStrings.FACES.current[][:default], aface, Face(height=120), bface)
        @test StyledStrings.getface(:a) == merge(StyledStrings.FACES.current[][:default], aface)
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

@testset "Styled Markup" begin
    # Preservation of an unstyled string
    @test styled"some string" == Base.AnnotatedString("some string")
    # Basic styled constructs
    @test styled"{thing=val:some} string" == Base.AnnotatedString("some string", [(1:4, :thing => "val")])
    @test styled"some {thing=val:string}" == Base.AnnotatedString("some string", [(6:11, :thing => "val")])
    @test styled"some {a=1:s}trin{b=2:g}" == Base.AnnotatedString("some string", [(6:6, :a => "1"), (11:11, :b => "2")])
    @test styled"{thing=val with spaces:some} string" == Base.AnnotatedString("some string", [(1:4, :thing => "val with spaces")])
    @test styled"{aface:some} string" == Base.AnnotatedString("some string", [(1:4, :face => :aface)])
    @test styled"{aface,bface:some} string" ==
        Base.AnnotatedString("some string", [(1:4, :face => :aface), (1:4, :face => :bface)])
    # Inline face attributes
    @test styled"{(slant=italic):some} string" ==
        Base.AnnotatedString("some string", [(1:4, :face => Face(slant=:italic))])
    @test styled"{(foreground=magenta,background=#555555):some} string" ==
        Base.AnnotatedString("some string", [(1:4, :face => Face(foreground=:magenta, background=0x555555))])
    # Inline face attributes: empty attribute lists are legal
    @test styled"{():}" == styled"{( ):}" == Base.AnnotatedString("")
    # Inline face attributes: leading/trailing whitespace
    @test styled"{ ( fg=red ) :a}" ==  Base.AnnotatedString("a", [(1:1, :face => Face(foreground=:red))])
    # Curly bracket escaping
    @test styled"some \{string" == Base.AnnotatedString("some {string")
    @test styled"some string\}" == Base.AnnotatedString("some string}")
    @test styled"some \{string\}" == Base.AnnotatedString("some {string}")
    @test styled"some \{str:ing\}" == Base.AnnotatedString("some {str:ing}")
    @test styled"some \{{bold:string}\}" == Base.AnnotatedString("some {string}", [(7:12, :face => :bold)])
    @test styled"some {bold:string \{other\}}" == Base.AnnotatedString("some string {other}", [(6:19, :face => :bold)])
    # Nesting
    @test styled"{bold:nest{italic:ed st{red:yling}}}" ==
        Base.AnnotatedString(
            "nested styling", [(1:14, :face => :bold), (5:14, :face => :italic), (10:14, :face => :red)])
    # Production of a `(Base.AnnotatedString)` value instead of an expression when possible
    @test Base.AnnotatedString("val") == @macroexpand styled"val"
    @test Base.AnnotatedString("val", [(1:3, :face => :style)]) == @macroexpand styled"{style:val}"
    # Interpolation
    let annotatedstring = GlobalRef(StyledStrings.StyledMarkup, :annotatedstring)
        AnnotatedString = GlobalRef(StyledStrings.StyledMarkup, :AnnotatedString)
        Pair            = GlobalRef(StyledStrings.StyledMarkup, :Pair)
        Symbol          = GlobalRef(StyledStrings.StyledMarkup, :Symbol)
        Any             = GlobalRef(StyledStrings.StyledMarkup, :Any)
        @test :($annotatedstring(val)) == @macroexpand styled"$val"
        @test :($annotatedstring("a", val)) == @macroexpand styled"a$val"
        @test :($annotatedstring("a", val, "b")) == @macroexpand styled"a$(val)b"
        # @test :($annotatedstring(StyledStrings.AnnotatedString(string(val), $(Pair{Symbol, Any}(:face, :style))))) ==
        #     @macroexpand styled"{style:$val}"
        @test :($annotatedstring($AnnotatedString(
            "val", [($(1:3), $Pair{$Symbol, $Any}(:face, face))]))) ==
            @macroexpand styled"{$face:val}"
        @test :($annotatedstring($AnnotatedString(
            "val", [($(1:3), $Pair{$Symbol, $Any}(key, "val"))]))) ==
            @macroexpand styled"{$key=val:val}"
        @test :($annotatedstring($AnnotatedString(
            "val", [($(1:3), $Pair{$Symbol, $Any}(key, val))]))) ==
            @macroexpand styled"{$key=$val:val}"
        # @test :($annotatedstring($AnnotatedString(
        #     string(val), $Pair{$Symbol, $Any}(key, val)))) ==
        #     @macroexpand styled"{$key=$val:$val}"
        @test :($annotatedstring($AnnotatedString(
            "val", [($(1:3), $Pair{$Symbol, $Any}(:face, $(Face)(foreground = color)))]))) ==
            @macroexpand styled"{(foreground=$color):val}"
    end

    # Trailing (and non-trailing) Backslashes
    @test String(styled"\\") == "\\"
    @test String(styled"\\\\") == "\\\\"
    @test String(styled"\\\\\\") == "\\\\\\"
    @test String(styled".\\") == ".\\"
    @test String(styled".\\\\") == ".\\\\"
    @test String(styled".\\\\\\") == ".\\\\\\"

    # newlines
    strlines = "abc\
                def"
    stylines = styled"abc\
                      def"
    @test strlines == stylines == "abcdef"

    strlines = "abc\\ndef"
    stylines = styled"abc\\ndef"
    @test strlines == stylines == "abc\\ndef"

    strlines = eval(Meta.parse("\"abc\\\n \tdef\""))
    stylines = eval(Meta.parse("styled\"abc\\\n \tdef\""))
    @test strlines == stylines == "abcdef"

    strlines = eval(Meta.parse("\"abc\\\r\n  def\""))
    stylines = eval(Meta.parse("styled\"abc\\\r\n  def\""))
    @test strlines == stylines == "abcdef"

    # The function form. As this uses the same FSM as the macro,
    # we don't need many tests to verify it's behaving sensibly.
    @test styled("{red:hey} {blue:there}") == styled"{red:hey} {blue:there}"
    @test styled("\\{green:hi\\}") == styled"\{green:hi\}"
    @test styled("\$hey") == styled"\$hey"
end

@testset "AnnotatedIOBuffer" begin
    aio = Base.AnnotatedIOBuffer()
    @test write(aio, styled"{red:hey} {blue:there}") == 9
    buf = IOBuffer()
    @test write(buf, seekstart(aio)) == 9
    @test String(take!(buf)) == "hey there"
    cbuf = IOContext(buf, :color => true)
    @test write(cbuf, seekstart(aio)) == 29
    @test String(take!(buf)) == "\e[31mhey\e[39m \e[34mthere\e[39m"
end

@testset "Legacy" begin
    @test StyledStrings.Legacy.legacy_color(:blue) == SimpleColor(:blue)
    @test StyledStrings.Legacy.legacy_color(:light_blue) == SimpleColor(:bright_blue)
    @test StyledStrings.Legacy.legacy_color(-1) === nothing
    @test StyledStrings.Legacy.legacy_color(0) == SimpleColor(0x000000)
    @test StyledStrings.Legacy.legacy_color(44) == SimpleColor(0x00d7d7)
    @test StyledStrings.Legacy.legacy_color(255) == SimpleColor(0xeeeeee)
    @test StyledStrings.Legacy.legacy_color(256) === nothing
    @test StyledStrings.Legacy.legacy_color("blue") == SimpleColor(:blue)
    @test StyledStrings.Legacy.legacy_color("light_blue") == SimpleColor(:bright_blue)
    @test StyledStrings.Legacy.legacy_color("-1") === nothing
    @test StyledStrings.Legacy.legacy_color("0") == SimpleColor(0x000000)
    @test StyledStrings.Legacy.legacy_color("44") == SimpleColor(0x00d7d7)
    @test StyledStrings.Legacy.legacy_color("255") == SimpleColor(0xeeeeee)
    @test StyledStrings.Legacy.legacy_color("256") === nothing
    @test StyledStrings.Legacy.legacy_color("invalid") === nothing
end
