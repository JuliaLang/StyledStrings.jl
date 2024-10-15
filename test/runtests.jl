# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test

using StyledStrings: StyledStrings, Legacy, SimpleColor, FACES, Face,
    @styled_str, styled, StyledMarkup, eachregion, getface, addface!, loadface!, resetfaces!
using .StyledMarkup: MalformedStylingMacro
import StyledStrings.AnnotatedStrings: AnnotatedString, AnnotatedChar,
    AnnotatedIOBuffer, annotations
using StyledStrings.Compat

# include("styfuzz.jl") # For use in the "Styled Markup" testset
styfuzz() = nothing

# For output testing

const vt100 = StyledStrings.TermInfo(read(joinpath(@__DIR__, "terminfos", "vt100"), StyledStrings.TermInfoRaw))
const fancy_term = StyledStrings.TermInfo(read(joinpath(@__DIR__, "terminfos", "fancy"), StyledStrings.TermInfoRaw))

function with_terminfo(fn::Function, tinfo::StyledStrings.TermInfo)
    prev_terminfo = StyledStrings.current_terminfo
    prev_truecolor = StyledStrings.have_truecolor
    try
        Core.eval(StyledStrings, :(current_terminfo = $tinfo))
        Core.eval(StyledStrings, :(have_truecolor = $(haskey(tinfo, :setrgbf))))
        fn()
    catch _
    end
    Core.eval(StyledStrings, :(current_terminfo = $prev_terminfo))
    Core.eval(StyledStrings, :(have_truecolor = $prev_truecolor))
end


@static if VERSION < v"1.8"
    function chopprefix(s::Union{String, SubString{String}},
                        prefix::Union{String, SubString{String}})
        if startswith(s, prefix)
            SubString(s, 1 + ncodeunits(prefix))
        else
            SubString(s)
        end
    end
end

# When tested as part of the stdlib, the package prefix can start appearing in show methods.
choppkg(s::String) = chopprefix(s, "StyledStrings.")

@testset "Eachregion" begin
    annregions(str::String, annots::Vector{<:Tuple{UnitRange{Int}, <:Pair{Symbol, <:Any}}}) =
        collect(eachregion(AnnotatedString(str, annots)))
    # Regions that do/don't extend to the left/right edges
    @test annregions(" abc ", [(2:4, :face => :bold)]) ==
        [(" ", []),
         ("abc", [:face => :bold]),
         (" ", [])]
    @test annregions(" x ", [(2:2, :face => :bold)]) ==
        [(" ", []),
         ("x", [:face => :bold]),
         (" ", [])]
    @test annregions(" x", [(2:2, :face => :bold)]) ==
        [(" ", []),
         ("x", [:face => :bold])]
    @test annregions("x ", [(1:1, :face => :bold)]) ==
        [("x", [:face => :bold]),
         (" ", [])]
    @test annregions("x", [(1:1, :face => :bold)]) ==
        [("x", [:face => :bold])]
    # Overlapping/nested regions
    @test annregions(" abc ", [(2:4, :face => :bold), (3:3, :face => :italic)]) ==
        [(" ", []),
         ("a", [:face => :bold]),
         ("b", [:face => :bold, :face => :italic]),
         ("c", [:face => :bold]),
         (" ", [])]
    @test annregions("abc-xyz", [(1:7, :face => :bold), (1:3, :face => :green), (4:4, :face => :yellow), (4:7, :face => :italic)]) ==
        [("abc", [:face => :bold, :face => :green]),
         ("-", [:face => :bold, :face => :yellow, :face => :italic]),
         ("xyz", [:face => :bold, :face => :italic])]
    # Preserving annotation order
    @test annregions("abcd", [(1:3, :face => :red), (2:2, :face => :yellow), (2:3, :face => :green), (2:4, :face => :blue)]) ==
        [("a", [:face => :red]),
         ("b", [:face => :red, :face => :yellow, :face => :green, :face => :blue]),
         ("c", [:face => :red, :face => :green, :face => :blue]),
         ("d", [:face => :blue])]
    @test annregions("abcd", [(2:4, :face => :blue), (1:3, :face => :red), (2:3, :face => :green), (2:2, :face => :yellow)]) ==
        [("a", [:face => :red]),
         ("b", [:face => :blue, :face => :red, :face => :green, :face => :yellow]),
         ("c", [:face => :blue, :face => :red, :face => :green]),
         ("d", [:face => :blue])]
    # Region starting after a character spanning multiple codepoints.
    @test annregions("𝟏x", [(1:4, :face => :red)]) ==
        [("𝟏", [:face => :red]),
         ("x", [])]
end

@testset "SimpleColor" begin
    @test SimpleColor(:hey).value == :hey # no error
    @test SimpleColor(0x01, 0x02, 0x03).value == (r=0x01, g=0x02, b=0x03)
    @test SimpleColor((r=0x01, g=0x02, b=0x03)).value == (r=0x01, g=0x02, b=0x03)
    @test SimpleColor(0x010203).value == (r=0x01, g=0x02, b=0x03)
    @test tryparse(SimpleColor, "hey") == SimpleColor(:hey)
    @test tryparse(SimpleColor, "#010203") == SimpleColor(0x010203)
    @test tryparse(SimpleColor, "#12345g") === nothing
    @test tryparse(SimpleColor, "!not a color") === nothing
    @test parse(SimpleColor, "blue") == SimpleColor(:blue)
    @test_throws ArgumentError parse(SimpleColor, "!not a color")
    @test sprint(show, SimpleColor(:blue)) |> choppkg ==
        "SimpleColor(:blue)"
    @test sprint(show, SimpleColor(0x123456)) |> choppkg ==
        "SimpleColor(0x123456)"
    @test sprint(show, MIME("text/plain"), SimpleColor(:blue)) |> choppkg ==
        "SimpleColor(blue)"
    @test sprint(show, MIME("text/plain"), SimpleColor(:blue), context = :color => true) |> choppkg ==
        "SimpleColor(\e[34m■\e[39m blue)"
    @static if VERSION >= v"1.7"
        @test sprint(show, MIME("text/plain"), SimpleColor(:blue), context = (:color => true, :typeinfo => SimpleColor)) ==
            "\e[34m■\e[39m blue"
    end
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
    @test Face(foreground="red") ==
        Face(nothing, nothing, nothing, nothing, SimpleColor(:red),
             nothing, nothing, nothing, nothing, Symbol[])
    @test Face(foreground="#ff0000") ==
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
    @test loadface!(:testface => Face(font="test")) == Face(font="test")
    @test get(FACES.current[], :testface, nothing) == Face(font="test")
    @test loadface!(:bold => Face(weight=:extrabold)) == Face(weight=:extrabold)
    @test get(FACES.current[], :bold, nothing) == Face(weight=:extrabold)
    @test_nowarn loadface!(:bold => nothing)
    @test get(FACES.current[], :bold, nothing) == Face(weight=:bold)
    @test loadface!(:testface => Face(height=2.0)) == Face(font="test", height=2.0)
    @test get(FACES.current[], :testface, nothing) == Face(font="test", height=2.0)
    @static if VERSION >= v"1.7"
        @test_warn "reset, but it had no default value" loadface!(:testface => nothing)
    else
        loadface!(:testface => nothing)
    end
    @test get(FACES.current[], :testface, nothing) === nothing
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
    @test get(FACES.current[], :anotherface, nothing) ==
        Face(font = "afont", height = 123, weight = :semibold,
             slant = :oblique, foreground = :green, background = :magenta,
             underline = (:blue, :curly), strikethrough = true,
             inverse = true, inherit = [:iface])
    StyledStrings.resetfaces!()
    @test get(FACES.current[], :bold, nothing) == Face(weight=:bold)
    @test haskey(FACES.current[], :testface) == false
    @test haskey(FACES.current[], :anotherface) == false
    # `withfaces`
    @test StyledStrings.withfaces(:testface => Face(font="test")) do
        get(FACES.current[], :testface, nothing)
    end == Face(font="test")
    @test haskey(FACES.current[], :testface) == false
    @test StyledStrings.withfaces(:red => :green) do
        get(FACES.current[], :red, nothing)
    end == Face(foreground=:green)
    @test StyledStrings.withfaces(:red => [:green, :inverse]) do
        get(FACES.current[], :red, nothing)
    end == Face(inherit=[:green, :inverse])
    @test StyledStrings.withfaces(:red => nothing) do
        get(FACES.current[], :red, nothing)
    end === nothing
    @test StyledStrings.withfaces(Dict(:green => Face(foreground=:blue))) do
        get(FACES.current[], :green, nothing)
    end == Face(foreground=:blue)
    @test StyledStrings.withfaces(() -> 1) == 1
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
        loadface!(:a => aface)
        loadface!(:b => bface)
        loadface!(:c => cface)
        loadface!(:d => dface)
        loadface!(:e => eface)
        loadface!(:f => fface)
        @test getface(:c) == merge(FACES.current[][:default], aface, bface, Face(height=120), cface)
        @test getface(:b) == merge(FACES.current[][:default], aface, Face(height=120), bface)
        @test getface(:a) == merge(FACES.current[][:default], aface)
        @test getface([:c]) == getface(:c)
        @test getface(bface) == getface(:b)
        @test getface(cface) == getface(:c)
        @test getface([:c, :d]).foreground.value == :blue
        @test getface([[:c, :d]]).foreground.value == :red
        @test getface(:e).foreground.value == :red
        @test getface([:d, :c]).foreground.value == :red
        @test getface([[:d, :c]]).foreground.value == :blue
        @test getface(:f).foreground.value == :blue
        StyledStrings.resetfaces!()
    end
    # Equality/hashing equivalence
    let testfaces = [Face(foreground=:blue),
                     Face(background=:blue),
                     Face(inherit=:something),
                     Face(inherit=:something)]
        for f1 in testfaces, f2 in testfaces
            @test (f1 == f2) == (hash(f1) == hash(f2))
        end
    end
    # Pretty display
    @test sprint(show, MIME("text/plain"), getface()) |> choppkg ==
        """
        Face (sample)
                  font: monospace
                height: 120
                weight: normal
                 slant: normal
            foreground: default
            background: default
             underline: false
         strikethrough: false
               inverse: false"""
    @test sprint(show, MIME("text/plain"), getface(), context = :color => true) |> choppkg ==
        """
        Face (sample)
                  font: monospace
                height: 120
                weight: normal
                 slant: normal
            foreground: ■ default
            background: ■ default
             underline: false
         strikethrough: false
               inverse: false"""
    @test sprint(show, MIME("text/plain"), FACES.default[:red], context = :color => true) |> choppkg ==
        """
        Face (\e[31msample\e[39m)
            foreground: \e[31m■\e[39m red"""
    @test sprint(show, FACES.default[:red]) |> choppkg ==
        "Face(foreground=SimpleColor(:red))"
    @test sprint(show, MIME("text/plain"), FACES.default[:red], context = :compact => true) |> choppkg ==
        "Face(foreground=SimpleColor(:red))"
    @static if VERSION >= v"1.7"
        @test sprint(show, MIME("text/plain"), FACES.default[:red], context = (:compact => true, :color => true)) |> choppkg ==
            "Face(\e[31msample\e[39m)"
    end
    @test sprint(show, MIME("text/plain"), FACES.default[:highlight], context = :compact => true) |> choppkg ==
        "Face(inverse=true, inherit=[:emphasis])"
    with_terminfo(vt100) do # Not truecolor capable
        @test sprint(show, MIME("text/plain"), FACES.default[:region], context = :color => true) |> choppkg ==
            """
            Face (\e[48;5;237msample\e[49m)
                background: \e[38;5;237m■\e[39m #3a3a3a"""
    end
    with_terminfo(fancy_term) do # Truecolor capable
        @test sprint(show, MIME("text/plain"), FACES.default[:region], context = :color => true) |> choppkg ==
            """
            Face (\e[48;2;58;58;58msample\e[49m)
                background: \e[38;2;58;58;58m■\e[39m #3a3a3a"""
    end
    with_terminfo(vt100) do # Ensure `enter_reverse_mode` exists
        @test sprint(show, MIME("text/plain"), FACES.default[:highlight], context = :color => true) |> choppkg ==
            """
            Face (\e[34m\e[7msample\e[39m\e[27m)
                   inverse: true
                   inherit: emphasis(\e[34m*\e[39m)"""
    end
end

@static if VERSION < v"1.1" # Why *on earth* is this needed!?!?
    macro maybetestset(label, body)
        esc(body)
    end
elseif VERSION < v"1.3"
    macro maybetestset(label, body)
        esc(:(@testset($label, $body)))
    end
else
    var"@maybetestset" = var"@testset"
end

@maybetestset "Styled Markup" begin
    # Preservation of an unstyled string
    @test styled"some string" == AnnotatedString("some string")
    # Basic styled constructs
    @test styled"{thing=val:some} string" == AnnotatedString("some string", [(1:4, :thing => "val")])
    @test styled"some {thing=val:string}" == AnnotatedString("some string", [(6:11, :thing => "val")])
    @test styled"some {a=1:s}trin{b=2:g}" == AnnotatedString("some string", [(6:6, :a => "1"), (11:11, :b => "2")])
    @test styled"{thing=val with spaces:some} string" == AnnotatedString("some string", [(1:4, :thing => "val with spaces")])
    @test styled"{aface:some} string" == AnnotatedString("some string", [(1:4, :face => :aface)])
    # Annotation prioritisation
    @test styled"{aface,bface:some} string" ==
        AnnotatedString("some string", [(1:4, :face => :aface), (1:4, :face => :bface)])
    @test styled"{aface:{bface:some}} string" ==
        AnnotatedString("some string", [(1:4, :face => :aface), (1:4, :face => :bface)])
    @test styled"{aface,bface:$(1)} string" ==
        AnnotatedString("1 string", [(1:1, :face => :aface), (1:1, :face => :bface)])
    @test styled"{aface:{bface:$(1)}} string" ==
        AnnotatedString("1 string", [(1:1, :face => :aface), (1:1, :face => :bface)])
    # Inline face attributes
    @test styled"{(slant=italic):some} string" ==
        AnnotatedString("some string", [(1:4, :face => Face(slant=:italic))])
    @test styled"{(foreground=magenta,background=#555555):some} string" ==
        AnnotatedString("some string", [(1:4, :face => Face(foreground=:magenta, background=0x555555))])
    # Inline face attributes: empty attribute lists are legal
    @test styled"{():}" == styled"{( ):}" == AnnotatedString("")
    # Inline face attributes: leading/trailing whitespace
    @test styled"{ ( fg=red , ) :a}" == AnnotatedString("a", [(1:1, :face => Face(foreground=:red))])
    # Inline face attributes: each recognised key
    @test styled"{(font=serif):a}" == AnnotatedString("a", [(1:1, :face => Face(font="serif"))])
    @test styled"{(font=some serif):a}" == AnnotatedString("a", [(1:1, :face => Face(font="some serif"))])
    @test styled"{(font=\"some serif\"):a}" == AnnotatedString("a", [(1:1, :face => Face(font="some serif"))])
    @test styled"{(font=\"{},):\"):a}" == AnnotatedString("a", [(1:1, :face => Face(font="{},):"))])
    @test styled"{(height=120):a}" == AnnotatedString("a", [(1:1, :face => Face(height=120))])
    @test styled"{(height=1.2):a}" == AnnotatedString("a", [(1:1, :face => Face(height=1.2))])
    @test styled"{(weight=normal):a}" == AnnotatedString("a", [(1:1, :face => Face(weight=:normal))])
    @test styled"{(weight=bold):a}" == AnnotatedString("a", [(1:1, :face => Face(weight=:bold))])
    @test styled"{(slant=italic):a}" == AnnotatedString("a", [(1:1, :face => Face(slant=:italic))])
    @test styled"{(fg=red):a}" == AnnotatedString("a", [(1:1, :face => Face(foreground=:red))])
    @test styled"{(foreground=red):a}" == AnnotatedString("a", [(1:1, :face => Face(foreground=:red))])
    @test styled"{(bg=red):a}" == AnnotatedString("a", [(1:1, :face => Face(background=:red))])
    @test styled"{(background=red):a}" == AnnotatedString("a", [(1:1, :face => Face(background=:red))])
    @test styled"{(underline=true):a}" == AnnotatedString("a", [(1:1, :face => Face(underline=true))])
    @test styled"{(underline=cyan):a}" == AnnotatedString("a", [(1:1, :face => Face(underline=:cyan))])
    @test styled"{(underline=(cyan,curly)):a}" == AnnotatedString("a", [(1:1, :face => Face(underline=(:cyan, :curly)))])
    @test styled"{(strikethrough=true):a}" == AnnotatedString("a", [(1:1, :face => Face(strikethrough=true))])
    @test styled"{(inverse=true):a}" == AnnotatedString("a", [(1:1, :face => Face(inverse=true))])
    @test styled"{(inherit=b):a}" == AnnotatedString("a", [(1:1, :face => Face(inherit=:b))])
    @test styled"{(inherit=[b,c]):a}" == AnnotatedString("a", [(1:1, :face => Face(inherit=[:b, :c]))])
    # Curly bracket escaping
    @test styled"some \{string" == AnnotatedString("some {string")
    @test styled"some string\}" == AnnotatedString("some string}")
    @test styled"some \{string\}" == AnnotatedString("some {string}")
    @test styled"some \{str:ing\}" == AnnotatedString("some {str:ing}")
    @test styled"some \{{bold:string}\}" == AnnotatedString("some {string}", [(7:12, :face => :bold)])
    @test styled"some {bold:string \{other\}}" == AnnotatedString("some string {other}", [(6:19, :face => :bold)])
    # Nesting
    @test styled"{bold:nest{italic:ed st{red:yling}}}" ==
        AnnotatedString(
            "nested styling", [(1:14, :face => :bold), (5:14, :face => :italic), (10:14, :face => :red)])
    # Production of a `(AnnotatedString)` value instead of an expression when possible
    @test AnnotatedString("val") == @macroexpand styled"val"
    @test AnnotatedString("val", [(1:3, :face => :style)]) == @macroexpand styled"{style:val}"
    # Interpolation
    let annotatedstring = GlobalRef(StyledMarkup, :annotatedstring)
        AnnotatedString = GlobalRef(StyledMarkup, :AnnotatedString)
        annotatedstring_optimize! = GlobalRef(StyledMarkup, :annotatedstring_optimize!)
        chain = GlobalRef(StyledMarkup, :|>)
        Pair            = GlobalRef(StyledMarkup, :Pair)
        Symbol          = GlobalRef(StyledMarkup, :Symbol)
        Any             = GlobalRef(StyledMarkup, :Any)
        @test :($chain($annotatedstring(val), $annotatedstring_optimize!)) == @macroexpand styled"$val"
        @test :($chain($annotatedstring("a", val), $annotatedstring_optimize!)) == @macroexpand styled"a$val"
        @test :($chain($annotatedstring("a", val, "b"), $annotatedstring_optimize!)) == @macroexpand styled"a$(val)b"
        # @test :($annotatedstring(StyledStrings.AnnotatedString(string(val), $(Pair{Symbol, Any}(:face, :style))))) ==
        #     @macroexpand styled"{style:$val}"
        @test :($chain($annotatedstring($AnnotatedString(
            "val", [($(1:3), $Pair{$Symbol, $Any}(:face, face))])),
                       $annotatedstring_optimize!)) ==
            @macroexpand styled"{$face:val}"
        @test :($chain($annotatedstring($AnnotatedString(
            "val", [($(1:3), $Pair{$Symbol, $Any}(key, "val"))])),
                       $annotatedstring_optimize!)) ==
            @macroexpand styled"{$key=val:val}"
        @test :($chain($annotatedstring($AnnotatedString(
            "val", [($(1:3), $Pair{$Symbol, $Any}(key, val))])),
                       $annotatedstring_optimize!)) ==
            @macroexpand styled"{$key=$val:val}"
        # @test :($annotatedstring($AnnotatedString(
        #     string(val), $Pair{$Symbol, $Any}(key, val)))) ==
        #     @macroexpand styled"{$key=$val:$val}"
        @test :($chain($annotatedstring($AnnotatedString(
            "val", [($(1:3), $Pair{$Symbol, $Any}(:face, $(Face)(foreground = color)))])),
                       $annotatedstring_optimize!)) ==
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
    @static if VERSION >= v"1.7"
        strlines = "abc\\ndef"
        stylines = styled"abc\\ndef"
        @test strlines == stylines == "abc\\ndef"

        strlines = eval(Meta.parse("\"abc\\\n \tdef\""))
        stylines = eval(Meta.parse("styled\"abc\\\n \tdef\""))
        @test strlines == stylines == "abcdef"

        strlines = eval(Meta.parse("\"abc\\\r\n  def\""))
        stylines = eval(Meta.parse("styled\"abc\\\r\n  def\""))
        @test strlines == stylines == "abcdef"
    end

    # The function form. As this uses the same FSM as the macro,
    # we don't need many tests to verify it's behaving sensibly.
    @test styled("{red:hey} {blue:there}") == styled"{red:hey} {blue:there}"
    @test styled("\\{green:hi\\}") == styled"\{green:hi\}"
    @test styled("\$hey") == styled"\$hey"

    StyErr = if VERSION >= v"1.6" MalformedStylingMacro else Exception end
    # Various kinds of syntax errors that should be reported
    @test_throws StyErr styled("{incomplete")
    @test_throws StyErr styled("{unterminated:")
    # @test_throws LoadError styled("$") # FIXME still throws 😢
    @test_throws StyErr styled("}")
    @test_throws StyErr styled("{(:}")
    @test_throws StyErr styled("{(underline=()):}")
    @test_throws StyErr styled("{(underline=(_)):}")
    @test_throws StyErr styled("{(underline=(_,invalid)):}")
    @test_throws StyErr styled("{(height=invalid):}")
    @test_throws StyErr styled("{(weight=invalid):}")
    @test_throws StyErr styled("{(slant=invalid):}")
    @test_throws StyErr styled("{(invalid=):}")
    # Test the error printing too
    aio = AnnotatedIOBuffer()
    try
        styled("{")
    catch err
        showerror(aio, err)
    end
    errstr = read(seekstart(aio), AnnotatedString)
    @test errstr ==
        styled"MalformedStylingMacro\n\
               {error:│} Incomplete annotation declaration:\n\
               {error:│}  {bright_green:\"\{\"}\n\
               {error:│}   {info:╰─╴starts here}\n\
               {error:┕} {light,italic:1 issue}\n"
end

# Markup fuzzing!
styfuzz()

@testset "AnnotatedIOBuffer" begin
    aio = AnnotatedIOBuffer()
    @test write(aio, styled"{red:hey} {blue:there}") == 9
    buf = IOBuffer()
    @test write(buf, seekstart(aio)) == 9
    @test String(take!(buf)) == "hey there"
    cbuf = IOContext(buf, :color => true)
    @test write(cbuf, seekstart(aio)) == 29
    @test String(take!(buf)) == "\e[31mhey\e[39m \e[34mthere\e[39m"
end

@testset "ANSI encoding" begin
    # 4-bit color
    @test StyledStrings.ansi_4bit_color_code(:cyan, false) == 36
    @test StyledStrings.ansi_4bit_color_code(:cyan, true) == 46
    @test StyledStrings.ansi_4bit_color_code(:bright_cyan, false) == 96
    @test StyledStrings.ansi_4bit_color_code(:bright_cyan, true) == 106
    @test StyledStrings.ansi_4bit_color_code(:nonexistant) == 39
    # 8-bit color
    @test sprint(StyledStrings.termcolor8bit, (r=0x40, g=0x63, b=0xd8), '3') == "\e[38;5;26m"
    @test sprint(StyledStrings.termcolor8bit, (r=0x38, g=0x98, b=0x26), '3') == "\e[38;5;28m"
    @test sprint(StyledStrings.termcolor8bit, (r=0x95, g=0x58, b=0xb2), '3') == "\e[38;5;97m"
    @test sprint(StyledStrings.termcolor8bit, (r=0xcb, g=0x3c, b=0x33), '3') == "\e[38;5;160m"
    @test sprint(StyledStrings.termcolor8bit, (r=0xee, g=0xee, b=0xee), '3') == "\e[38;5;255m"
    # 24-bit color
    @test sprint(StyledStrings.termcolor24bit, (r=0x40, g=0x63, b=0xd8), '3') == "\e[38;2;64;99;216m"
    @test sprint(StyledStrings.termcolor24bit, (r=0x38, g=0x98, b=0x26), '3') == "\e[38;2;56;152;38m"
    @test sprint(StyledStrings.termcolor24bit, (r=0x95, g=0x58, b=0xb2), '3') == "\e[38;2;149;88;178m"
    @test sprint(StyledStrings.termcolor24bit, (r=0xcb, g=0x3c, b=0x33), '3') == "\e[38;2;203;60;51m"
    # The color reset method
    @test sprint(StyledStrings.termcolor, nothing, '3') == "\e[39m"
    # ANSI attributes
    function ansi_change(; attrs...)
        face = getface(Face(; attrs...))
        dface = getface()
        sprint(StyledStrings.termstyle, face, dface),
        sprint(StyledStrings.termstyle, dface, face)
    end
    with_terminfo(vt100) do
        @test ansi_change(foreground=:cyan) == ("\e[36m", "\e[39m")
        @test ansi_change(background=:cyan) == ("\e[46m", "\e[49m")
        @test ansi_change(weight=:bold) == ("\e[1m", "\e[22m")
        @test ansi_change(weight=:extrabold) == ("\e[1m", "\e[22m")
        @test ansi_change(inverse=true) == ("\e[7m", "\e[27m")
        # Reduced-capability behaviours
        @test ansi_change(foreground=(r=0x40, g=0x63, b=0xd8)) == ("\e[38;5;26m", "\e[39m")
        @test ansi_change(background=(r=0x40, g=0x63, b=0xd8)) == ("\e[48;5;26m", "\e[49m")
        @test ansi_change(weight=:light) == ("", "\e[22m")
        @test ansi_change(slant=:italic) == ("\e[4m", "\e[24m")
        @test ansi_change(underline=true) == ("\e[4m", "\e[24m")
        @test ansi_change(underline=:green) == ("\e[4m", "\e[24m")
        @test ansi_change(strikethrough=true) == ("", "")
    end
    with_terminfo(fancy_term) do
        # Extra-capability behaviours
        @test ansi_change(foreground=(r=0x40, g=0x63, b=0xd8)) == ("\e[38;2;64;99;216m", "\e[39m")
        @test ansi_change(background=(r=0x40, g=0x63, b=0xd8)) == ("\e[48;2;64;99;216m", "\e[49m")
        @test ansi_change(weight=:light) == ("\e[2m", "\e[22m")
        @test ansi_change(slant=:italic) == ("\e[3m", "\e[23m")
        @test ansi_change(underline=:green) == ("\e[4m\e[58;5;2m", "\e[59m\e[24m")
        @test ansi_change(underline=:straight) == ("\e[4:1m", "\e[24m")
        @test ansi_change(underline=:double) == ("\e[4:2m", "\e[24m")
        @test ansi_change(underline=:curly)  == ("\e[4:3m", "\e[24m")
        @test ansi_change(underline=:dotted) == ("\e[4:4m", "\e[24m")
        @test ansi_change(underline=:dashed) == ("\e[4:5m", "\e[24m")
        @test ansi_change(underline=(:cyan, :double)) == ("\e[4:2m\e[58;5;6m", "\e[59m\e[24m")
        @test ansi_change(strikethrough=true) == ("\e[9m", "\e[29m")
    end
    # AnnotatedChar
    @test sprint(print, AnnotatedChar('a')) == "a"
    @test sprint(print, AnnotatedChar('a', [:face => :red]), context = :color => true) == "\e[31ma\e[39m"
    @test sprint(show, AnnotatedChar('a')) == "'a'"
    @test sprint(show, AnnotatedChar('a', [:face => :red]), context = :color => true) == "'\e[31ma\e[39m'"
    # Might as well put everything together for a final test
    fancy_string = styled"The {magenta:`{green:StyledStrings}`} package {italic:builds}{bold: on top} of the {magenta:`{green:AnnotatedString}`} {link={https://en.wikipedia.org/wiki/Type_system}:type} to provide a {(underline=(red,curly)):full-fledged} textual {(bg=#4063d8,fg=#adbdf8,inherit=[bold,strikethrough]):styling} system, suitable for {inverse:terminal} and graphical displays."
    @test sprint(print, fancy_string) == "The `StyledStrings` package builds on top of the `AnnotatedString` type to provide a full-fledged textual styling system, suitable for terminal and graphical displays."
    @test sprint(print, fancy_string[1:27], context = :color => true) ==
        "The \e[35m`\e[32mStyledStrings\e[35m`\e[39m package"
    with_terminfo(vt100) do
        @test sprint(print, fancy_string, context = :color => true) ==
            "The \e[35m`\e[32mStyledStrings\e[35m`\e[39m package \e[4mbuilds\e[1m\e[24m on top\e[22m of the \e[35m`\e[32mAnnotatedString\e[35m`\e[39m \e]8;;https://en.wikipedia.org/wiki/Type_system\e\\type\e]8;;\e\\ to provide a \e[4mfull-fledged\e[24m textual \e[38;5;147m\e[48;5;26m\e[1mstyling\e[39m\e[49m\e[22m system, suitable for \e[7mterminal\e[27m and graphical displays."
    end
    with_terminfo(fancy_term) do
        @test sprint(print, fancy_string, context = :color => true) ==
            "The \e[35m`\e[32mStyledStrings\e[35m`\e[39m package \e[3mbuilds\e[1m\e[23m on top\e[22m of the \e[35m`\e[32mAnnotatedString\e[35m`\e[39m \e]8;;https://en.wikipedia.org/wiki/Type_system\e\\type\e]8;;\e\\ to provide a \e[4:3m\e[58;5;1mfull-fledged\e[59m\e[24m textual \e[38;2;173;189;248m\e[48;2;64;99;216m\e[1m\e[9mstyling\e[39m\e[49m\e[22m\e[29m system, suitable for \e[7mterminal\e[27m and graphical displays."
    end
end

@testset "HTML encoding" begin
    @test sprint(StyledStrings.htmlcolor, SimpleColor(:black)) == "#1c1a23"
    @test sprint(StyledStrings.htmlcolor, SimpleColor(:green)) == "#25a268"
    @test sprint(StyledStrings.htmlcolor, SimpleColor(:warning)) == "#e5a509"
    @test sprint(StyledStrings.htmlcolor, SimpleColor(:nonexistant)) == "initial"
    @test sprint(StyledStrings.htmlcolor, SimpleColor(0x40, 0x63, 0xd8)) == "#4063d8"
    function html_change(; attrs...)
        face = getface(Face(; attrs...))
        sprint(StyledStrings.htmlstyle, face)
    end
    @test html_change(foreground=:cyan) == "<span style=\"color: #0097a7;\">"
    @test html_change(background=:cyan) == "<span style=\"background-color: #0097a7;\">"
    @test html_change(weight=:bold) == "<span style=\"font-weight: 700;\">"
    @test html_change(weight=:extrabold) == "<span style=\"font-weight: 800;\">"
    @test html_change(weight=:light) == "<span style=\"font-weight: 300;\">"
    @test html_change(foreground=:blue, background=:red, inverse=true) ==
        "<span style=\"color: #a51c2c;background-color: #195eb3;\">"
    @test html_change(slant=:italic) == "<span style=\"font-style: italic;\">"
    @test html_change(height=180) == "<span style=\"font-size: 18pt;\">"
    @test html_change(underline=true) == "<span style=\"text-decoration: underline;\">"
    @test html_change(underline=:green) == "<span style=\"text-decoration: #25a268 underline;\">"
    @test html_change(underline=:straight) == "<span style=\"text-decoration: solid underline;\">"
    @test html_change(underline=:double) == "<span style=\"text-decoration: double underline;\">"
    @test html_change(underline=:curly)  == "<span style=\"text-decoration: wavy underline;\">"
    @test html_change(underline=:dotted) == "<span style=\"text-decoration: dotted underline;\">"
    @test html_change(underline=:dashed) == "<span style=\"text-decoration: dashed underline;\">"
    @test html_change(underline=(:cyan, :double)) == "<span style=\"text-decoration: #0097a7 double underline;\">"
    @test html_change(strikethrough=true) == "<span style=\"text-decoration: line-through\">"
    # Might as well put everything together for a final test
    fancy_string = styled"The {magenta:`{green:StyledStrings}`} package {italic:builds}{bold: on top} of the {magenta:`{green:AnnotatedString}`} {link={https://en.wikipedia.org/wiki/Type_system}:type} to provide a {(underline=(red,curly)):full-fledged} textual {(bg=#4063d8,fg=#adbdf8,inherit=[bold,strikethrough]):styling} system, suitable for {inverse:terminal} and graphical displays."
    @test sprint(show, MIME("text/html"), fancy_string[1:27]) ==
        "The <span style=\"color: #803d9b;\">`</span><span style=\"color: #25a268;\">StyledStrings</span><span style=\"color: #803d9b;\">`</span> package"
    @test sprint(show, MIME("text/html"), fancy_string) ==
        "The <span style=\"color: #803d9b;\">`</span><span style=\"color: #25a268;\">StyledStrings</span><span style=\"color: #803d9b;\"> `</span> package <span style=\"font-style: italic;\">builds<span style=\"font-weight: 700;font-style: normal;\"> on top</span></span> of the <span style=\"color: #803d9b;\">`</span><span style=\"color: #25a268;\">AnnotatedString</span><span style=\"color: #803d9b;\">`</span> <a href=\"https://en.wikipedia.org/wiki/Type_system\">type</a> to provide a <span style=\"text-decoration: #a51c2c wavy underline;\">full-fledged</span> textual <span style=\"font-weight: 700;color: #adbdf8;background-color: #4063d8;text-decoration: line-through\">styling</span> system, suitable for <span style=\"\">terminal</span> and graphical displays."
end

@testset "Legacy" begin
    @test Legacy.legacy_color(:blue) == SimpleColor(:blue)
    @test Legacy.legacy_color(:light_blue) == SimpleColor(:bright_blue)
    @test Legacy.legacy_color(-1) === nothing
    @test Legacy.legacy_color(0) == SimpleColor(0x000000)
    @test Legacy.legacy_color(44) == SimpleColor(0x00d7d7)
    @test Legacy.legacy_color(255) == SimpleColor(0xeeeeee)
    @test Legacy.legacy_color(256) === nothing
    @test Legacy.legacy_color("blue") == SimpleColor(:blue)
    @test Legacy.legacy_color("light_blue") == SimpleColor(:bright_blue)
    @test Legacy.legacy_color("-1") === nothing
    @test Legacy.legacy_color("0") == SimpleColor(0x000000)
    @test Legacy.legacy_color("44") == SimpleColor(0x00d7d7)
    @test Legacy.legacy_color("255") == SimpleColor(0xeeeeee)
    @test Legacy.legacy_color("256") === nothing
    @test Legacy.legacy_color("invalid") === nothing
    withenv("JULIA_INFO_COLOR" => "magenta") do
        Legacy.load_env_colors!() isa Any
        @test getface(:info).foreground.value == :magenta
        StyledStrings.resetfaces!()
    end
    aio = AnnotatedIOBuffer()
    @test printstyled(aio, "a", bold=true)      |> isnothing
    @test printstyled(aio, "b", italic=true)    |> isnothing
    @test printstyled(aio, "c", underline=true) |> isnothing
    @test printstyled(aio, "d", reverse=true)   |> isnothing
    @test printstyled(aio, "e", color=:green)   |> isnothing
    @test read(seekstart(aio), AnnotatedString) == styled"{bold:a}{italic:b}{underline:c}{inverse:d}{(fg=green):e}"
end
