# Usage Examples

!!! info "Visual inconsistencies"
    There are some examples on this page that don't quite look as they should, see [Documenter.jl#2488](https://github.com/JuliaDocs/Documenter.jl/issues/2488) for more information and potential improvements to this situation.

With flexibility and composability as major considerations in the design of
StyledStrings, it is easy to describe the capabilities of this system while
failing to actually convey what it can accomplish. With this in mind, examples
can be particularly useful to show how it can be used.

A styled string can be constructed manually, but the [`styled"..."`](@ref
@styled_str) literal is almost always a nicer option. We can see what a manual construction would involve by extracting the string and annotation parts of a [`AnnotatedString`](@ref Base.AnnotatedString).

```@repl examples
using StyledStrings
str = styled"{yellow:hello} {blue:there}"
(String(str), annotations(str))
```

```@setup example
# Due to a bug in Documenter.jl we've run into issues with printing
# with color. This should always happen here, so we can try a hacky
# workaround for now.
Base.get(::IOContext, s::Symbol, d::Bool) = s === :color || d
```

Most of the examples here will show [`AnnotatedString`](@ref Base.AnnotatedString) in a terminal/REPL context, however they can be trivially adapted to produce HTML with `show(::IO, ::MIME"text/plain", ::AnnotatedString)`, and other packages wanting do deal with styled content in other contexts will likely find integration with StyledStrings worthwhile.

## Basic face application and colouring

As an end-user or a package-author, adding color is one of if not the most frequent application of styling. The default set of colors has the eight ANSI colors, and if we're to be brutally honest, `black`/`white` are just shades, leaving us with:

```@repl examples
styled"{red:■} {green:■} {yellow:■} {blue:■} {magenta:■} {cyan:■}"
```

along with "bright"  variants

```@repl examples
styled"{bright_red:■} {bright_green:■} {bright_yellow:■} \
       {bright_blue:■} {bright_magenta:■} {bright_cyan:■}"
```

## Adding new faces

This seems somewhat limited, because it is. This is only the _default_ set of
colors though. It is important to note that the way the color `red` is
implemented is by having it name a `Face(foreground=:red)` value. The ANSI
printer knows to handle the ANSI named colors specially, but you can create
more "named colors" simply by adding new faces.

```@repl examples
StyledStrings.addface!(:orange => StyledStrings.Face(foreground = 0xFF7700))
styled"{orange:this is orange text}"
```

!!! warning "Appropriate face naming"
    The face name `orange` is used here as an example, but this would be
    inappropriate for a package to introduce as it's missing the `packagename_`
    prefix. This is important for predictability, and to prevent name clashes.

The fact that named colors are implemented this way also allows for other nice
conveniences. For example, if you wanted a more subtle version of the warning
styling, you could print text with the underline color set to the warning
foreground color.

```@repl examples
styled"{(underline=warning):this is some minor/slight warning text}"
styled"A very major {(fg=error,bg=warning),bold:warning!}"
```

!!! tip "Focus on semantics, and let style follow"
    Note that when using faces, it is generally recommended to focus on the
    *semantic intent*, not the specific styling (`cyan`, `bold`, etc.),
    introducing new faces when there is no pre-existing semantically appropriate
    face.

## Inline face attributes

Should you want to use a particular color just once, the `foreground`/`fg` and `background`/`bg` inline face attributes can be set to hex codes.

```@repl examples
styled"{(fg=#4063d8):ju}{(fg=#389826):l}{(fg=#cb3c33):i}{(fg=#9558b2):a}"
```

This works just as well with named colours.

```@repl examples
styled"Since {orange:orange} is a face with a foreground, \
       you can use it as a {(fg=orange):foreground color}."
```

All the other [`Face`](@ref StyledStrings.Face) attributes can be set similarly.

## Considerations when creating and reusing faces

It is recommended that package authors create faces with a focus on the semantic
meaning they wish to impart, and then consider what styling suits. For example,
say that a hypothetical package `foobar` wants to mark something as important.
Creating a named face allows for it to be re-used across the codebase and allows
the styling everywhere its used to be updated by only changing the line
declaring it. `foobar_important` would be an appropriate name for such a face.

```@repl examples
StyledStrings.addface!(:foobar_important => StyledStrings.Face(weight = :bold, inherit = :emphasis))
styled"this is some {foobar_important:rather important} content"
```

Other packages that interact with `foobar` can also re-use the
`foobar_important` face for consistent styling. This is possible even for
packages that don't have `foobar` as a direct dependency, as faces that don't
exist are just ignored. Consider this styled content as an example:

```@repl examples
styled"{info,foobar_important,baz_important:some text}"
```

The styling of `"some text"` will be based only on `info` if neither
`foobar_important` or `baz_important` are defined. Since `foobar_important` _is_
defined, after applying the attributes of `info`, the attributes of
`foobar_important` are applied to `"some text"` _overwriting_ any attributes set
by `info`. Should `bar_important` be defined in the future, any attributes it
sets will override `foobar_important` and `info`. Put more simply, the last face
mentioned "wins".

!!! note
    The silent ignoring of undefined faces is important in making it so that it's known if a [`styled"..."`](@ref @styled_str) string will cause errors when printed is known at compile-time instead of runtime.
    
## User-customisation of faces
    
Naming faces also allows for convenient customisation. Once `foobar_important`
is defined, a user can change how it is styled in their `faces.toml`.

```toml
[foobar.important]
italic = true
```

!!! note "Accessibility"
    User-customisation is particularly important when using color, as it allows people with color-blindness or other neuro-ophthalmological abnormalities to make text easier to read/distinguish.
    
## Application-customisation of faces

Named faces can also be customised on-the-fly in certain printing contexts created by [`withfaces`](@ref StyledStrings.withfaces).

```@repl examples
StyledStrings.withfaces(:foobar_important => :tip) do
    println(styled"Sometimes you might want {foobar_important:some text} to look different")
end
StyledStrings.withfaces(:log_info => [:magenta, :italic]) do
    @info "Hello there"
end
```

This feature can be used to for example change the default colors to follow a
certain color theme when generating HTML output.

```@repl examples
StyledStrings.withfaces(:green => StyledStrings.Face(foreground = 0xb7ba25),
                        :yellow => StyledStrings.Face(foreground = 0xfabc2e),
                        :magenta => StyledStrings.Face(foreground = 0xd2859a)) do
    str = styled"Sometimes you might want {green:different} {yellow:shades} of {magenta:colors}."
    println(str, "\n")
    show(stdout, MIME("text/html"), str)
end
```

## Composing and interpolating styled content

As you work with more styled content, the ability to compose styled content and
styling information is rather useful. Helpfully, StyledStrings allows for
interpolation of both content and attributes.

```@repl examples
small_rainbow = (:red, :yellow, :green, :blue, :magenta)
color = join([styled"{$f:$c}" for (f, c) in tuple.(small_rainbow, collect("color"))])
styled"It's nice to include $color, and it composes too: {bold,inverse:$color}"
```

Sometimes it's useful to compose a string incrementally, or interoperate with
other `IO`-based code. For these use-cases, the [`AnnotatedIOBuffer`](@ref Base.AnnotatedIOBuffer) is very handy, as you can [`read`](@ref Base.read) an [`AnnotatedString`](@ref Base.AnnotatedString) from it.

```@repl examples
aio = AnnotatedIOBuffer()
typ = Int
print(aio, typ)
while typ != Any # We'll pretend that `supertypes` doesn't exist.
    typ = supertype(typ)
    print(aio, styled" {bright_red:<:} $typ")
end
read(seekstart(aio), AnnotatedString)
```

StyledStrings adds a specialised [`printstyled`](@ref) method `printstyled(::AnnotatedIOBuffer, ...)` that means that you can pass an `AnnotatedIOBuffer` as IO to "legacy" code written to use `printstyled`, and extract all the styling as though it had used [`styled"..."`](@ref @styled_str) macros.

```@repl
aio = AnnotatedIOBuffer()
printstyled(aio, 'c', color=:red)
printstyled(aio, 'o', color=:yellow)
printstyled(aio, 'l', color=:green)
printstyled(aio, 'o', color=:blue)
printstyled(aio, 'r', color=:magenta)
read(seekstart(aio), AnnotatedString)
read(seekstart(aio), String)
```
