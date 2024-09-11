# [StyledStrings](@id stdlib-styledstrings)

```@meta
CurrentModule = StyledStrings
DocTestSetup = quote
    using StyledStrings
end
```

!!! note
    The API for StyledStrings and AnnotatedStrings is considered experimental and is subject to change between
    Julia versions.

## [Styling](@id stdlib-styledstrings-styling)

When working with strings, formatting and styling often appear as a secondary
concern.

For instance, when printing to a terminal you might want to sprinkle [ANSI
escape
sequences](https://en.wikipedia.org/wiki/ANSI_escape_code#SGR_(Select_Graphic_Rendition)_parameters)
in the output, when outputting HTML styling constructs (`<span style="...">`,
etc.) serve a similar purpose, and so on. It is possible to simply insert the
raw styling constructs into the string next to the content itself, but it
quickly becomes apparent that this is not well suited for anything but the most
basic use cases. Not all terminals support the same ANSI codes, the styling
constructs need to be painstakingly removed when calculating the width of
already-styled content, and that's before you even get into handling multiple
output formats.

Instead of leaving this headache to be widely experienced downstream, it is
tackled head-on by the introduction of a special string type
([`AnnotatedString`](@ref Base.AnnotatedString)). This string type wraps any other
[`AbstractString`](@ref) type and allows for formatting information to be applied to regions (e.g.
characters 1 through to 7 are bold and red).

Regions of a string are styled by applying [`Face`](@ref StyledStrings.Face)s
(think "typeface") to them — a structure that holds styling information. As a
convenience, faces in the global faces dictionary (e.g. `shadow`) can just be
named instead of giving the [`Face`](@ref StyledStrings.Face) directly.

Along with these capabilities, we also provide a convenient way for constructing
[`AnnotatedString`](@ref Base.AnnotatedString)s, detailed in [Styled String
Literals](@ref stdlib-styledstring-literals).

```@repl demo
using StyledStrings
styled"{yellow:hello} {blue:there}"
```

## [Annotated Strings](@id man-annotated-strings)

It is sometimes useful to be able to hold metadata relating to regions of a
string. A [`AnnotatedString`](@ref Base.AnnotatedString) wraps another string and
allows for regions of it to be annotated with labelled values (`:label => value`).
All generic string operations are applied to the underlying string. However,
when possible, styling information is preserved. This means you can manipulate a
[`AnnotatedString`](@ref Base.AnnotatedString) —taking substrings, padding them,
concatenating them with other strings— and the metadata annotations will "come
along for the ride".

This string type is fundamental to the [StyledStrings stdlib](@ref
stdlib-styledstrings), which uses `:face`-labelled annotations to hold styling
information.

When concatenating a [`AnnotatedString`](@ref Base.AnnotatedString), take care to use
[`annotatedstring`](@ref StyledStrings.annotatedstring) instead of [`string`](@ref) if you want
to keep the string annotations.

```jldoctest
julia> str = AnnotatedString("hello there", [(1:5, :word, :greeting), (7:11, :label, 1)])
"hello there"

julia> length(str)
11

julia> lpad(str, 14)
"   hello there"

julia> typeof(lpad(str, 7))
AnnotatedString{String}

julia> str2 = AnnotatedString(" julia", [(2:6, :face, :magenta)])
" julia"

julia> annotatedstring(str, str2)
"hello there julia"

julia> str * str2 == annotatedstring(str, str2) # *-concatenation works
true
```

The annotations of a [`AnnotatedString`](@ref Base.AnnotatedString) can be accessed
and modified via the [`annotations`](@ref StyledStrings.annotations) and
[`annotate!`](@ref StyledStrings.annotate!) functions.

## Styling via [`AnnotatedString`](@ref Base.AnnotatedString)s

## [Faces](@id stdlib-styledstrings-faces)

### The `Face` type

A [`Face`](@ref StyledStrings.Face) specifies details of a typeface that text can be set in. It
covers a set of basic attributes that generalize well across different formats,
namely:

- `font`
- `height`
- `weight`
- `slant`
- `foreground`
- `background`
- `underline`
- `strikethrough`
- `inverse`
- `inherit`

For details on the particular forms these attributes take, see the
[`Face`](@ref StyledStrings.Face) docstring, but of particular interest is `inherit` as it allows
you to _inherit_ attributes from other [`Face`](@ref StyledStrings.Face)s.

### The global faces dictionary

To make referring to particular styles more convenient, there is a global
`Dict{Symbol, Face}` that allows for [`Face`](@ref StyledStrings.Face)s to be
referred to simply by name. Packages can add faces to this dictionary via the
[`addface!`](@ref StyledStrings.addface!) function, and the loaded faces can be
easily [customized](@ref stdlib-styledstrings-face-toml).

!!! warning "Appropriate face naming"
    Any package registering new faces should ensure that they are prefixed
    by the package name, i.e. follow the format `mypackage_myface`.
    This is important for predictability, and to prevent name clashes.

    Furthermore, packages should take care to use (and introduce) *semantic*
    faces (like `code`) over direct colours and styles (like `cyan`). This is helpful
    in a number of ways, from making the intent in usage more obvious, aiding
    composability, and making user customisation more intuitive.

There are two set of exemptions to the package-prefix rule:
- the set of basic faces that are part of the default value of the faces dictionary
- faces introduced by Julia's own standard library, namely `JuliaSyntaxHighlighting`

#### [Basic faces](@id stdlib-styledstrings-basic-faces)

Basic faces are intended to represent a general idea that is widely applicable.

For setting some text with a certain attribute, we have the `bold`, `light`,
`italic`, `underline`, `strikethrough`, and `inverse` faces.

There are also named faces for the 16 terminal colors: `black`, `red`, `green`,
`yellow`, `blue`, `magenta`, `cyan`, `white`, `bright_black`/`grey`/`gray`,
`bright_red`, `bright_green`, `bright_blue`, `bright_magenta`, `bright_cyan`,
and `bright_white`.

For shadowed text (i.e. dim but there) there is the `shadow` face. To indicate a
selected region, there is the `region` face. Similarly for emphasis and
highlighting the `emphasis` and `highlight` faces are defined. There is also
`code` for code-like text.

For visually indicating the severity of messages, the `error`, `warning`,
`success`, `info`, `note`, and `tip` faces are defined.

### [Customisation of faces (`Faces.toml`)](@id stdlib-styledstrings-face-toml)

It is good for the name faces in the global face dictionary to be customizable.
Theming and aesthetics are nice, and it is important for accessibility reasons
too. A TOML file can be parsed into a list of [`Face`](@ref StyledStrings.Face) specifications that
are merged with the pre-existing entry in the face dictionary.

A [`Face`](@ref StyledStrings.Face) is represented in TOML like so:

```toml
[facename]
attribute = "value"
...

[package.facename]
attribute = "value"
```

For example, if the `shadow` face is too hard to read it can be made brighter
like so:

```toml
[shadow]
foreground = "white"
```

On initialization, the `config/faces.toml` file under the first Julia depot (usually `~/.julia`) is loaded.

### Applying faces to a `AnnotatedString`

By convention, the `:face` attributes of a [`AnnotatedString`](@ref
Base.AnnotatedString) hold information on the [`Face`](@ref StyledStrings.Face)s
that currently apply. This can be given in multiple forms, as a single `Symbol`
naming a [`Face`](@ref StyledStrings.Face)s in the global face dictionary, a
[`Face`](@ref StyledStrings.Face) itself, or a vector of either.

The `show(::IO, ::MIME"text/plain", ::AnnotatedString)` and `show(::IO,
::MIME"text/html", ::AnnotatedString)` methods both look at the `:face` attributes
and merge them all together when determining the overall styling.

We can supply `:face` attributes to a `AnnotatedString` during construction, add
them to the properties list afterwards, or use the convenient [Styled String
literals](@ref stdlib-styledstring-literals).

```@repl demo
str1 = AnnotatedString("blue text", [(1:9, :face, :blue)])
str2 = styled"{blue:blue text}"
str1 == str2
sprint(print, str1, context = :color => true)
sprint(show, MIME("text/html"), str1, context = :color => true)
```

## [Styled String Literals](@id stdlib-styledstring-literals)

To ease construction of [`AnnotatedString`](@ref Base.AnnotatedString)s with [`Face`](@ref StyledStrings.Face)s applied,
the [`styled"..."`](@ref @styled_str) styled string literal allows for the content and
attributes to be easily expressed together via a custom grammar.

Within a [`styled"..."`](@ref @styled_str) literal, curly braces are considered
special characters and must be escaped in normal usage (`\{`, `\}`). This allows
them to be used to express annotations with (nestable) `{annotations...:text}`
constructs.

The `annotations...` component is a comma-separated list of three types of annotations.
- Face names
- Inline `Face` expressions `(key=val,...)`
- `key=value` pairs

Interpolation is possible everywhere except for inline face keys.

For more information on the grammar, see the extended help of the
[`styled"..."`](@ref @styled_str) docstring.

As an example, we can demonstrate the list of built-in faces mentioned above like so:

```julia-repl
julia> println(styled"
The basic font-style attributes are {bold:bold}, {light:light}, {italic:italic},
{underline:underline}, and {strikethrough:strikethrough}.

In terms of color, we have named faces for the 16 standard terminal colors:
 {black:■} {red:■} {green:■} {yellow:■} {blue:■} {magenta:■} {cyan:■} {white:■}
 {bright_black:■} {bright_red:■} {bright_green:■} {bright_yellow:■} {bright_blue:■} {bright_magenta:■} {bright_cyan:■} {bright_white:■}

Since {code:bright_black} is effectively grey, we define two aliases for it:
{code:grey} and {code:gray} to allow for regional spelling differences.

To flip the foreground and background colors of some text, you can use the
{code:inverse} face, for example: {magenta:some {inverse:inverse} text}.

The intent-based basic faces are {shadow:shadow} (for dim but visible text),
{region:region} for selections, {emphasis:emphasis}, and {highlight:highlight}.
As above, {code:code} is used for code-like text.

Lastly, we have the 'message severity' faces: {error:error}, {warning:warning},
{success:success}, {info:info}, {note:note}, and {tip:tip}.

Remember that all these faces (and any user or package-defined ones) can
arbitrarily nest and overlap, {region,tip:like {bold,italic:so}}.")
```

```@raw comment
Documenter doesn't properly represent all the styling above, so I've converted it manually to HTML and LaTeX.
```

```@raw html
<pre>
 The basic font-style attributes are <span style="font-weight: 700;">bold</span>, <span style="font-weight: 300;">light</span>, <span style="font-style: italic;">italic</span>,
 <span style="text-decoration: underline;">underline</span>, and <span style="text-decoration: line-through">strikethrough</span>.

 In terms of color, we have named faces for the 16 standard terminal colors:
  <span style="color: #1c1a23;">■</span> <span style="color: #a51c2c;">■</span> <span style="color: #25a268;">■</span> <span style="color: #e5a509;">■</span> <span style="color: #195eb3;">■</span> <span style="color: #803d9b;">■</span> <span style="color: #0097a7;">■</span> <span style="color: #dddcd9;">■</span>
  <span style="color: #76757a;">■</span> <span style="color: #ed333b;">■</span> <span style="color: #33d079;">■</span> <span style="color: #f6d22c;">■</span> <span style="color: #3583e4;">■</span> <span style="color: #bf60ca;">■</span> <span style="color: #26c6da;">■</span> <span style="color: #f6f5f4;">■</span>

 Since <span style="color: #0097a7;">bright_black</span> is effectively grey, we define two aliases for it:
 <span style="color: #0097a7;">grey</span> and <span style="color: #0097a7;">gray</span> to allow for regional spelling differences.

 To flip the foreground and background colors of some text, you can use the
 <span style="color: #0097a7;">inverse</span> face, for example: <span style="color: #803d9b;">some </span><span style="background-color: #803d9b;">inverse</span><span style="color: #803d9b;"> text</span>.

 The intent-based basic faces are <span style="color: #76757a;">shadow</span> (for dim but visible text),
 <span style="background-color: #3a3a3a;">region</span> for selections, <span style="color: #195eb3;">emphasis</span>, and <span style="background-color: #195eb3;">highlight</span>.
 As above, <span style="color: #0097a7;">code</span> is used for code-like text.

 Lastly, we have the 'message severity' faces: <span style="color: #ed333b;">error</span>, <span style="color: #e5a509;">warning</span>,
 <span style="color: #25a268;">success</span>, <span style="color: #26c6da;">info</span>, <span style="color: #76757a;">note</span>, and <span style="color: #33d079;">tip</span>.

 Remember that all these faces (and any user or package-defined ones) can
 arbitrarily nest and overlap, <span style="color: #33d079;background-color: #3a3a3a;">like <span style="font-weight: 700;font-style: italic;">so</span></span>.</pre>
```

```@raw latex
\begingroup
\ttfamily
\setlength{\parindent}{0pt}
\setlength{\parskip}{\baselineskip}

The basic font-style attributes are {\fontseries{b}\selectfont bold}, {\fontseries{l}\selectfont light}, {\fontshape{it}\selectfont italic},\\
\underline{underline}, and {strikethrough}.

In terms of color, we have named faces for the 16 standard terminal colors:\\
{\color[HTML]{1c1a23}\(\blacksquare\)} {\color[HTML]{a51c2c}\(\blacksquare\)} {\color[HTML]{25a268}\(\blacksquare\)}
{\color[HTML]{e5a509}\(\blacksquare\)} {\color[HTML]{195eb3}\(\blacksquare\)} {\color[HTML]{803d9b}\(\blacksquare\)}
{\color[HTML]{0097a7}\(\blacksquare\)} {\color[HTML]{dddcd9}\(\blacksquare\)} \\
{\color[HTML]{76757a}\(\blacksquare\)} {\color[HTML]{ed333b}\(\blacksquare\)} {\color[HTML]{33d079}\(\blacksquare\)} {\color[HTML]{f6d22c}\(\blacksquare\)} {\color[HTML]{3583e4}\(\blacksquare\)} {\color[HTML]{bf60ca}\(\blacksquare\)} {\color[HTML]{26c6da}\(\blacksquare\)} {\color[HTML]{f6f5f4}\(\blacksquare\)}

Since {\color[HTML]{0097a7}bright\_black} is effectively grey, we define two aliases for it:\\
{\color[HTML]{0097a7}grey} and {\color[HTML]{0097a7}gray} to allow for regional spelling differences.

To flip the foreground and background colors of some text, you can use the\\
{\color[HTML]{0097a7}inverse} face, for example: {\color[HTML]{803d9b}some \colorbox[HTML]{803d9b}{\color[HTML]{000000}inverse} text}.

The intent-based basic faces are {\color[HTML]{76757a}shadow} (for dim but visible text),\\
\colorbox[HTML]{3a3a3a}{region} for selections, {\color[HTML]{195eb3}emphasis}, and \colorbox[HTML]{195eb3}{highlight}.\\
As above, {\color[HTML]{0097a7}code} is used for code-like text.

Lastly, we have the 'message severity' faces: {\color[HTML]{ed333b}error}, {\color[HTML]{e5a509}warning},\\
{\color[HTML]{25a268}success}, {\color[HTML]{26c6da}info}, {\color[HTML]{76757a}note}, and {\color[HTML]{33d079}tip}.

Remember that all these faces (and any user or package-defined ones) can\\
arbitrarily nest and overlap, \colorbox[HTML]{3a3a3a}{\color[HTML]{33d079}like
  {\fontseries{b}\fontshape{it}\selectfont so}}.
\endgroup
```

## [API reference](@id stdlib-styledstrings-api)


### Styling and Faces

```@docs
StyledStrings.@styled_str
StyledStrings.styled
StyledStrings.Face
StyledStrings.addface!
StyledStrings.withfaces
StyledStrings.SimpleColor
StyledStrings.parse(::Type{StyledStrings.SimpleColor}, ::String)
StyledStrings.tryparse(::Type{StyledStrings.SimpleColor}, ::String)
StyledStrings.merge(::StyledStrings.Face, ::StyledStrings.Face)
```
