# Internals

Everything documented in this page is internal and subject to breaking changes,
even in minor version updates of Julia or StyledStrings.jl. If you are curious
about the internals, read on, but if you want to depend on them, please consider
opening a pull request or issue to discuss making them part of the public API.

```@docs
StyledStrings.ANSI_4BIT_COLORS
StyledStrings.FACES
StyledStrings.HTML_BASIC_COLORS
StyledStrings.Legacy.ANSI_256_COLORS
StyledStrings.Legacy.NAMED_COLORS
StyledStrings.Legacy.RENAMED_COLORS
StyledStrings.Legacy.legacy_color
StyledStrings.Legacy.load_env_colors!
StyledStrings.ansi_4bit_color_code
StyledStrings.eachregion
StyledStrings.face!
StyledStrings.getface
StyledStrings.loadface!
StyledStrings.loaduserfaces!
StyledStrings.resetfaces!
StyledStrings.termcolor
StyledStrings.termcolor24bit
StyledStrings.termcolor8bit
```
