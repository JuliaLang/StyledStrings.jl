using StyledStrings
using Documenter

DocMeta.setdocmeta!(StyledStrings, :DocTestSetup, :(using StyledStrings); recursive=true)

makedocs(;
    modules = [StyledStrings],
    sitename = "Styled Strings",
    authors = "tecosaur <contact@tecosaur.net> and contributors",
    repo = "https://github.com/JuliaLang/StyledStrings.jl/blob/{commit}{path}#{line}",
    format = Documenter.HTML(),
    pages = [
        "StyledStrings" => "index.md",
        "Internals" => "internals.md",
    ],
    warnonly = [:cross_references],
)

deploydocs(repo="github.com/JuliaLang/StyledStrings.jl")
