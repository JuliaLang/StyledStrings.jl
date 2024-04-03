# This file is a part of Julia. License is MIT: https://julialang.org/license

# *Not* run as part of the Julia central stdlib testsuite

const IS_CI = lowercase(get(ENV, "CI", "no")) in ("true", "t", "1", "yes", "y")
const PARSER_FILE = joinpath("src", "styledmarkup.jl")
const PROJECT_DIR = dirname(@__DIR__)

function hasparserdiverged()
    upstream = try
        readchomp(pipeline(Cmd(`git rev-parse --abbrev-ref main@\{upstream\}`, dir=PROJECT_DIR), stderr=devnull))
    catch err
        if success(Cmd(`git show-ref --verify --quiet refs/remotes/origin/main`, dir=PROJECT_DIR))
            "origin/main"
        elseif err isa ProcessFailedException
            @warn "Upstream branch could not be detected, assuming no changes to parser"
            return false
        else
            rethrow()
        end
    end
    !success(Cmd(`git diff --quiet --diff-filter=M $upstream -- $PARSER_FILE`, dir=PROJECT_DIR))
end

using Supposition
include("fuzz.jl")
using .Fuzzer

function styfuzz()
    isstyled(s) = StyledStrings.styled(s) isa Base.AnnotatedString
    max_examples = if hasparserdiverged()
        ifelse(IS_CI, 5_000, 10_000)
    else
        1_000
    end
    @testset "Fuzz ($max_examples)" begin
        @check max_examples = max_examples isstyled(Fuzzer.content)
    end
end
