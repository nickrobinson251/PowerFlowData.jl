using PowerFlowData
using Documenter

DocMeta.setdocmeta!(PowerFlowData, :DocTestSetup, :(using PowerFlowData); recursive=true)

makedocs(;
    modules=[PowerFlowData],
    authors="Nick Robinson <npr251@gmail.com> and contributors",
    repo="https://github.com/nickrobinson251/PowerFlowData.jl/blob/{commit}{path}#{line}",
    sitename="PowerFlowData.jl",
    format=Documenter.HTML(
        canonical="https://nickrobinson251.github.io/PowerFlowData.jl",
        prettyurls=false,
    ),
    pages=[
        "Home" => "index.md",
        "API" => "api.md",
        "Alternatives" => "alternatives.md",
        "Implementation" => "implementation.md",
    ],
    strict=true,
    checkdocs=:exports,
)

deploydocs(;
    repo="github.com/nickrobinson251/PowerFlowData.jl",
    devbranch="main",
    push_preview=true,
)
