using PowerFlowData
using Documenter

DocMeta.setdocmeta!(PowerFlowData, :DocTestSetup, :(using PowerFlowData); recursive=true)

makedocs(;
    modules=[PowerFlowData],
    authors="Nick Robinson <nicholas.robinson@invenialabs.co.uk> and contributors",
    repo="https://github.com/nickrobinson251/PowerFlowData.jl/blob/{commit}{path}#{line}",
    sitename="PowerFlowData.jl",
    format=Documenter.HTML(;
        prettyurls=get(ENV, "CI", "false") == "true",
        canonical="https://nickrobinson251.github.io/PowerFlowData.jl",
        assets=String[],
    ),
    pages=[
        "Home" => "index.md",
    ],
)

deploydocs(;
    repo="github.com/nickrobinson251/PowerFlowData.jl",
)
