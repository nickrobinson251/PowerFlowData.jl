using PowerFlowData
using Documenter

DocMeta.setdocmeta!(PowerFlowData, :DocTestSetup, :(using PowerFlowData); recursive=true)

makedocs(;
    modules=[PowerFlowData],
    authors="Nick Robinson <npr251@gmail.com> and contributors",
    repo="https://github.com/nickrobinson251/PowerFlowData.jl/blob/{commit}{path}#{line}",
    sitename="PowerFlowData.jl",
    format=Documenter.HTML(canonical="https://nickrobinson251.github.io/PowerFlowData.jl"),
    pages=["Home" => "index.md" ],
    strict=true,
    checkdocs=:exports,
)

deploydocs(;
    repo="github.com/nickrobinson251/PowerFlowData.jl",
    push_preview=true,
)
