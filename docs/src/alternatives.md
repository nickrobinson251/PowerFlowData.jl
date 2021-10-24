# Alternatives

In Julia, I am are aware of two other open-source packages with functionality to parse PSS/E files:
  - [PowerModels.jl](https://lanl-ansi.github.io/PowerModels.jl/stable/parser/#PTI-Data-Files-(PSS/E))
  - [PowerSystems.jl](https://nrel-siip.github.io/PowerSystems.jl/stable/modeler_guide/generated_parsing/)

I have not used either so cannot recommend one over the other.
From what I can see, these parsers are almost identical to each other.
It seems PowerSystems.jl originally vendored the PowerModels.jl code, but the parsers may have diverged slightly over time.

Importantly, these alternatives take a completely different approach to this package.
These other parsers read the `.raw` files as a `String` (e.g. using `readlines`), then operate on string data, and parse strings into other Julia types as necessary.

PowerFlowData.jl (this package) reads the `.raw` files as a bytes buffer (`Vector{UInt8}`), then parses the bytes directly into Julia types.

Hopefully this will be much faster and more memory efficient, but benchmarks pending.
For now, this package is being developed as a fun exercise.
Feel encouraged to give this package a try (and open issues!), but these alternative parsers are surely more battle-tested!

## Implementation details

We use [Parsers.jl](https://github.com/JuliaData/Parsers.jl/) to parse bytes into Julia types.
Broadly speaking, we use `Parsers.Options` to configure the parsing based on the `.raw` format (e.g. `,` characters are delimiters), and then `Parsers.xparse` to actually parse the bytes between delimiters into the expected Julia types.
The expected Julia type depends on the category of data we are reading at that point in the file (buses, loads, …);
if the PSS/E user manual says "load records" should come after "bus records", and each load record should have 12 columns with the first column containing an integer "bus number", then we try to parse the first value in a load record as an `Int`, and so on.
