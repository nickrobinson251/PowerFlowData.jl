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

The aim is to capture the domain knowledge / format specification in the types,
and keep the parsing code as minimal as possible (relying on the types for the domain knowledge).
When trying to support multiple versions of the format, the rough strategy is:
* if a wholly new data category exists in the new format, create a new `Records` subtype
* if a data category has added a new column to the end of a record, add a new `Union{T, Missing}` field to the existing `Records` subtype
* if a data category has added/removed columns in the middle of a records, create a new `Records` subtype with a common support type as the existing `Records` subtype for the category
  (e.g. `Buses30 <: Buses`, `Buses33 <: Buses`, `Buses <: Records`)
* update the top-level `parse_network` function is updated to parse the records in the order expected by the version of the format
  (using the version number extracted as part of the Case ID data).
