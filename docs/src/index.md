```@meta
CurrentModule = PowerFlowData
```

# PowerFlowData

[PowerFlowData.jl](https://github.com/nickrobinson251/PowerFlowData.jl)
provides a parser for PSS/E-format `.raw` Power Flow Data Files.

To read a `.raw` file, use [`parse_network`](@ref):
```julia
parse_network("file.raw")
```
This will return a [`Network`](@ref) object, which contains the data parsed into dedicated structures matching the PSS/E-format specification.

## Example

Usually your data will be in a file, and you'd read it with `parse_network("file.raw")`,
but here we'll pass in the data directly, to show how it matches to the output:

```@repl
using PowerFlowData, DataFrames
data = IOBuffer("""
    0,   100.00          / PSS/E-29.3    WED, SEP 15 2021  21:04
    SE SNAPSHOT 15-09-2021 PEAK CASE 18:00
    FULL COPY OF SYNTHETIC
         1,'AAA    3    ', 111.0000,4,     0.000,     0.000, 327,   1,0.00000,   0.0000,   1
    222222,'PRPR C D    ',  42.0000,1,     0.000,     0.000, 694,  24,1.11117,  20.0606,   7
    0 / END OF BUS DATA, BEGIN LOAD DATA
    """
);
network = parse_network(data);
NamedTuple(network.caseid)  # Case Identification data is a single row.
DataFrame(network.buses)    # Bus data, and all other data, is a table.
```

## API

```@docs
parse_network
```

```@docs
Network
CaseID
Buses
Loads
Generators
Branches
Transformers
AreaInterchanges
TwoTerminalDCLines
VSCDCLines
SwitchedShunts
```

## Alternatives

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

### Implementation details

We use [Parsers.jl](https://github.com/JuliaData/Parsers.jl/) to parse bytes into Julia types.
Broadly speaking, we use `Parsers.Options` to configure the parsing based on the `.raw` format (e.g. `,` characters are delimiters), and then `Parsers.xparse` to actually parse the bytes between delimiters into the expected Julia types.
The expected Julia type depends on the category of data we are reading at that point in the file (buses, loads, …);
if the PSS/E user manual says "load records" should come after "bus records", and each load record should have 12 columns with the first column containing an integer "bus number", then we try to parse the first value in a load record as an `Int`, and so on.
