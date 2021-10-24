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
