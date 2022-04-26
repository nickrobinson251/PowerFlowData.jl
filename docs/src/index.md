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

You can specify what version of the PSS/E data format the file is in using the `v` keyword,
like:
```julia
parse_network("file.raw"; v=33)
```
Alternatively, the version of the data format will automatically be determined when parsing.
Versions 30 and 33 of the format are currently supported.

You can specify which character separates values in the file using the `delim` keyword,
like:
```julia
parse_network("file.raw"; delim=' ')
```
If not specified, the delimiter will automatically be detected when parsing.
Comma delimited files `delim=','` and space delimited files `delim=' '` are currently supported.

## Example

Usually your data will be in a file, and you'd read it with `parse_network("file.raw")`,
but here we'll pass in the data directly, to show how it matches to the output:

```@repl
using PowerFlowData, DataFrames
data = IOBuffer("""
    0,   100.00          / PSS/E-30.3    WED, SEP 15 2021  21:04
    SE SNAPSHOT 15-09-2021 PEAK CASE 18:00
    FULL COPY OF SYNTHETIC
         1,'AAA    3    ', 111.0000,4,     0.000,     0.000, 327,   1,0.00000,   0.0000,   1
    222222,'PRPR C D    ',  42.0000,1,     0.000,     0.000, 694,  24,1.11117,  20.0606,   7
    0 / END OF BUS DATA, BEGIN LOAD DATA
    """
);
network = parse_network(data; v=30);
NamedTuple(network.caseid)  # Case Identification data is a single row.
DataFrame(network.buses)    # Bus data, and all other data, is a table.
```
