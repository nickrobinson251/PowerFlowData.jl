# Implementation details

This page has rough notes about the how the internals of PowerFlowData.jl.

We use [Parsers.jl](https://github.com/JuliaData/Parsers.jl/) to parse bytes into Julia types.
Broadly speaking, we use `Parsers.Options` to configure the parsing based on the `.raw` format (e.g. `,` characters are delimiters), and then `Parsers.xparse` to actually parse the bytes between delimiters into the expected Julia types.
The expected Julia type depends on the category of data we are reading at that point in the file (buses, loads, …);
if the PSS/E user manual says "load records" should come after "bus records", and each load record should have 12 columns with the first column containing an integer "bus number", then we try to parse the first value in a load record as an `Int`, and so on.

The aim is to capture the domain knowledge / format specification in the types,
and keep the parsing code as minimal as possible (relying on the types for the domain knowledge).
When trying to support multiple versions of the format, the rough strategy is:
* if a wholly new data category exists in the new format, create a new `Records` subtype
* if a data category has added a new column to the end of a record, add a new `Union{T, Missing}` field to the existing `Records` subtype
* if a data category has added/removed columns in the middle of a record, or changed the element type of a column, create a new `Records` subtype with a common supertype as the existing `Records` subtype for the category
  (e.g. `Buses30 <: Buses`, `Buses33 <: Buses`, `Buses <: Records`)
* if a version-specific `Records` subtype is being used, update the top-level `parse_network` function to parse the records into the appropriate type, and in the order expected by the version of the format
  (using the version number extracted as part of the Case ID data).
