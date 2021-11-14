# Alternatives

## Julia

I am are aware of two other open-source packages with functionality to parse PSS/E files:
  - [PowerModels.jl](https://lanl-ansi.github.io/PowerModels.jl/stable/parser/#PTI-Data-Files-(PSS/E))
  - [PowerSystems.jl](https://nrel-siip.github.io/PowerSystems.jl/stable/modeler_guide/generated_parsing/)

I have not used either so cannot recommend one over the other.
From what I can see, these parsers are almost identical to each other.
It seems PowerSystems.jl originally vendored the PowerModels.jl code, but the parsers may have diverged slightly over time.

Both of these packages only support parsing v33 PSS/E files, and only parse a subset of the data categories in the file.
Whereas PowerFlowData.jl (this package) can parse all data categories from both v30 and v33 PSS/E files.

Importantly, these alternatives also take a completely different approach to this package.
These other parsers read the `.raw` files as a `String` (e.g. using `readlines`), then operate on string data, and parse strings into other Julia types as necessary.
PowerFlowData.jl reads the `.raw` files as a bytes buffer (`Vector{UInt8}`), then parses the bytes directly into Julia types.
This is much faster and more memory efficient.

This package was originally developed as a fun exercise, but should correctly implement the full data format specification.
Please feel encouraged to give this package a try, and open issues if you encounter any
problems or missing features.

## Others

In Python, there is a package named [`grg-pssedata`](https://github.com/lanl-ansi/grg-pssedata), which says it parses PSSE v33 data files.

In Matlab, the [`MATPOWER`](https://github.com/MATPOWER/matpower/) package says it has parsing functionality for (unspecified) PSSE data files.
