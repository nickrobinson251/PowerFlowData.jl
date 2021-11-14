# PowerFlowData

[![Dev](https://img.shields.io/badge/docs-dev-blue.svg)](https://nickrobinson251.github.io/PowerFlowData.jl/dev)
[![Build Status](https://github.com/nickrobinson251/PowerFlowData.jl/workflows/CI/badge.svg)](https://github.com/nickrobinson251/PowerFlowData.jl/actions)
[![Coverage](https://codecov.io/gh/nickrobinson251/PowerFlowData.jl/branch/main/graph/badge.svg)](https://codecov.io/gh/nickrobinson251/PowerFlowData.jl)
[![Code Style: Blue](https://img.shields.io/badge/code%20style-blue-4495d1.svg)](https://github.com/invenia/BlueStyle)
[![ColPrac: Contributor Guide on Collaborative Practices for Community Packages](https://img.shields.io/badge/ColPrac-Contributor%20Guide-blueviolet)](https://github.com/SciML/ColPrac)

[PowerFlowData.jl](https://github.com/nickrobinson251/PowerFlowData.jl)
provides a parser for PSS/E-format `.raw` Power Flow Data Files.

To read a `.raw` file, use `parse_network`:
```julia
using PowerFlowData
parse_network("file.raw")
```
This will return a `Network` object, which contains the data parsed into dedicated structures matching the
[PSS/E](https://en.wikipedia.org/wiki/Power_system_simulator_for_engineering)-format specification.

**[Documentation](https://nickrobinson251.github.io/PowerFlowData.jl/dev)**

The format specification is based on old PSS/E user-manuals and example files I could find online.
Currently v30 and v33 of the format are supported.
Please open an issue if you run into any problems or missing features.
