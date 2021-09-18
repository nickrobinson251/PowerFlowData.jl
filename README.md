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
This will return a `Network` object, which contains the data parsed into dedicated structures matching the PSS/E-format specification.

**[Documentation](https://nickrobinson251.github.io/PowerFlowData.jl/)**

### Stability

Currently this is a work-in-progress package being developed for fun… 'cos you gotta have hobbies.
It is not stable.
It is not yet very robust.
The format specification is based on old [PSS/E](https://en.wikipedia.org/wiki/Power_system_simulator_for_engineering) user-manuals and example files I could find online.
There are likely more performance improvements to be made too.
