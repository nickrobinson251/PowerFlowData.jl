@setup_workload begin
    # Precompile using all test data files to ensure comprehensive coverage
    testfiles_dir = joinpath(pkgdir(@__MODULE__), "test", "testfiles")

    # List of all test files to precompile with
    test_files = [
        "synthetic_data_v30.raw",  # v30 format with all sections
        "synthetic_data_v33.RAW",  # v33 format with all sections
        "synthetic_data_v29.raw",  # v29/v30 format variant
        "spacedelim.raw",          # space-delimited format
        "spacezero.raw",           # edge case: space before zero
        "quotedzero.raw",          # edge case: quoted zero
    ]

    @compile_workload begin
        for filename in test_files
            filepath = joinpath(testfiles_dir, filename)
            if isfile(filepath)
                # Parse the network
                net = parse_network(filepath)

                # Access various fields to ensure type conversions are compiled
                length(net.buses)
                length(net.loads)
                length(net.generators)
                length(net.branches)
                length(net.transformers)
                length(net.area_interchanges)
                length(net.two_terminal_dc)
                length(net.vsc_dc)
                length(net.switched_shunts)
                length(net.impedance_corrections)
                length(net.multi_terminal_dc)
                length(net.multi_section_lines)
                length(net.zones)
                length(net.area_transfers)
                length(net.owners)
                length(net.facts)

                # Access some data fields to precompile field access
                if !isempty(net.buses)
                    net.buses.i[1]
                    net.buses.name[1]
                    # Access last element as well
                    net.buses.i[end]
                end

                if !isempty(net.loads)
                    net.loads.i[1]
                    net.loads.pl[1]
                end

                if !isempty(net.generators)
                    net.generators.i[1]
                    net.generators.pg[1]
                end

                if !isempty(net.branches)
                    net.branches.i[1]
                    net.branches.j[1]
                end

                if !isempty(net.transformers)
                    net.transformers.i[1]
                    net.transformers.j[1]
                end

                # Precompile fixed_shunts access (v33 only)
                if net.fixed_shunts !== nothing && !isempty(net.fixed_shunts)
                    net.fixed_shunts.i[1]
                    net.fixed_shunts.bl[1]
                end

                # Precompile multi-terminal DC access if present
                if !isempty(net.multi_terminal_dc)
                    mt_dc = net.multi_terminal_dc.lines[1]
                    length(mt_dc.converters)
                    length(mt_dc.buses)
                    length(mt_dc.links)
                end

                # Precompile Tables.jl interface
                Tables.istable(typeof(net.buses))
                Tables.columnnames(net.buses)
                Tables.columnnames(net.loads)
                Tables.columnnames(net.generators)
                Tables.columnnames(net.transformers)
                Tables.schema(net.buses)
                Tables.schema(net.loads)

                # Precompile getcolumn
                if !isempty(net.buses)
                    Tables.getcolumn(net.buses, :i)
                    Tables.getcolumn(net.buses, 1)
                end

                # Precompile show methods
                io = IOBuffer()
                show(io, MIME("text/plain"), net)
                show(io, MIME("text/plain"), net.buses)
                show(io, MIME("text/plain"), net.caseid)

                if !isempty(net.loads)
                    show(io, MIME("text/plain"), net.loads)
                end
                if !isempty(net.transformers)
                    show(io, MIME("text/plain"), net.transformers)
                end
            end
        end
    end
end
