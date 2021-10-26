using DataFrames
using InteractiveUtils: subtypes
using InlineStrings: InlineStrings  # for `eval(repr(...))`
using PowerFlowData
using Tables
using Test

@testset "PowerFlowData.jl" begin

    @testset "Infers" begin
        for T in subtypes(PowerFlowData.Records)
            @inferred T()
            @inferred T(1)
        end
    end

    @testset "Tables interface" begin
        struct TestRecs <: PowerFlowData.Records
            x1::Vector{Int}
            yy::Vector{Float64}
        end
        recs = TestRecs([1, 2, 3], [3.14, 1.72, 0.05])

        @test Tables.istable(TestRecs)

        @test Tables.columnaccess(TestRecs)
        @test Tables.columns(recs) === recs
        @test Tables.columnnames(recs) == (:x1, :yy)

        @test recs.x1 == [1, 2, 3]
        @test Tables.getcolumn(recs, :x1) == [1, 2, 3]
        @test Tables.getcolumn(recs, 1) == [1, 2, 3]

        @test length(recs) == 3
        @test size(recs) == (3, 2)

        df = DataFrame(recs)
        @test df isa DataFrame
        @test size(df) == (3, 2)

        for T in (
            Buses, Loads, Generators, Branches, Transformers, AreaInterchanges,
            TwoTerminalDCLines, VSCDCLines, SwitchedShunts, ImpedanceCorrections,
            MultiTerminalDCLines, MultiSectionLineGroups, Zones, InterAreaTransfers,
            Owners, FACTSDevices
        )
            @test T <: PowerFlowData.Records
            @test Tables.istable(T)
        end

        caseid = CaseID(0, 100.0)
        @test caseid[1] == caseid[:ic]
        @test caseid[2] == caseid[:sbase]
        @test NamedTuple(caseid) == (ic=0, sbase=100.0)
    end

    @testset "show" begin
        # test of file which has MultiTerminalDCLines as they have their own `show`.
        net = parse_network("testfiles/synthetic_data_v29.raw")

        # CaseID should have a parseable `repr`; `AbstractRows` don't get this for free.
        @test repr(net.caseid) == "CaseID(ic = 0, sbase = 100.0)"
        @test eval(Meta.parse(repr(net.caseid))) == CaseID(0, 100.0)

        mime = MIME("text/plain")
        context = :compact => true
        @test repr(mime, net; context) == "Network"
        @test repr(mime, net) == strip(
            """
            Network with 17 data categories:
             $(sprint(show, mime, net.caseid))
             $(sprint(show, mime, net.buses; context))
             $(sprint(show, mime, net.loads; context))
             $(sprint(show, mime, net.generators; context))
             $(sprint(show, mime, net.branches; context))
             $(sprint(show, mime, net.transformers; context))
             $(sprint(show, mime, net.interchanges; context))
             $(sprint(show, mime, net.two_terminal_dc; context))
             $(sprint(show, mime, net.vsc_dc; context))
             $(sprint(show, mime, net.switched_shunts; context))
             $(sprint(show, mime, net.impedance_corrections; context))
             $(sprint(show, mime, net.multi_terminal_dc; context))
             $(sprint(show, mime, net.multi_section_lines; context))
             $(sprint(show, mime, net.zones; context))
             $(sprint(show, mime, net.area_transfers; context))
             $(sprint(show, mime, net.owners; context))
             $(sprint(show, mime, net.facts; context))
            """
        )
        @test repr(mime, net.caseid) == "CaseID: (ic = 0, sbase = 100.0)"

        @test repr(mime, net.buses; context=(:compact => true)) == "Buses with 2 records"
        @test startswith(repr(mime, net.buses), "Buses with 2 records, 11 columns:\n──")

        mt_dc_line = net.multi_terminal_dc.lines[1]
        @test eval(Meta.parse(repr(mt_dc_line))) isa MultiTerminalDCLine
        @test repr(mime, mt_dc_line) == strip(
            """
            $(sprint(show, mime, mt_dc_line.line))
            $(sprint(show, mime, mt_dc_line.converters))
            $(sprint(show, mime, mt_dc_line.buses))
            $(sprint(show, mime, mt_dc_line.links))
            """
        )
        dc_line = mt_dc_line.line
        @test eval(Meta.parse(repr(dc_line))) isa DCLineID
    end

    @testset "v30 file" begin
        net1 = parse_network("testfiles/synthetic_data_v30.raw")
        @test net1 isa Network

        caseid = net1.caseid
        @test caseid.ic == 0
        @test caseid.sbase == 100.0

        # Test first and last columns are parsed as expected
        buses = net1.buses
        @test buses.i == [111, 112, 113]
        @test buses.owner == [1, 2, 2]

        # Test string column as expected
        @test buses.name[2] == "D2JK  "

        loads = net1.loads
        @test loads.i == [111, 113]
        @test loads.owner == [1, 2]

        gens = net1.generators
        @test gens.i == [111, -112, 113]
        @test gens.fi == [1.0, 1.0, 1.0]
        @test gens.id[1] == "ST"

        branches = net1.branches
        @test branches.i == [111, 111, 112]
        @test branches.j == [112, -113, 113]  # negative numbers should be allowed
        @test branches.fi == [1.0, 1.0, 1.0]
        @test branches.ckt[1] == "3 "

        transformers = net1.transformers
        @test transformers.i == [112, 113]                     #  1st entry of 1st row
        @test transformers.fi == [1.0, 1.0]                    # last entry of 1st row
        @test transformers.r1_2 == [0.032470, 0.039570]        #  1st entry of 2nd row
        @test transformers.sbase1_2 == [200.0, 200.0]          # last entry of 2nd row (T2)
        @test isequal(transformers.anstar, [missing, 1.01893]) # last entry of 2nd row (T3)
        @test transformers.windv1 == [1.0, 1.0]                #  1st entry of 3rd row
        @test transformers.cx1 == [0.0, 0.0]                   # last entry of 3rd row
        @test transformers.windv2 == [1.0, 1.0]                #  1st entry of 4th row
        @test transformers.nomv2 == [169.0, 169.0]             # last entry of 4th row (T2)
        @test isequal(transformers.cx2, [missing, 0.0])        # last entry of 4th row (T3)
        @test isequal(transformers.windv3, [missing, 13.8])    #  1st entry of 5th row
        @test isequal(transformers.cx3, [missing, 0.0])        # last entry of 5th row
        @test transformers.ckt[1] == "G1"                      # string col

        # v30 testfile has both 2-winding and 3-winding data, so should return all columns
        @test length(Tables.columnnames(transformers)) == fieldcount(Transformers)
        @test size(DataFrame(transformers)) == (2, fieldcount(Transformers))
        @test size(transformers) == (2, fieldcount(Transformers))
        @test length(transformers) == 2

        interchanges = net1.interchanges
        @test interchanges.i == [113]
        @test interchanges.isw == [456]
        @test interchanges.pdes == [2121.7211]
        @test interchanges.ptol == [6.0]
        @test interchanges.arname == ["ABC     "]

        two_terminal_dc = net1.two_terminal_dc
        @test two_terminal_dc.i == [11]           #  1st entry of 1st row
        @test two_terminal_dc.cccacc == [1.0]     # last entry of 1st row
        @test two_terminal_dc.ipr == [112]        #  1st entry of 2nd row
        @test two_terminal_dc.xcapr == [0.0]      # last entry of 2nd row
        @test two_terminal_dc.ipi == [2222]       #  1st entry of 3nd row
        @test two_terminal_dc.xcapi == [2.0]      # last entry of 3rd row

        vsc_dc = net1.vsc_dc
        @test vsc_dc.name == ["line 1 "]   #  1st entry of 1st row
        @test vsc_dc.f4 == [1.0]           # last entry of 1st row
        @test vsc_dc.ibus1 == [1117]       #  1st entry of 2nd row
        @test vsc_dc.rmpct1 == [100.0]     # last entry of 2nd row
        @test vsc_dc.ibus2 == [114]        #  1st entry of 3nd row
        @test vsc_dc.rmpct2 == [100.0]     # last entry of 3nd row

        switched_shunts = net1.switched_shunts
        @test switched_shunts.i == [113]   # first col
        @test switched_shunts.n1 == [1]    # `n1` always present
        @test switched_shunts.b1 == [26.0] # `b1` always present
        @test switched_shunts.n8 == [0]    # `n8` not present; should default to zero
        @test switched_shunts.b8 == [0.0]  # last col; `b8` not present; default to zero

        impedance_corrections = net1.impedance_corrections
        @test impedance_corrections.i == [1]       # first col
        @test impedance_corrections.f11 == [0.0]   # last col; `f11` not present; default to zero

        multi_terminal_dc = net1.multi_terminal_dc
        @test isempty(multi_terminal_dc)

        multi_section_lines = net1.multi_section_lines
        @test multi_section_lines.i == [1, 114]                     # first col
        @test multi_section_lines.id == ["&1", "&2"]                # string col
        @test isequal(multi_section_lines.dum2, [missing, 5])       # `dum2` present only for 1 row of the data
        @test isequal(multi_section_lines.dum9, [missing, missing]) # last col; not present; default to missing

        zones = net1.zones
        @test zones.i == [117, 127, 227]
        @test zones.zoname == ["ABC ", "CDEF", " CDEG "]

        area_transfers = net1.area_transfers
        @test area_transfers.arfrom == [1, 1]
        @test area_transfers.ptran == [10.0, -20.0]

        owners = net1.owners
        @test owners.i == [1, 2]
        @test owners.owname == ["      ABC   ", "      CDE  "]

        facts = net1.facts
        @test facts.n == [1]
        @test facts.vsref == [0]
    end

    @testset "v29 file" begin
        net2 = parse_network("testfiles/synthetic_data_v29.raw")

        caseid = net2.caseid
        @test caseid.ic == 0
        @test caseid.sbase == 100.0

        # Test first and last columns are parsed as expected
        buses = net2.buses
        @test buses.i == [1, 222222]
        @test buses.owner == [1, 7]

        # Test string column as expected
        @test buses.name[2] == "PRPR C D    "

        loads = net2.loads
        @test loads.i == [1, 222222]
        @test loads.owner == [1, 7]

        gens = net2.generators
        @test gens.i == [104]    # first col
        @test gens.fi == [1.0]   # last col
        @test gens.id[1] == "1 " # string col

        branches = net2.branches
        @test branches.i == [1, 2, 222222]
        @test branches.j == [-543210, 9, 333333]  # negative numbers should be allowed
        @test branches.fi == [1.0, 1.0, 1.0]
        @test branches.ckt[2] == "6 "

        transformers = net2.transformers
        @test length(transformers.i) == 3
        @test transformers.i == [42, 4774, 222222]            #  1st entry of 1st row
        @test transformers.fi == [1.0, 1.0, 1.0]              # last entry of 1st row
        @test transformers.r1_2 == [0.0025, 0.0, 0.0005]      #  1st entry of 2nd row
        @test transformers.sbase1_2 == [100.0, 100.0, 100.0]  # last entry of 2nd row
        @test transformers.windv1 == [1.0, 1.0462, 1.0]       #  1st entry of 3rd row
        @test transformers.cx1 == [0.0, 0.0, 0.0]             # last entry of 3rd row
        # Important to test a row where the 1st character is '0', to get it does not
        # get misinterpreted as the start of a "0 bus" records terminating the section.
        @test transformers.windv2 == [1.0, 1.045, 0.98250]    #  1st entry of 4th row
        @test transformers.nomv2 == [138.0, 240.35, 345.0]    # last entry of 4th row
        @test transformers.ckt == ["K1", "90", "B1"]          # string col

        # v29 testfile has only 2-winding data, so should return only 2-winding columns
        @test length(Tables.columnnames(transformers)) == 35
        @test size(DataFrame(transformers)) == (3, 35)
        @test size(transformers) == (3, 35)
        @test length(transformers) == 3

        interchanges = net2.interchanges
        @test interchanges.i == [615, 762]
        @test interchanges.isw == [615001, 1234]
        @test interchanges.pdes == [32.677, -224.384]
        @test interchanges.ptol == [5.0, 5.0]
        @test interchanges.arname == ["RE          ", "OTP         "]

        two_terminal_dc = net2.two_terminal_dc
        @test isempty(two_terminal_dc)

        vsc_dc = net2.vsc_dc
        @test isempty(vsc_dc)

        switched_shunts = net2.switched_shunts
        @test switched_shunts.i == [175, 177]      # first col
        @test switched_shunts.n1 == [2, 1]         # `n1` col present for both
        @test switched_shunts.b1 == [19.76, 15.60] # `b1` col present for both
        @test switched_shunts.n2 == [0, 1]         # `n2` col present only for second entry
        @test switched_shunts.b2 == [0.0, 17.69]   # `b2` col present only for second entry
        @test switched_shunts.n8 == [0, 0]         # `n8` col not present for either entry
        @test switched_shunts.b8 == [0.0, 0.0]     # last col; `b8` col not present for either entry

        impedance_corrections = net2.impedance_corrections
        @test impedance_corrections.i == [1, 2]          # first col
        @test impedance_corrections.f11 == [3.34, 1.129] # last col

        multi_terminal_dc = net2.multi_terminal_dc
        @test length(multi_terminal_dc) == 1

        mt_dc = only(multi_terminal_dc.lines)
        dc_line = mt_dc.line
        @test dc_line.i == 1     # first val
        @test dc_line.vconvn == 0 # last val

        converters = mt_dc.converters
        @test length(converters) == dc_line.nconv == 4
        @test converters.ib == [402, 401, 212, 213]
        @test converters.cnvcod == [3, 3, 1, 4]

        dc_buses = mt_dc.buses
        @test length(dc_buses) == dc_line.ndcbs == 5
        @test dc_buses.idc == [1, 2, 3, 4, 5]
        @test dc_buses.owner == [4, 2, 4, 2, 4]

        dc_links = mt_dc.links
        @test length(dc_links) == dc_line.ndcln == 4
        @test dc_links.idc == [1, 2, 3, 4]
        @test dc_links.ldc == [0.0, 0.0, 0.0, 0.0]

        multi_section_lines = net2.multi_section_lines
        @test isempty(multi_section_lines)

        zones = net2.zones
        @test zones.i == [1, 9]
        @test zones.zoname == ["ABL         ", "EFGN        "]

        area_transfers = net2.area_transfers
        @test isempty(area_transfers)

        owners = net2.owners
        @test owners.i == [1, 2]
        @test owners.owname == ["      EAI   ", "      OUYK  "]

        facts = net2.facts
        @test isempty(facts)
    end

    @testset "issues" begin
        net = parse_network("testfiles/spacezero.raw")
        @test length(net.buses) == 2
        @test length(net.loads) == 1
    end
end
