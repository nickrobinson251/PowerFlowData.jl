using DataFrames
using InteractiveUtils: subtypes
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
            Buses, Loads, Generators, Branches, Transformers, AreaInterchanges, VSCDCLines,
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
        net = parse_network("testfiles/synthetic_data_v30.raw")

        # CaseID should have a parseable `repr`; `AbstractRows` don't get this for free.
        @test repr(net.caseid) == "CaseID(ic = 0, sbase = 100.0)"
        @test eval(Meta.parse(repr(net.caseid))) == CaseID(0, 100.0)

        mime = MIME("text/plain")
        context = :compact => true
        @test repr(mime, net; context) == "Network"
        @test repr(mime, net) == strip(
            """
            Network with 9 data categories:
             $(sprint(show, mime, net.caseid))
             $(sprint(show, mime, net.buses; context))
             $(sprint(show, mime, net.loads; context))
             $(sprint(show, mime, net.generators; context))
             $(sprint(show, mime, net.branches; context))
             $(sprint(show, mime, net.transformers; context))
             $(sprint(show, mime, net.interchanges; context))
             $(sprint(show, mime, net.two_terminal_dc; context))
             $(sprint(show, mime, net.vsc_dc; context))
            """
        )
        @test repr(mime, net.caseid) == "CaseID: (ic = 0, sbase = 100.0)"

        @test repr(mime, net.buses; context=(:compact => true)) == "Buses with 3 records"
        @test startswith(repr(mime, net.buses), "Buses with 3 records, 11 columns:\n──")
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
    end
end
