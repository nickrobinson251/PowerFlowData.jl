using DataFrames
using PowerFlowData
using Tables
using Test

@testset "PowerFlowData.jl" begin

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

        df = DataFrame(recs)
        @test df isa DataFrame
        @test size(df) == (3, 2)

        for T in (Buses, Loads, Generators, Branches)
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
        @test repr(mime, net) == strip(
            """
            Network with 5 data categories:
             $(sprint(show, mime, net.caseid))
             $(sprint(show, mime, net.buses; context))
             $(sprint(show, mime, net.loads; context))
             $(sprint(show, mime, net.generators; context))
             $(sprint(show, mime, net.branches; context))
            """
        )
        @test repr(mime, net.caseid) == "CaseID: (ic = 0, sbase = 100.0)"

        @test repr(mime, net.buses; context=(:compact => true)) == "Buses with 3 records"
        @test repr(mime, net.buses) == strip("""
            Buses with 3 records:
             i : [111, 112, 113]
            $(sprint(show, mime, Tables.schema(Buses); context=(:limit => true)))
            """
        )
        @test contains(repr(mime, net.branches), "i => j")  # custom branches "identifier"
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

    end
end
