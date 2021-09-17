using DataFrames
using PowerFlowData
using Tables
using Test

@testset "PowerFlowData.jl" begin

    @testset "Tables interface" begin
        struct TestRecs <: Records
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

        for T in (Buses, Loads, Generators)
            @test T <: Records
            @test Tables.istable(T)
        end
    end

    @testset "v30 file" begin
        net1 = parse_network("testfiles/synthetic_data_v30.raw")
        @test net1 isa Network

        caseid = net1.caseid
        @test caseid.ic == 0
        @test caseid.sbase == 100.0

        buses = net1.buses
        @test buses.i == [111, 112, 113]

        loads = net1.loads
        @test loads.i == [111, 113]

        gens = net1.generators
        @test gens.i == [111, -112, 113]
    end

    @testset "v29 file" begin
        net2 = parse_network("testfiles/synthetic_data_v29.raw")

        caseid = net2.caseid
        @test caseid.ic == 0
        @test caseid.sbase == 100.0

        buses = net2.buses
        @test buses.i == [1, 222222]
        @test buses.owner == [1, 7]

        loads = net2.loads
        @test loads.i == [1, 222222]
        @test loads.owner == [1, 7]

        gens = net2.generators
        @test gens.i == [104]
    end
end
