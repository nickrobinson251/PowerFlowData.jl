using DataFrames
using PowerFlowData
using Tables
using Test

@testset "PowerFlowData.jl" begin
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
