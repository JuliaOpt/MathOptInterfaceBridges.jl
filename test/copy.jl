@testset "Copy test" begin
    @testset "Default copy" begin
        instance = Instance{Float64}()
        MOIT.failcopytestc(instance)
        MOIT.failcopytestia(instance)
        MOIT.failcopytestva(instance)
        MOIT.failcopytestca(instance)
        MOIT.copytest(instance, Instance{Float64}())
    end
    @testset "Allocate-Load copy" begin
        mock = MOIU.MockSolverInstance(Instance{Float64}())
        mock.needsallocateload = true
        MOIT.failcopytestc(mock)
        MOIT.failcopytestia(mock)
        MOIT.failcopytestva(mock)
        MOIT.failcopytestca(mock)
        MOIT.copytest(mock, Instance{Float64}())
    end
end
