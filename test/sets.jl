@testset "Sets constant" begin
    @test MOIU.getconstant(MOI.EqualTo(3)) == 3
    @test MOIU.getconstant(MOI.GreaterThan(6)) == 6
    @test MOIU.getconstant(MOI.LessThan(2)) == 2
end
@testset "Sets copy" begin
    @test deepcopy(MOI.EqualTo(3)).value == 3
    w = [2.3, 1.2]
    s1 = deepcopy(MOI.SOS1(w))
    s2 = deepcopy(MOI.SOS2(w))
    push!(w, 3.1)
    @test s1.weights == [2.3, 1.2]
    @test s2.weights == [2.3, 1.2]
end
