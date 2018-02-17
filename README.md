# MathOptInterfaceBridges

| **Build Status** |
|:----------------:|
| [![Build Status][build-img]][build-url] [![Build Status][winbuild-img]][winbuild-url] |
| [![Coveralls branch][coveralls-img]][coveralls-url] [![Codecov branch][codecov-img]][codecov-url] |

Constraint Bridges for packages which build on-top-of [MathOptInterface](https://github.com/JuliaOpt/MathOptInterface.jl) (such as [JuMP](https://github.com/JuliaOpt/JuMP.jl)).

For example, with the [`SplitIntervalBridge`](https://github.com/JuliaOpt/MathOptInterfaceBridges.jl/blob/master/src/intervalbridge.jl), one can create an optimizer layer bridging the constraints `ScalarAffineFunction`-in-`Interval` as follows:
```julia
const MOIB = MathOptInterfaceBridges
@bridge SplitInterval MOIB.SplitIntervalBridge () (Interval,) () () () (ScalarAffineFunction,) () ()
```
Given an optimizer `optimizer` implementing `ScalarAffineFunction`-in-`GreaterThan` and `ScalarAffineFunction`-in-`LessThan`, the optimizer
```
bridgedoptimizer = SplitInterval(optimizer)
```
will additionally support `ScalarAffineFunction`-in-`Interval`.

[build-img]: https://travis-ci.org/JuliaOpt/MathOptInterfaceBridges.jl.svg?branch=master
[build-url]: https://travis-ci.org/JuliaOpt/MathOptInterfaceBridges.jl
[winbuild-img]: https://ci.appveyor.com/api/projects/status/50cqm3r9knscon6f/branch/master?svg=true
[winbuild-url]: https://ci.appveyor.com/project/JuliaOpt/mathoptinterfaceutilities-jl/branch/master
[coveralls-img]: https://coveralls.io/repos/github/JuliaOpt/MathOptInterfaceBridges.jl/badge.svg?branch=master
[coveralls-url]: https://coveralls.io/github/JuliaOpt/MathOptInterfaceBridges.jl?branch=master
[codecov-img]: http://codecov.io/github/JuliaOpt/MathOptInterfaceBridges.jl/coverage.svg?branch=master
[codecov-url]: http://codecov.io/github/JuliaOpt/MathOptInterfaceBridges.jl?branch=master
