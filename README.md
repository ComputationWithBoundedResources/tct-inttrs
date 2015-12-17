##tct-inttrs
This package is part of the _Tyrolean Complexity Tool (TcT)_ and provides
a presentation for (top-level) integer term rewrite systems.

###Example
```
  outer(x, r)       -> inner(1, 1, x, r)       [ x >= 0 && r <= 100000]
  inner(f, i, x, r) -> inner(f + i, i+1, x, r) [ i <= x ]
  inner(f, i, x, r) -> outer(x - 1, r + f)     [ i > x ]
  g(cons(x, xs), y) -> g(xs, y + 1)
  h(xs, y)          -> h(cons(0, xs), y - 1)   [ y  > 0]
```
See [here](http://aprove.informatik.rwth-aachen.de/help_new/inttrs.html) for details about the format.

##Requirements

Executables:
  * [Glasgow Haskell Compiler, version 7.10](http://www.haskell.org/ghc/)
  * [minismt, version 0.6](http://cl-informatik.uibk.ac.at/software/minismt/) or [z3, version 4.3](https://github.com/Z3Prover/z3)
  * [yices, version 2.3](http://yices.csl.sri.com/)

Other packages:
  * [slogic](https://github.com/ComputationWithBoundedResources/slogic/)
  * [tct-core](https://github.com/ComputationWithBoundedResources/tct-core/)
  * [tct-common](https://github.com/ComputationWithBoundedResources/tct-common/)
  * [tct-its](https://github.com/ComputationWithBoundedResources/tct-trs/)
  * [tct-trs](https://github.com/ComputationWithBoundedResources/tct-its/)

The tool is only tested under GNU/Linux.

###Installation

####Using Stack
We recommend using [stack](https://github.com/commercialhaskell/stack) with the accompanied `stack.yaml` file.
To build and install the package run following command:

```bash
stack install tct-inttrs
```

###Example Usage

To transform a problem into TRS or ITS format use following commands respectively:
```bash
#output TRS in WST format
tct-inttrs --putTrs $problem
#output ITS in Koat format
tct-inttrs --putIts $problem
```

To directly invoke the resource analysis on the transformed problems use one of the following commands:
```bash
# executes the default strategies on resulting TRS and ITS problem, returning the first successfull result
tct-inttrs $problem
# executes default trs strategy on resulting TRS problem
tct-inttrs --strategy withTrs $problem
# executes default its strategy on resulting ITS problem
tct-inttrs --strategy withIts $problem 
# executes 'polys' strategy on resulting TRS problem
tct-inttrs --strategy "withTrs :trs polys"
```

