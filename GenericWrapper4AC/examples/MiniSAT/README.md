#Example for using the Generic Wrapper for MiniSAT

This example shows how we used the generic wrapper in the Configurable SAT Solver Challenge (CSSC).
As a SAT solver, we use here the well-known solver [MiniSAT](http://minisat.se/).

As a general interface to SAT solvers, we provide the class `genericWrapper4AC.domain_specific.satwrapper`.
  
For a new SAT solver, only a single function needs to be implemented: `get_command_line_args()` 
that generates the call string given an instance, a configuration and a cutoff time.
  
The following, SAT-specific function is already implemented, `process_results()`:

  1. Reading the output file of the solver
  1. Searches for `SATISFIABLE` or `UNSATISFIABLE` and checks the result if possible.
    1. `SATISFIABLE` can be checked with one of the following ways:
      * With the option `--sat-checker`, you can provide the binary of the SAT checker tool from the SAT competition; this will explicitly check the returned variable assignment.
      * With the option `--sol-file`, you provide a CSV file (whitespace separated) that includes an entry for each instance in the format `<instance name> <SATISIFABLE|UNSATISFIABLE>`
      * The instance specific is set to either `SAT` or `UNSAT`
    1.   `UNSATISFIABLE` can be checked with the two latter ways of checking `SATISFIABLE`

An example call for minisat could the be following:

`python examples/MiniSAT/MiniSATWrapper.py examples/MiniSAT/gzip_vc1071.cnf SAT 10 0 42 -rnd-freq 0 -var-decay 0.001 -cla-decay 0.001 -gc-frac 0.000001 -rfirst 1000`

This will run `MiniSat` on the instance `gzip_vc1071.cnf`, assuming that it is a `SATISFIABLE` instance, using at most 10 CPU seconds and a random seed of 42. This will be translated in the following call:

`examples/MiniSAT/minisat -rnd-seed=42 -cla-decay=0.001 -var-decay=0.001 -gc-frac=0.000001 -rfirst=1000 -rnd-freq=0 examples/MiniSAT/gzip_vc1071.cnf`

The output of the wrapper call will include:

`Result for ParamILS: SUCCESS, 0.004, -1, -1, 42, SAT checker was not given; could not verify SAT`

With this line, ParamILS/SMAC will know that `MiniSat` returned successfully and needed 0.004 CPU seconds.

The alternative call in the new format would be

`python examples/MiniSAT/MiniSATWrapper.py --instance examples/MiniSAT/gzip_vc1071.cnf --cutoff 10 --seed 42 --config -rnd-freq 0 -var-decay 0.001 -cla-decay 0.001 -gc-frac 0.000001 -rfirst 1000`
 