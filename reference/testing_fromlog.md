# Test configurations given in the logfile (typically `irace.Rdata`) produced by irace.

`testing_fromlog` executes the testing of the target algorithm
configurations found by an irace execution.

## Usage

``` r
testing_fromlog(
  logFile,
  testNbElites,
  testIterationElites,
  testInstancesDir,
  testInstancesFile,
  testInstances
)
```

## Arguments

- logFile:

  `character(1)`  
  Path to the logfile (typically `irace.Rdata`) produced by irace.

- testNbElites:

  Number of (final) elite configurations to test. Overrides the value
  found in `logFile`.

- testIterationElites:

  `logical(1)`  
  If `FALSE`, only the final `testNbElites` configurations are tested;
  otherwise, also test the best configurations of each iteration.
  Overrides the value found in `logFile`.

- testInstancesDir:

  Directory where testing instances are located, either absolute or
  relative to current directory.

- testInstancesFile:

  File containing a list of test instances and optionally additional
  parameters for them.

- testInstances:

  Character vector of the instances to be used in the `targetRunner`
  when executing the testing.

## Value

`logical(1)`  
`TRUE` if the testing ended successfully otherwise, `FALSE`.

## Details

The function `testing_fromlog` loads the `logFile` and obtains the
testing setup and configurations to be tested. Within the `logFile`, the
variable `scenario$testNbElites` specifies how many final elite
configurations to test and `scenario$testIterationElites` indicates
whether test the best configuration of each iteration. The values may be
overridden by setting the corresponding arguments in this function. The
set of testing instances must appear in `scenario[["testInstances"]]`.

## See also

[`defaultScenario()`](https://mlopez-ibanez.github.io/irace/reference/defaultScenario.md)
to provide a default scenario for irace.
[`testing_fromfile()`](https://mlopez-ibanez.github.io/irace/reference/testing_fromfile.md)
provides a different interface for testing.

## Author

Manuel López-Ibáñez and Leslie Pérez Cáceres
