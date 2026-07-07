# Test that the given irace scenario can be run.

Test that the given irace scenario can be run by checking the scenario
settings provided and trying to run the target-algorithm.

## Usage

``` r
checkIraceScenario(scenario)
```

## Arguments

- scenario:

  [`list()`](https://rdrr.io/r/base/list.html)  
  Data structure containing irace settings. The data structure has to be
  the one returned by the function
  [`defaultScenario()`](https://mlopez-ibanez.github.io/irace/reference/defaultScenario.md)
  or
  [`readScenario()`](https://mlopez-ibanez.github.io/irace/reference/readScenario.md).

## Value

returns `TRUE` if successful and gives an error and returns `FALSE`
otherwise.

## Details

If the `parameters` argument is missing, then the parameters will be
read from the file `parameterFile` given by `scenario`. If `parameters`
is provided, then `parameterFile` will not be read. This function will
try to execute the target-algorithm.

## See also

- [`readScenario`](https://mlopez-ibanez.github.io/irace/reference/readScenario.md):

  for reading a configuration scenario from a file.

- [`printScenario`](https://mlopez-ibanez.github.io/irace/reference/printScenario.md):

  prints the given scenario.

- [`defaultScenario`](https://mlopez-ibanez.github.io/irace/reference/defaultScenario.md):

  returns the default scenario settings of irace.

- [`checkScenario`](https://mlopez-ibanez.github.io/irace/reference/checkScenario.md):

  to check that the scenario is valid.

## Author

Manuel López-Ibáñez and Jérémie Dubois-Lacoste
