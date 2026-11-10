# Execute the given configurations on the testing instances specified in the scenario

Execute the given configurations on the testing instances specified in
the scenario

## Usage

``` r
testConfigurations(configurations, scenario)
```

## Arguments

- configurations:

  `data.frame`  
  Parameter configurations of the target algorithm (one per row).

- scenario:

  [`list()`](https://rdrr.io/r/base/list.html)  
  Data structure containing irace settings. The data structure has to be
  the one returned by the function
  [`defaultScenario()`](https://mlopez-ibanez.github.io/irace/reference/defaultScenario.md)
  or
  [`readScenario()`](https://mlopez-ibanez.github.io/irace/reference/readScenario.md).

## Value

A list with the following elements:

- `experiments`:

  Experiments results.

- `seeds`:

  Array of the instance seeds used in the experiments.

## Details

A test instance set must be provided through
`scenario[["testInstances"]]`.

## See also

[`testing_fromlog()`](https://mlopez-ibanez.github.io/irace/reference/testing_fromlog.md)

## Author

Manuel López-Ibáñez
