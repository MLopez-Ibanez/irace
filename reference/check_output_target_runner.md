# Check the output of the target runner and repair it if possible. If the output is incorrect, this function will throw an error.

Check the output of the target runner and repair it if possible. If the
output is incorrect, this function will throw an error.

## Usage

``` r
check_output_target_runner(output, scenario, bound = NULL)
```

## Arguments

- output:

  The output from target runner.

- scenario:

  [`list()`](https://rdrr.io/r/base/list.html)  
  Data structure containing irace settings. The data structure has to be
  the one returned by the function
  [`defaultScenario()`](https://mlopez-ibanez.github.io/irace/reference/defaultScenario.md)
  or
  [`readScenario()`](https://mlopez-ibanez.github.io/irace/reference/readScenario.md).

- bound:

  Optional time bound that the target runner should have respected.

## Value

The output with its contents repaired.
