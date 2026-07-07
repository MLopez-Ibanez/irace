# Default `targetRunner` function.

Use it as an advanced example of how to create your own `targetRunner`
function.

## Usage

``` r
target_runner_default(experiment, scenario)
```

## Arguments

- experiment:

  A list describing the experiment. It contains at least:

  `id_configuration`

  :   An alphanumeric string that uniquely identifies a configuration;

  `id_instance`

  :   An alphanumeric string that uniquely identifies an instance;

  `seed`

  :   Seed for the random number generator to be used for this
      evaluation, ignore the seed for deterministic algorithms;

  `instance`

  :   String giving the instance to be used for this evaluation;

  `bound`

  :   (only when `capping` is enabled) Time bound for the execution;

  `configuration`

  :   1-row data frame with a column per parameter name;

- scenario:

  [`list()`](https://rdrr.io/r/base/list.html)  
  Data structure containing irace settings. The data structure has to be
  the one returned by the function
  [`defaultScenario()`](https://mlopez-ibanez.github.io/irace/reference/defaultScenario.md)
  or
  [`readScenario()`](https://mlopez-ibanez.github.io/irace/reference/readScenario.md).

## Value

If `targetEvaluator` is `NULL`, then the `targetRunner` function must
return a list with at least one element `"cost"`, the numerical value
corresponding to the evaluation of the given configuration on the given
instance.

If the scenario option `maxTime` is non-zero or if `capping` is enabled
then the list must contain at least another element `"time"` that
reports the execution time for this call to `targetRunner`. The return
list may also contain the following optional elements that are used by
irace for reporting errors in `targetRunner`:

- `error`:

  is a string used to report an error;

- `outputRaw`:

  is a string used to report the raw output of calls to an external
  program or function;

- `call`:

  is a string used to report how `targetRunner` called an external
  program or function.

## Author

Manuel López-Ibáñez and Jérémie Dubois-Lacoste
