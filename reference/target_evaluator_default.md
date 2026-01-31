# target_evaluator_default

`target_evaluator_default` is the default `targetEvaluator` function
that is invoked if `targetEvaluator` is a string (by default
`targetEvaluator` is `NULL` and this function is not invoked). You can
use it as an advanced example of how to create your own
`targetEvaluator` function.

## Usage

``` r
target_evaluator_default(
  experiment,
  num_configurations,
  all_conf_id,
  scenario,
  target_runner_call
)
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

- num_configurations:

  Number of configurations alive in the race.

- all_conf_id:

  Vector of configuration IDs of the alive configurations.

- scenario:

  [`list()`](https://rdrr.io/r/base/list.html)  
  Data structure containing irace settings. The data structure has to be
  the one returned by the function
  [`defaultScenario()`](https://mlopez-ibanez.github.io/irace/reference/defaultScenario.md)
  or
  [`readScenario()`](https://mlopez-ibanez.github.io/irace/reference/readScenario.md).

- target_runner_call:

  String describing the call to `targetRunner` that corresponds to this
  call to `targetEvaluator`. This is used for providing extra
  information to the user, for example, in case `targetEvaluator` fails.

## Value

The function `targetEvaluator` must return a list with one element
`"cost"`, the numerical value corresponding to the cost measure of the
given configuration on the given instance.

The return list may also contain the following optional elements that
are used by irace for reporting errors in `targetEvaluator`:

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
