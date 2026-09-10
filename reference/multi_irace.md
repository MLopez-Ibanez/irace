# Execute [`irace()`](https://mlopez-ibanez.github.io/irace/reference/irace.md) multiple times with the same or different scenarios and parameter space definitions.

There are three modes of operation:

- One `scenarios` and `k` `parameters`: `k` runs with the same scenario
  and each parameter space definition.

- One `parameters` and `k` `scenarios`: `k` runs with the same parameter
  space definition and each scenario.

- `k` `parameters` and `k` scenarios: `k` runs with each scenario and
  parameter space definition.

Each of the `k` runs can be repeated `n` times by supplying a value for
`n`.

## Usage

``` r
multi_irace(
  scenarios,
  parameters,
  n = 1L,
  parallel = 1L,
  split_output = parallel > 1L,
  global_seed = NULL
)
```

## Arguments

- scenarios:

  [`list()`](https://rdrr.io/r/base/list.html)  
  A list of scenarios. If only a single scenario is supplied, it is used
  for all parameters.

- parameters:

  [`list()`](https://rdrr.io/r/base/list.html)  
  A list of parameter space definitions. If only a single definition is
  supplied, it is used for all scenarios.

- n:

  `integer(1)`  
  The number of repetitions.

- parallel:

  `integer(1)`  
  The number of workers to use. A value of `1` means sequential
  execution. Note that `parallel > 1` is not supported on Windows.

- split_output:

  `logical(1)`  
  If `TRUE`, the output of
  [`irace()`](https://mlopez-ibanez.github.io/irace/reference/irace.md)
  is written to `{execDir}/run_{i}/irace.out` instead of the standard
  output.

- global_seed:

  `integer(1)`  
  The global seed used to seed the individual runs.

## Value

A list of the outputs of
[`irace()`](https://mlopez-ibanez.github.io/irace/reference/irace.md).

## See also

- [`irace()`](https://mlopez-ibanez.github.io/irace/reference/irace.md):

  the main interface for single irace runs.
