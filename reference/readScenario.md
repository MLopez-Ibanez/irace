# Reads from a file the scenario settings to be used by irace.

The scenario argument is an initial scenario that is overwritten for
every setting specified in the file to be read.

## Usage

``` r
readScenario(filename = "", scenario = list(), params_def = .irace.params.def)
```

## Arguments

- filename:

  `character(1)`  
  Filename from which the scenario will be read. If empty, the default
  `scenarioFile` is used. An example scenario file is provided in
  `system.file(package="irace", "templates/scenario.txt.tmpl")`.

- scenario:

  [`list()`](https://rdrr.io/r/base/list.html)  
  Data structure containing irace settings. The data structure has to be
  the one returned by the function
  [`defaultScenario()`](https://mlopez-ibanez.github.io/irace/reference/defaultScenario.md)
  or `readScenario()`.

- params_def:

  [`data.frame()`](https://rdrr.io/r/base/data.frame.html)  
  Definition of the options accepted by the scenario. This should only
  be modified by packages that wish to extend irace.

## Value

The scenario list read from the file. The scenario settings not present
in the file are not present in the list, i.e., they are `NULL`.

## See also

- [`printScenario()`](https://mlopez-ibanez.github.io/irace/reference/printScenario.md):

  prints the given scenario.

- [`defaultScenario()`](https://mlopez-ibanez.github.io/irace/reference/defaultScenario.md):

  returns the default scenario settings of irace.

- [`checkScenario()`](https://mlopez-ibanez.github.io/irace/reference/checkScenario.md):

  to check that the scenario is valid.

## Author

Manuel López-Ibáñez and Jérémie Dubois-Lacoste
