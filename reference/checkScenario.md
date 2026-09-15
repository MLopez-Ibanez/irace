# Check and correct the given scenario

Checks for errors a (possibly incomplete) scenario setup of irace and
transforms it into a valid scenario.

## Usage

``` r
checkScenario(scenario = defaultScenario())
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

The scenario received as a parameter, possibly corrected. Unset scenario
settings are set to their default values.

## Details

This function checks that the directories and the file names provided
and required by the irace exist. It also checks that the settings are of
the proper type, e.g. that settings expected to be integers are really
integers. Finally, it also checks that there is no inconsistency between
settings. If an error is found that prevents irace from running
properly, it will stop with an error.

## See also

- [`readScenario()`](https://mlopez-ibanez.github.io/irace/reference/readScenario.md):

  for reading a configuration scenario from a file.

- [`printScenario()`](https://mlopez-ibanez.github.io/irace/reference/printScenario.md):

  prints the given scenario.

- [`defaultScenario()`](https://mlopez-ibanez.github.io/irace/reference/defaultScenario.md):

  returns the default scenario settings of irace.

- `checkScenario()`:

  to check that the scenario is valid.

## Author

Manuel López-Ibáñez and Jérémie Dubois-Lacoste
