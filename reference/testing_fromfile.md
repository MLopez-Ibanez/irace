# Test configurations given an explicit table of configurations and a scenario file

Executes the testing of an explicit list of configurations given in
`filename` (same format as in
[`readConfigurationsFile()`](https://mlopez-ibanez.github.io/irace/reference/readConfigurationsFile.md)).
A `logFile` is created unless disabled in `scenario`. This may overwrite
an existing one!

## Usage

``` r
testing_fromfile(filename, scenario)
```

## Arguments

- filename:

  `character(1)`  
  Path to a file containing configurations: one configuration per line,
  one parameter per column, parameter names in header.

- scenario:

  [`list()`](https://rdrr.io/r/base/list.html)  
  Data structure containing irace settings. The data structure has to be
  the one returned by the function
  [`defaultScenario()`](https://mlopez-ibanez.github.io/irace/reference/defaultScenario.md)
  or
  [`readScenario()`](https://mlopez-ibanez.github.io/irace/reference/readScenario.md).

## Value

iraceResults

## See also

[`testing_fromlog()`](https://mlopez-ibanez.github.io/irace/reference/testing_fromlog.md)
provides a different interface for testing.

## Author

Manuel López-Ibáñez
