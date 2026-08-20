# Return the elite configurations of the final iteration.

Return the elite configurations of the final iteration.

## Usage

``` r
getFinalElites(iraceResults, n = 0L, drop.metadata = FALSE)
```

## Arguments

- iraceResults:

  [`list()`](https://rdrr.io/r/base/list.html)\|`character(1)`  
  Object created by irace and typically saved in the log file
  `irace.Rdata`. If a character string is given, then it is interpreted
  as the path to the log file from which the `iraceResults` object will
  be loaded.

- n:

  `integer(1)`  
  Number of elite configurations to return, if `n` is larger than the
  number of configurations, then only the existing ones are returned.
  The default (`n=0`) returns all of them.

- drop.metadata:

  `logical(1)`  
  Remove metadata, such as the configuration ID and the ID of the
  parent, from the returned configurations. See
  [`removeConfigurationsMetaData()`](https://mlopez-ibanez.github.io/irace/reference/removeConfigurationsMetaData.md).

## Value

A data frame containing the elite configurations required.

## Author

Manuel López-Ibáñez and Leslie Pérez Cáceres

## Examples

``` r
log_file <- system.file("exdata/irace-acotsp.Rdata", package="irace", mustWork=TRUE)
print(removeConfigurationsMetaData(getFinalElites(log_file, n=1)))
#>     algorithm localsearch  alpha   beta    rho ants nnls     q0 dlb rasrank
#> 119       acs           3 3.4033 3.8517 0.4882    7   11 0.7569   1      NA
#>     elitistants time
#> 119          NA    5
```
