# Returns the configurations selected by ID.

Returns the configurations selected by ID.

## Usage

``` r
getConfigurationById(iraceResults, ids, drop.metadata = FALSE)
```

## Arguments

- iraceResults:

  [`list()`](https://rdrr.io/r/base/list.html)\|`character(1)`  
  Object created by irace and typically saved in the log file
  `irace.Rdata`. If a character string is given, then it is interpreted
  as the path to the log file from which the `iraceResults` object will
  be loaded.

- ids:

  [`integer()`](https://rdrr.io/r/base/integer.html)  
  The id or a vector of ids of the candidates configurations to obtain.

- drop.metadata:

  `logical(1)`  
  Remove metadata, such as the configuration ID and the ID of the
  parent, from the returned configurations. See
  [`removeConfigurationsMetaData()`](https://mlopez-ibanez.github.io/irace/reference/removeConfigurationsMetaData.md).

## Value

A data frame containing the elite configurations required, in the order
and with the repetitions given by `ids`.

## Author

Manuel López-Ibáñez and Leslie Pérez Cáceres

## Examples

``` r
log_file <- system.file("exdata/irace-acotsp.Rdata", package="irace", mustWork=TRUE)
getConfigurationById(log_file, ids = c(2,1), drop.metadata = TRUE)
#>   algorithm localsearch  alpha   beta    rho ants nnls q0  dlb rasrank
#> 2      mmas           2 0.3654 6.2246 0.9672   19   50 NA    1      NA
#> 1        as           0 1.0000 1.0000 0.9500   10   NA NA <NA>      NA
#>   elitistants time
#> 2          NA    5
#> 1          NA    5
```
