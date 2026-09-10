# Returns the configurations by the iteration in which they were executed.

Returns the configurations by the iteration in which they were executed.

## Usage

``` r
getConfigurationByIteration(iraceResults, iterations, drop.metadata = FALSE)
```

## Arguments

- iraceResults:

  [`list()`](https://rdrr.io/r/base/list.html)\|`character(1)`  
  Object created by irace and typically saved in the log file
  `irace.Rdata`. If a character string is given, then it is interpreted
  as the path to the log file from which the `iraceResults` object will
  be loaded.

- iterations:

  [`integer()`](https://rdrr.io/r/base/integer.html)  
  The iteration number or a vector of iteration numbers from where the
  configurations should be obtained. Negative values start counting from
  the last iteration.

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
getConfigurationByIteration(log_file, iterations = c(-2, -1), drop.metadata = TRUE)
#>     algorithm localsearch  alpha   beta    rho ants nnls q0 dlb rasrank
#> 136        as           3 3.9196 0.3768 0.9887    8   22 NA   1      NA
#> 150        as           3 3.7455 2.5667 0.7661   15   15 NA   1      NA
#> 120        as           3 3.9042 2.3662 0.8216   13   16 NA   1      NA
#> 166        as           3 4.0313 1.4703 0.8952   20   13 NA   1      NA
#> 134        as           3 4.2465 1.2228 0.8997   20   10 NA   1      NA
#> 167        as           3 4.0264 1.6084 0.9609   13   17 NA   1      NA
#> 168        as           3 2.7959 0.9602 0.8926    9   27 NA   1      NA
#>     elitistants time
#> 136          NA    5
#> 150          NA    5
#> 120          NA    5
#> 166          NA    5
#> 134          NA    5
#> 167          NA    5
#> 168          NA    5
```
