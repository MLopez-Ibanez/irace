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
#>     algorithm localsearch  alpha   beta    rho ants nnls     q0 dlb rasrank
#> 121       acs           3 4.8954 2.4081 0.0516    9   16 0.2240   1      NA
#> 119       acs           3 3.4033 3.8517 0.4882    7   11 0.7569   1      NA
#> 139       acs           3 3.7213 0.3944 0.8664   10   14 0.8420   1      NA
#> 125       acs           3 4.4088 1.6574 0.4104    7   11 0.6845   1      NA
#> 141       acs           3 4.2378 2.1780 0.5210    8   12 0.6151   1      NA
#> 142       acs           3 4.2020 1.8978 0.4704    9    9 0.6805   1      NA
#> 143       acs           3 4.2317 0.9912 0.2704   13   20 0.5922   1      NA
#> 144       acs           3 4.6329 3.7012 0.3543    6   12 0.5125   1      NA
#> 145       acs           3 2.7261 3.4642 0.4557   15   11 0.4044   1      NA
#>     elitistants time
#> 121          NA    5
#> 119          NA    5
#> 139          NA    5
#> 125          NA    5
#> 141          NA    5
#> 142          NA    5
#> 143          NA    5
#> 144          NA    5
#> 145          NA    5
```
