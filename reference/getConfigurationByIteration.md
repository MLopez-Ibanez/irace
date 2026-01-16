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
#> 118       ras           3 4.0526 2.5427 0.7522   17   20 NA   1       9
#> 113       ras           3 4.1042 1.5310 0.8104   16   12 NA   1      24
#> 149       ras           3 3.7080 0.4873 0.9452    8   11 NA   1      50
#> 117       ras           3 4.1817 0.5324 0.8893   13   18 NA   1      23
#> 124       ras           3 3.1663 0.6971 0.8396   24   26 NA   1      19
#> 154       ras           3 4.3131 2.6776 0.7208   14   16 NA   1      11
#> 155       ras           3 3.5982 1.4889 0.6575   15    9 NA   1      26
#>     elitistants time
#> 118          NA    5
#> 113          NA    5
#> 149          NA    5
#> 117          NA    5
#> 124          NA    5
#> 154          NA    5
#> 155          NA    5
```
