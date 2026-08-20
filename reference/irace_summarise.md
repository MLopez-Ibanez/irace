# Summarise the results of a run of irace

Summarise the results of a run of irace

## Usage

``` r
irace_summarise(iraceResults)
```

## Arguments

- iraceResults:

  [`list()`](https://rdrr.io/r/base/list.html)\|`character(1)`  
  Object created by irace and typically saved in the log file
  `irace.Rdata`. If a character string is given, then it is interpreted
  as the path to the log file from which the `iraceResults` object will
  be loaded.

## Value

[`list()`](https://rdrr.io/r/base/list.html)

## Author

Manuel López-Ibáñez

## Examples

``` r
irace_results <- read_logfile(system.file("exdata/irace-acotsp.Rdata",
                                          package="irace", mustWork=TRUE))
irace_summarise(irace_results)
#> $version
#> [1] "4.4.0.d5a98af"
#> 
#> $n_iterations
#> [1] 7
#> 
#> $n_configurations
#> [1] 145
#> 
#> $n_initial_configurations
#> [1] 0
#> 
#> $n_instances
#> [1] 17
#> 
#> $n_experiments
#> [1] 1000
#> 
#> $n_elites
#> [1] 5
#> 
#> $n_soft_restarts
#> [1] 0
#> 
#> $n_rejected
#> [1] 0
#> 
#> $time_targetrunner
#> [1] 0
#> 
#> $time_cpu_user
#> [1] 5273.256
#> 
#> $time_cpu_sys
#> [1] 58.97
#> 
#> $time_cpu_total
#> [1] 5332.226
#> 
#> $time_wallclock
#> [1] 2788.873
#> 
#> $termination_reason
#> [1] "Not enough budget to race more than the minimum configurations"
#> 
```
