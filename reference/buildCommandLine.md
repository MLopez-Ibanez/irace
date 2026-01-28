# Generate a command-line representation of a configuration

`buildCommandLine` receives two vectors, one containing the values of
the parameters, the other containing the switches of the parameters. It
builds a string with the switches and the values that can be used as a
command line to call the program to be tuned, thus generating one
candidate configuration.

## Usage

``` r
buildCommandLine(values, switches)
```

## Arguments

- values:

  A vector containing the value of each parameter for the candidate
  configuration.

- switches:

  A vector containing the switches of each paramter (in an order that
  corresponds to the values vector).

## Value

A string concatenating each element of `switches` and `values` for all
parameters with a space between each pair of parameters (but none
between the switches and the corresponding values).

## Author

Manuel López-Ibáñez and Jérémie Dubois-Lacoste

## Examples

``` r
switches <- c("--switch1 ", "--switch2-", "--switch3=")
values <- list("value_1", 1L, sqrt(2))
buildCommandLine (values, switches)
#> [1] "--switch1 value_1 --switch2-1 --switch3=1.4142135623731"
## Build a command-line from the results produced by a previous run of irace.
# First, load the data produced by irace.
logfile <- file.path(system.file(package="irace"), "exdata", "irace-acotsp.Rdata")
iraceResults <- read_logfile(logfile)
allConfigurations <- iraceResults$allConfigurations
parameters <- iraceResults$scenario$parameters
apply(allConfigurations[1:10, unlist(parameters$names)], 1, buildCommandLine,
      unlist(parameters$switches))
#>                                                                                                                         1 
#>                                      "--as --localsearch 0 --alpha 1.0000 --beta 1.0000 --rho  0.9500 --ants 10 --time 5" 
#>                                                                                                                         2 
#>                  "--mmas --localsearch 2 --alpha 0.3654 --beta 6.2246 --rho  0.9672 --ants 19 --nnls 50 --dlb 1 --time 5" 
#>                                                                                                                         3 
#>                       "--ras --localsearch 0 --alpha 2.8654 --beta 1.2246 --rho  0.4722 --ants 86 --rasranks 42 --time 5" 
#>                                                                                                                         4 
#>       "--acs --localsearch 3 --alpha 1.6154 --beta 8.7246 --rho  0.2247 --ants 41 --nnls 39 --q0 0.7617 --dlb 0 --time 5" 
#>                                                                                                                         5 
#> "--eas --localsearch 1 --alpha 4.1154 --beta 3.7246 --rho  0.7197 --ants  9 --nnls 16 --dlb 1 --elitistants 602 --time 5" 
#>                                                                                                                         6 
#>                                      "--as --localsearch 0 --alpha 2.2404 --beta 7.4746 --rho  0.8434 --ants  6 --time 5" 
#>                                                                                                                         7 
#> "--eas --localsearch 2 --alpha 4.7404 --beta 2.4746 --rho  0.3484 --ants 28 --nnls 44 --dlb 1 --elitistants 508 --time 5" 
#>                                                                                                                         8 
#>       "--acs --localsearch 1 --alpha 0.9904 --beta 9.9746 --rho  0.1009 --ants 59 --nnls 10 --q0 0.3867 --dlb 1 --time 5" 
#>                                                                                                                         9 
#>                  "--mmas --localsearch 3 --alpha 3.4904 --beta 4.9746 --rho  0.5959 --ants 13 --nnls 33 --dlb 0 --time 5" 
#>                                                                                                                        10 
#> "--eas --localsearch 2 --alpha 1.3029 --beta 0.5996 --rho  0.0390 --ants  5 --nnls 47 --dlb 1 --elitistants 649 --time 5" 
```
