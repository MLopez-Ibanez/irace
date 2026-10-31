# Returns the pairs of instance IDs and seeds used as instances in the race (and optionally the actual instances).

Returns the pairs of instance IDs and seeds used as instances in the
race (and optionally the actual instances).

## Usage

``` r
get_instanceID_seed_pairs(iraceResults, index, instances = FALSE)
```

## Arguments

- iraceResults:

  [`list()`](https://rdrr.io/r/base/list.html)\|`character(1)`  
  Object created by irace and typically saved in the log file
  `irace.Rdata`. If a character string is given, then it is interpreted
  as the path to the log file from which the `iraceResults` object will
  be loaded.

- index:

  [`integer()`](https://rdrr.io/r/base/integer.html)  
  Indexes of the (instanceID,seed) pairs to be returned. The default
  returns everything.

- instances:

  `logical(1)`  
  Whether to add the actual instances as an additional column (only if
  the instances are of atomic type).

## Value

`data.table()`  
With default arguments, a `data.table` containing two columns
`"instanceID"` and `"seed"`. With `instances=TRUE` and if the instances
are of atomic type (see
[`is.atomic()`](https://rdrr.io/r/base/is.recursive.html)) type, another
column `instance` is added that contains the actual instance.

## Author

Manuel López-Ibáñez

## Examples

``` r
log_file <- system.file("exdata/irace-acotsp.Rdata", package="irace", mustWork=TRUE)
head(get_instanceID_seed_pairs(log_file))
#>    instanceID       seed
#>         <int>      <int>
#> 1:         48 1498426593
#> 2:         49 1324006684
#> 3:          9  156117387
#> 4:         33 2123556176
#> 5:         31  975149182
#> 6:          2  657774990
# Add the instance names
get_instanceID_seed_pairs(log_file, index=1:10, instances=TRUE)
#>     instanceID       seed                 instance
#>          <int>      <int>                   <char>
#>  1:         48 1498426593 ./instances/2000-918.tsp
#>  2:         49 1324006684 ./instances/2000-919.tsp
#>  3:          9  156117387 ./instances/2000-519.tsp
#>  4:         33 2123556176 ./instances/2000-813.tsp
#>  5:         31  975149182 ./instances/2000-811.tsp
#>  6:          2  657774990 ./instances/2000-512.tsp
#>  7:         25 1688886839 ./instances/2000-715.tsp
#>  8:         39 1722597766 ./instances/2000-819.tsp
#>  9:         22  545710096 ./instances/2000-712.tsp
#> 10:         36  685987118 ./instances/2000-816.tsp
```
