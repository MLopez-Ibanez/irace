# Print configurations as a data frame

Print configurations as a data frame

## Usage

``` r
configurations_print(configurations, metadata = FALSE)
```

## Arguments

- configurations:

  `data.frame`  
  Parameter configurations of the target algorithm (one per row).

- metadata:

  `logical(1)`  
  whether to print the metadata or not. The metadata are data for the
  configurations (additionally to the value of each parameter) used by
  irace.

## Value

None.

## See also

[`configurations_print_command()`](https://mlopez-ibanez.github.io/irace/reference/configurations_print_command.md)
to print the configurations as command-line strings.

## Author

Manuel López-Ibáñez and Jérémie Dubois-Lacoste
