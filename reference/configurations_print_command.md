# Print configurations as command-line strings.

Prints configurations after converting them into a representation for
the command-line.

## Usage

``` r
configurations_print_command(configurations, parameters)
```

## Arguments

- configurations:

  `data.frame`  
  Parameter configurations of the target algorithm (one per row).

- parameters:

  `ParameterSpace`  
  Data structure containing the parameter space definition. The data
  structure has to similar to the one returned by the function
  [`readParameters`](https://mlopez-ibanez.github.io/irace/reference/readParameters.md).

## Value

None.

## See also

[`configurations_print()`](https://mlopez-ibanez.github.io/irace/reference/configurations_print.md)
to print the configurations as a data frame.

## Author

Manuel López-Ibáñez and Jérémie Dubois-Lacoste
