# removeConfigurationsMetaData

Remove the columns with "metadata" of a data frame containing
configurations. Currently, metadata corresponds to column names starting
with a period. This function should be used before printing the
configurations to output only the values for the parameters of the
configuration without metadata possibly useless to the user.

## Usage

``` r
removeConfigurationsMetaData(configurations)
```

## Arguments

- configurations:

  `data.frame`  
  Parameter configurations of the target algorithm (one per row).

## Value

The same data frame without "metadata".

## See also

[`configurations_print_command()`](https://mlopez-ibanez.github.io/irace/reference/configurations_print_command.md)
to print the configurations as command lines.
[`configurations_print()`](https://mlopez-ibanez.github.io/irace/reference/configurations_print.md)
to print the configurations as a data frame.

## Author

Manuel López-Ibáñez and Jérémie Dubois-Lacoste
