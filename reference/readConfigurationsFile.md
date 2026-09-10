# Read parameter configurations from a file

Reads a set of target-algorithm configurations from a file and puts them
in irace format. The configurations are checked to match the parameters
description provided.

## Usage

``` r
readConfigurationsFile(filename, parameters, debugLevel = 0L, text)
```

## Arguments

- filename:

  `character(1)`  
  Filename from which the configurations should be read. The contents
  should be readable by `read.table( , header=TRUE)`.

- parameters:

  `ParameterSpace`  
  Data structure containing the parameter space definition. The data
  structure has to similar to the one returned by the function
  [`readParameters`](https://mlopez-ibanez.github.io/irace/reference/readParameters.md).

- debugLevel:

  `integer(1)`  
  Larger values produce more verbose output.

- text:

  `character(1)`  
  If `file` is not supplied and this is, then configurations are read
  from the value of `text` via a text connection.

## Value

A data frame containing the obtained configurations. Each row of the
data frame is a candidate configuration, the columns correspond to the
parameter names in `parameters`.

## Details

Example of an input file:

    # This is a comment line
    param_1    param_2
        0.5  "value_1"
        1.0         NA
        1.2  "value_3"

The order of the columns does not necessarily have to be the same as in
the file containing the definition of the parameters.

## See also

[`readParameters()`](https://mlopez-ibanez.github.io/irace/reference/readParameters.md)
to obtain a valid parameter structure from a parameters file.

## Author

Manuel López-Ibáñez and Jérémie Dubois-Lacoste
