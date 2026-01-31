# Launch ablation with command-line options.

Launch
[`ablation()`](https://mlopez-ibanez.github.io/irace/reference/ablation.md)
with the same command-line options as the command-line executable
(`ablation.exe` in Windows).

## Usage

``` r
ablation_cmdline(argv = commandArgs(trailingOnly = TRUE))
```

## Arguments

- argv:

  [`character()`](https://rdrr.io/r/base/character.html)  
  The arguments provided on the R command line as a character vector,
  e.g., `c("-i", "irace.Rdata", "--src", 1)`.

## Value

A list containing the following elements:

- allConfigurations:

  Configurations tested in the ablation.

- state:

  State of the ablation process.

- experiments:

  A matrix with the results of the experiments (columns are
  configurations, rows are instances).

- scenario:

  Scenario object with the settings used for the experiments.

- trajectory:

  IDs of the best configurations at each step of the ablation.

- best:

  Best configuration found in the experiments.

- complete:

  `TRUE` if the ablation process was completed.

## Details

The function reads the parameters given on the command line used to
invoke R, launches
[`ablation()`](https://mlopez-ibanez.github.io/irace/reference/ablation.md)
and possibly
[`plotAblation()`](https://mlopez-ibanez.github.io/irace/reference/plotAblation.md).

List of command-line options:

    -l,--log-file            Path to the (.Rdata) file created by irace from which
                             the "iraceResults" object will be loaded.
    -S,--src                 Source configuration ID or the path to a file
                             containing the configuration. Default: 1.
    -T,--target              Target configuration ID (by default the best
                             configuration found by irace) or the path to a file
                             containing the configuration.
    -P,--params              Specific parameter names to be used for the ablation
                             (separated with commas). By default use all
    -t,--type                Type of ablation to perform: "full" will execute each
                             configuration on all "--n-instances" to determine the
                             best-performing one; "racing" will apply racing to
                             find the best configurations. Default: full.
    -n,--nrep                Number of replications per instance used in "full"
                             ablation. Default: 1.
       --seed                Integer value to use as seed for the random number
                             generation. Default: 1234567.
    -o,--output-file         Log file to save the ablation log. If "", the results
                             are not saved to a file. Default: log-ablation.Rdata.
       --instances-file      Instances file used for ablation: "train", "test" or a
                             filename containing the list of instances. Default:
                             train.
    -p,--plot                Output filename (.pdf) for the plot. If not given, no
                             plot is created.
    -O,--plot-type           Type of plot. Supported values are "mean", "boxplot",
                             "rank" or "rank,boxplot". Default: mean.
       --old-path            Old path found in the log-file (.Rdata) given as input
                             to be replaced by --new-path.
       --new-path            New path to replace the path found in the log-file
                             (.Rdata) given as input.
    -e,--exec-dir            Directory where the target runner will be run.
    -s,--scenario            Scenario file to override the scenario given in the
                             log-file (.Rdata)
       --parallel            Number of calls to targetRunner to execute in
                             parallel. Values 0 or 1 mean no parallelization.

## See also

[`plotAblation()`](https://mlopez-ibanez.github.io/irace/reference/plotAblation.md)
[`ablation()`](https://mlopez-ibanez.github.io/irace/reference/ablation.md)

## Author

Manuel López-Ibáñez

## Examples

``` r
ablation_cmdline("--help")
#> #------------------------------------------------------------------------------
#> # ablation: An implementation in R of Ablation Analysis
#> # Version: 4.4.0.dc51faf-dirty
#> # Copyright (C) 2020--2025
#> # Manuel Lopez-Ibanez     <manuel.lopez-ibanez@manchester.ac.uk>
#> # Leslie Perez Caceres    <leslie.perez.caceres@ulb.ac.be>
#> #
#> # This is free software, and you are welcome to redistribute it under certain
#> # conditions.  See the GNU General Public License for details. There is NO
#> # WARRANTY; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
#> #------------------------------------------------------------------------------
#> # installed at: /home/runner/work/_temp/Library/irace
#> # called with: --help
#> -l,--log-file            Path to the (.Rdata) file created by irace from which
#>                          the "iraceResults" object will be loaded.
#> -S,--src                 Source configuration ID or the path to a file
#>                          containing the configuration. Default: 1.
#> -T,--target              Target configuration ID (by default the best
#>                          configuration found by irace) or the path to a file
#>                          containing the configuration.
#> -P,--params              Specific parameter names to be used for the ablation
#>                          (separated with commas). By default use all
#> -t,--type                Type of ablation to perform: "full" will execute each
#>                          configuration on all "--n-instances" to determine the
#>                          best-performing one; "racing" will apply racing to
#>                          find the best configurations. Default: full.
#> -n,--nrep                Number of replications per instance used in "full"
#>                          ablation. Default: 1.
#>    --seed                Integer value to use as seed for the random number
#>                          generation. Default: 1234567.
#> -o,--output-file         Log file to save the ablation log. If "", the results
#>                          are not saved to a file. Default: log-ablation.Rdata.
#>    --instances-file      Instances file used for ablation: "train", "test" or a
#>                          filename containing the list of instances. Default:
#>                          train.
#> -p,--plot                Output filename (.pdf) for the plot. If not given, no
#>                          plot is created.
#> -O,--plot-type           Type of plot. Supported values are "mean", "boxplot",
#>                          "rank" or "rank,boxplot". Default: mean.
#>    --old-path            Old path found in the log-file (.Rdata) given as input
#>                          to be replaced by --new-path.
#>    --new-path            New path to replace the path found in the log-file
#>                          (.Rdata) given as input.
#> -e,--exec-dir            Directory where the target runner will be run.
#> -s,--scenario            Scenario file to override the scenario given in the
#>                          log-file (.Rdata)
#>    --parallel            Number of calls to targetRunner to execute in
#>                          parallel. Values 0 or 1 mean no parallelization.
# Find the ablation command-line executable:
Sys.glob(file.path(system.file(package="irace", "bin"), "ablation*"))
#> [1] "/home/runner/work/_temp/Library/irace/bin/ablation"
```
