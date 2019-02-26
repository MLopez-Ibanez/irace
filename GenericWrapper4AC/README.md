# Generic Wrapper for Algorithm Configuration

The generic wrapper is a base class to easily implement your own interface between your algorithm and an algorithm configurator (such as ParamILS or SMAC).

The generic wrapper is developed and maintained by members of the [AutoML](http://www.automl.org) Group at the University of Freiburg and the [Beta Lab](http://www.cs.ubc.ca/labs/beta/) of the university of British Columbia. 

Status for master branch: [![Build Status](https://travis-ci.org/automl/GenericWrapper4AC.svg?branch=master)](https://travis-ci.org/automl/GenericWrapper4AC)

Status for dev branch: [![Build Status](https://travis-ci.org/automl/GenericWrapper4AC.svg?branch=dev)](https://travis-ci.org/automl/GenericWrapper4AC)

## INSTALLATION

We provide a `setup.py` script which can be used to install generic wrapper as a package
and which also installs all dependencies (including `runsolver`).

```
python setup.py install

# To test installation run afterwards
python setup.py test
```

NOTE: GenericWrapper4AC is also available on pypi but the installation of the runsolver fails using pip. We are still looking into this issue.

## USAGE

The generic wrapper provides the interfaces to ParamILS and SMAC. Please see the [documentation of SMAC](http://www.cs.ubc.ca/labs/beta/Projects/SMAC/v2.10.03/manual.pdf) for an extensive description of the required interfaces.

The script `generic_wrapper.py` provides the class `AbstractWrapper`. It provides all necessary functions to run your algorithms, including:

  * reading the input format of ParamILS and SMAC
  * limiting the resources (CPU and memory) by using the [runsolver](http://www.cril.univ-artois.fr/~roussel/runsolver/)
  * returning the output in ParamILS and SMAC format
  
You have to implement only two functions

  * `get_command_line_args()` : given the parameter configuration, the instance at hand and the random see, the function has to return the command line call of your algorithm (string)
  *  `process_results()`: given the output of your algorithm, this function has to return a dictionary including the return status of your algorithm ("SUCCESS"|"TIMEOUT"|"CRASHED"|"ABORT") and the runtime/quality/cost of your algorithm run.
  
See the docstrings and the examples for more details.

### NOTE

For debugging reasons, the generic wrapper will not delete the output files of your algorithm if your algorithm crashed (or failed in any way). If your algorithm crashes very often, this can fill up your file system quickly.

If your command line call includes "--config", the generic wrapper assumes that the new aclib call format will be used.

```
<executable> [<arg>] [<arg>] ... [--cutoff <cutoff time>] [--instance <instance name>] 
[--seed <seed>] --config [-param_name_1 value_1] [-param_name_2 value_2] ...
```

## Requirements

Since we use the `runsolver` to limit resources, the generic wrapper can only be used on Linux systems.

## Examples

Please see `./examples/` for some examples with black box functions (no problem instances included) and examples of algorithms with problem instances (i.e., SAT solving).
Each example comes with its own README with further details.

## Troubleshooting

### Runsolver
The current version of the GenericWrapper4AC uses the runsolver version 3.4.0. 
We provide a statically compiled binary. However, under certain circumstances (special kernels and so on), this static binary of the runsolver can fail.
The setup script should check whether the runsolver can be at least called successfully.
We recommend to also run the unit tests to verify that the runsolver is working properly.
If there are any issues with the runsolver, we recommend to first recompile the runsolver (see `runsolver/runsolver-3.4.0/src`).
If there are even further issues, please check whether a new version of the runsolver is available: `http://www.cril.univ-artois.fr/~roussel/runsolver/`.

## License

The generic wrapper base class is published under a BSD license -- please see LICENSE for more details.
The used runsolver was written by Olivier Roussel and is published under GPLv3.0 -- see `runsolver/` 
Please note that not all code provided in the examples are under a BSD license -- please check the license for these examples separately.

## Contributors

  * Marius Lindauer
  * Katharina Eggensperger
  * Chris Fawcett
  * Frank Hutter
