**irace**: Iterated Racing for Automatic Algorithm Configuration
================================================================

<!-- badges: start -->
[![CRAN Status](https://www.r-pkg.org/badges/version-last-release/irace)](https://cran.r-project.org/package=irace) [![CRAN Downloads](https://cranlogs.r-pkg.org/badges/grand-total/irace)](https://CRAN.R-project.org/package=irace) [![R build
status](https://github.com/MLopez-Ibanez/irace/workflows/R-CMD-check/badge.svg)](https://github.com/MLopez-Ibanez/irace/actions) 
[![Codecov test coverage](https://codecov.io/gh/MLopez-Ibanez/irace/branch/master/graph/badge.svg)](https://app.codecov.io/gh/MLopez-Ibanez/irace?branch=master)
<!-- badges: end -->
  
[ [**Homepage**](https://mlopez-ibanez.github.io/irace/) ] [ [**User Guide (PDF)**](https://cran.r-project.org/package=irace/vignettes/irace-package.pdf) ] 

**Maintainers:** [Manuel López-Ibáñez](https://lopez-ibanez.eu/), Leslie Pérez Cáceres

**Creators:** [Manuel López-Ibáñez](https://lopez-ibanez.eu/), Jérémie Dubois-Lacoste
  
**Contributors:** Jérémie Dubois-Lacoste, Thomas Stützle, Mauro Birattari, Eric
  Yuan and Prasanna Balaprakash.

**Contact:** <https://groups.google.com/d/forum/irace-package>

---------------------------------------

Introduction
============

The **irace** package implements the Iterated Race method, which is a
generalization of the Iterated F-race method for the automatic configuration of
optimization algorithms, that is, the tuning of their parameters by finding the
most appropriate settings given a set of instances of an optimization problem.
It builds upon the race package by Birattari and it is implemented in R.

**Keywords:** automatic configuration, offline tuning, parameter tuning, racing, F-race.

**Relevant literature:**

 1. M. López-Ibáñez, J. Dubois-Lacoste, L. Pérez Cáceres, T. Stützle, and
    M. Birattari. [The irace package: Iterated Racing for Automatic Algorithm Configuration](http://dx.doi.org/10.1016/j.orp.2016.09.002).
*Operations Research Perspectives*, 3:43–58, 2016.<br>
    [ [bibtex](https://lopez-ibanez.eu/LopezIbanez_bib.html#LopDubPerStuBir2016irace) 
    | doi: [10.1016/j.orp.2016.09.002](http://dx.doi.org/10.1016/j.orp.2016.09.002) ]

 2. Manuel López-Ibáñez, Jérémie Dubois-Lacoste, Thomas Stützle, and Mauro
    Birattari. [The irace package, Iterated Race for Automatic Algorithm Configuration](https://iridia.ulb.ac.be/IridiaTrSeries/link/IridiaTr2011-004.pdf).
    Technical Report TR/IRIDIA/2011-004, IRIDIA, Université libre de Bruxelles,
    Belgium, 2011.<br>
    [ [bibtex](https://iridia-ulb.github.io/references/index.html#LopDubStu2011irace)
    | [PDF](https://iridia.ulb.ac.be/IridiaTrSeries/link/IridiaTr2011-004.pdf) ]

 3. Thomas Stützle and Manuel López-Ibáñez. [Tutorial: Automated algorithm
    configuration and design](https://doi.org/10.1145/3449726.3461404). GECCO
    '21: Proceedings of the Genetic and Evolutionary Computation Conference
    Companion, July 2021.
    [doi:10.1145/3449726.3461404](https://doi.org/10.1145/3449726.3461404)


Requisites
----------

 * R (<https://www.r-project.org>) is required for running irace, but
   you don't need to know the R language to use it.


User guide
----------

A complete [user guide](https://cran.r-project.org/package=irace/vignettes/irace-package.pdf)
comes with the package. You can access it online or, after installing the irace
package, invoking from R the following command:

```R
        R> vignette("irace-package")
```

The following is a quick-start guide. The user guide gives more detailed
instructions.

Quick Start 
===========
1. Install R (with your favourite package manager, and see more details below).
2. Install irace. This command works on CMD and Powershell with R added to PATH (see detailed instructions below).
```bash
    $ Rscript -e "install.packages('irace', repos='https://cloud.r-project.org')"
```

3. Add irace to path. For windows user, this step is unfortunately more involved, so please see more detailed instructions below.
```bash
    $ export PATH="$(Rscript -e "cat(paste0(system.file(package='irace', 'bin', mustWork=TRUE), ':'))" 2> /dev/null)${PATH}"
```
Consider adding this line to your `~/.bashrc`, `~/.zshrc`, or `~/.profile` for it to persist between sessions. 

4. You can open the user guide with the following command. This command works on CMD and Powershell with R added to PATH (see detailed instructions below).
```bash
    $ Rscript -e "vignette('irace-package')"
```

Installing R
============

The official instructions are available at
<https://cran.r-project.org/doc/manuals/r-release/R-admin.html>. We give below
a quick R installation guide that will work in most cases.

GNU/Linux
---------

You should install R from your package manager. On a Debian/Ubuntu system it
will be something like:

    $ sudo apt-get install r-base

Once R is installed, you can launch R from the Terminal and from the R
prompt install the irace package. See instructions below.


OS X
----

You can install R directly from a CRAN mirror
(<https://cran.r-project.org/bin/macosx/>).

Alternatively, if you use homebrew, you can just do
```
    $ brew install --cask r
```

(Using `brew install r` is not recommended because that will build R from source and you will not be able to use any CRAN binary, possibly resulting in annoying build failiures). 

Once R is installed, you can launch R from the Terminal (or from your
Applications), and from the R prompt install the irace package. See
instructions below.

Windows
-------

You can install R from a CRAN mirror
(<https://cran.r-project.org/bin/windows/>). Once R is installed, you can
launch the R console and install the irace package from it. See instructions
below.

In addition to using the R console, it might be very useful to add R to PATH so you can run most of the GUN/Linux shell commands without modification in CMD or Powershell. Usually, R is installed in `C:\Program Files\R\R-4.1.3` (the version number depends on your installation).

You should add the following line [to PATH](https://www.architectryan.com/2018/03/17/add-to-the-path-on-windows-10/) (if you want to use the 64-bit version)
```
C:\Program Files\R\R-4.1.3\bin\x64
```

Or, if you are on a 32-bit version 
```
C:\Program Files\R\R-4.1.3\bin\i386
```

Installing the irace package
============================

There are two methods for installing the
[irace](https://mlopez-ibanez.github.io/irace/) R package on your computer:

1. Install within R (automatic download):
```R
        $ R
        R> install.packages("irace")
```
   select a mirror close to you, and test the installation with
```R
        R> library(irace)
        R> CTRL+d
```

2. Manually
   [download the package from CRAN](https://cran.r-project.org/package=irace)
   and invoke at the command-line:
```bash
        $ R CMD INSTALL <package>
```
   where `<package>` is one of the three versions available: `.tar.gz`
   (Unix/BSD/GNU/Linux), `.tgz` (MacOS X), or `.zip` (Windows).


If the package fails to install because of insufficient permissions,
you need to force a *local installation* by doing:
```bash
    $ mkdir ~/R
    $ R CMD INSTALL --library=~/R irace.tar.gz
    $ export R_LIBS=~/R:${R_LIBS}
```

Once installed, test that it is working by doing:
```R
    $ R
    R> library(irace)
    R> cat(system.file(package="irace", "bin", mustWork=TRUE), "\n")
    /home/user/R/irace/bin
```
The last command tells you the installation directory of `irace`.

GNU/Linux and OS X
------------------

Save the installation directory of `irace` to a variable, and add it to your
`.bash_profile`, `.bashrc` or `.profile`:

```bash
    export IRACE_HOME=/home/user/R/irace/bin/ # Path given by system.file(package="irace", "bin", mustWork=TRUE)
    export PATH=${IRACE_HOME}:$PATH
    # export R_LIBS=~/R:${R_LIBS} # Only if local installation was forced
```

After adding this and opening a new terminal, you should be able to
invoke `irace` as follows:

```bash
    $ irace --help
```


Windows
-------

You can find out where the irace binary is installed by running the following in Powershell or CMD:

```Powershell
    C:\> Rscript -e "cat(gsub('/', '\\\\', system.file(package='irace', 'bin', 'x64', mustWork=TRUE)))"
```

It will output a path, such as `C:\Program Files\R\R-4.1.3\library\irace\bin\x64` (replace `x64` with `i386` if you are on a 32-bit system), which can you [add to PATH](https://www.architectryan.com/2018/03/17/add-to-the-path-on-windows-10/). 

Then running the following should work:
```Powershell 
    C:\> irace --help
```

You can also launch irace by opening the R console and executing:

```R
    R> library(irace)
    R> irace.cmdline("--help")
```

GitHub (Development version)
----------------------------

If you wish to try the development version, you can install it by executing the
following commands within the R console:

```R
    R> install.packages("devtools")
    R> devtools::install_github("MLopez-Ibanez/irace")
```

Python
------

You can use the irace R package from Python using `rpy2`. An example on how to do this is the implementation of [iracepy](https://github.com/auto-optimization/iracepy).


Usage
=====

1. Create a directory for storing the tuning scenario setup
```bash
        $ mkdir ~/tuning
        $ cd ~/tuning
```

2. Initialize your tuning directory with template config files
```bash
        $ $IRACE_HOME/bin/irace --init
```

3. Modify the generated files following the instructions found within each file. In particular,
    * The scripts `target-runner` and `target-evaluator` (if you need it at all)
      should be executable. The output of `target-runner` (or
      `target-evaluator` if you use a separate evaluation step) is minimized by
      default. If you wish to maximize it, just multiply the value by `-1`
      within the script.
    * In `scenario.txt`, uncomment and assign only the parameters for which
      you need a value different than the default one.
   
    There are examples in `$IRACE_HOME/examples/`.

4. Put the instances in `~/tuning/Instances/`. In addition, you can
   create a file that specifies which instances from that directory
   should be run and which instance-specific parameters to use. See
   `scenario.txt` and `instances-list.txt` for examples. The command
   irace will not attempt to create the execution directory (`execDir`),
   so it must exist before calling irace. The default `execDir` is the
   current directory.

5. Calling the command:
```bash
        $ cd ~/tuning/ && $IRACE_HOME/bin/irace
```
   performs one run of Iterated Race. See the output of `irace --help` for
   additional irace parameters. Command-line parameters override the
   scenario setup specified in the `scenario.txt` file.


Many tuning runs in parallel
----------------------------

For executing several repetitions of irace in parallel, call the
program

```bash
    $ cd ~/tuning/ && $IRACE_HOME/bin/parallel-irace N
```

where N is the number of repetitions. By default, the execution directory of
each run of irace will be set to `./execdir-dd`, where `dd` is a number padded
with zeroes.

**Be careful**, `parallel-irace` will create these directories from
scratch, deleting them first if they already exist.

Check the help of `parallel-irace` by running it without parameters.


Parallelize one tuning
----------------------

A single run of irace can be done much faster by executing the calls
to `targetRunner` (the runs of the algorithm being tuned) in
parallel. See the [user guide](https://cran.r-project.org/package=irace/vignettes/irace-package.pdf) for the details.

License
=======

**irace** is Copyright (C) 2010-2020 Manuel López-Ibáñez, Jérémie Dubois-Lacoste
and Leslie Pérez-Cáceres.

This program is free software (software libre); you can redistribute it and/or
modify it under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2 of the License, or (at your option)
any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE. See the [GNU General Public License](https://cran.r-project.org/web/licenses/GPL-3) for more details.

**IMPORTANT NOTE:** Please be aware that the fact that this program is released as
Free Software does not excuse you from scientific propriety, which obligates
you to give appropriate credit! If you write a scientific paper describing
research that made substantive use of this program, it is your obligation as a
scientist to (a) mention the fashion in which this software was used in the
Methods section; (b) mention the algorithm in the References section. The
appropriate citation is:

 * Manuel López-Ibáñez, Jérémie Dubois-Lacoste, Leslie Pérez Cáceres, Thomas
  Stützle, and Mauro Birattari.  [**The irace package: Iterated Racing for
  Automatic Algorithm
  Configuration.**](http://dx.doi.org/10.1016/j.orp.2016.09.002) _Operations
  Research Perspectives_, 2016. doi: 10.1016/j.orp.2016.09.002

The **irace** package uses code under the GPL from the [race
package](https://CRAN.R-project.org/package=race) is Copyright (C) 2003 Mauro
Birattari.


Building an irace standalone container
======================================

Thanks to [Singularity](https://sylabs.io/singularity/), you can build a
standalone container of `irace` using the file `irace.sindef` which is
available in the directory `inst/` in the source tarball and github repository
or, after installing the irace R package, in the installation directory given
by the R expression `system.file(package="irace")`. After installing
SingularityCE, the container may be build using:

    sudo singularity build irace.sindef irace.sif
    
and run with:

    singularity run irace.sif <arguments>
    


Frequently Asked Questions
==========================

The
[user guide](https://cran.r-project.org/package=irace/vignettes/irace-package.pdf)
contains a list of frequently asked questions.

<!--
Local Variables:
mode: markdown
End:
-->
