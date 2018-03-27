**irace**: Iterated Racing for Automatic Algorithm Configuration
================================================================

[![CRAN Status](https://www.r-pkg.org/badges/version-last-release/irace)](https://cran.r-project.org/package=irace) [![CRAN Downloads](https://cranlogs.r-pkg.org/badges/grand-total/irace)](https://CRAN.R-project.org/package=irace) 

[ [**Homepage**](http://iridia.ulb.ac.be/irace/) ] [ [**User Guide (PDF)**](https://cran.r-project.org/package=irace/vignettes/irace-package.pdf) ] 

**Maintainers:** [Manuel López-Ibáñez](http://lopez-ibanez.eu), Leslie Pérez Cáceres

**Creators:** [Manuel López-Ibáñez](http://lopez-ibanez.eu), Jérémie Dubois-Lacoste
  
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
    M. Birattari. [The irace package: Iterated Racing for Automatic Algorithm Configuration.](http://dx.doi.org/10.1016/j.orp.2016.09.002).
*Operations Research Perspectives*, 3:43–58, 2016.<br>
    [ [bibtex](http://lopez-ibanez.eu/LopezIbanez_bib.html#LopDubPerStuBir2016irace) 
    |
    doi:[10.1016/j.orp.2016.09.002](http://dx.doi.org/10.1016/j.orp.2016.09.002) ]

 2. Manuel López-Ibáñez, Jérémie Dubois-Lacoste, Thomas Stützle, and Mauro
    Birattari. [The irace package, Iterated Race for Automatic Algorithm Configuration](http://iridia.ulb.ac.be/IridiaTrSeries/IridiaTr2011-004.pdf).
    Technical Report TR/IRIDIA/2011-004, IRIDIA, Université libre de Bruxelles,
    Belgium, 2011.<br>
    [ [bibtex](http://iridia.ulb.ac.be/~manuel/LopezIbanez_bib.html#LopDubStu2011irace)
    | [PDF](http://iridia.ulb.ac.be/IridiaTrSeries/IridiaTr2011-004.pdf) ]

 3. Manuel López-Ibáñez. [The irace
    software package: A tutorial](http://iridia.ulb.ac.be/irace/files/irace-comex-tutorial.pdf). COMEX Workshop on Practical Automatic Algorithm Configuration, 2014.<br>
    [ [workshop webpage](http://iridia.ulb.ac.be/~manuel/comex_workshop/)
    | [PDF](http://iridia.ulb.ac.be/irace/files/irace-comex-tutorial.pdf) ]


Requisites
----------

 * R (<https://www.r-project.org>) is required for running irace, but
   you don't need to know the R language to use it.
   Versions that work: >= 2.15.0

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

Alternatively, if you use homebrew, you can just brew the R formula
from the science tap (unfortunately it does not come already bottled
so you need to have Xcode installed to compile it):

```bash
    $ brew tap homebrew/science
    $ brew install r
```

Once R is installed, you can launch R from the Terminal (or from your
Applications), and from the R prompt install the irace package. See
instructions below.

Windows
-------

You can install R from a CRAN mirror
(<https://cran.r-project.org/bin/windows/>). Once R is installed, you can
launch the R console and install the irace package from it. See instructions
below.


Installing the irace package
============================

There are two methods for installing the [irace](http://iridia.ulb.ac.be/irace) R package on your
computer:

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
    R> system.file(package="irace")
    [1] "~/R/irace"
```
The last command tells you the installation directory of `irace`.

GNU/Linux and OS X
------------------

Save the installation directory of `irace` to a variable, and add it to your
`.bash_profile`, `.bashrc` or `.profile`:

```bash
    export IRACE_HOME=~/R/irace/ # Path given by system.file(package="irace")
    export PATH=${IRACE_HOME}/bin/:$PATH
    # export R_LIBS=~/R:${R_LIBS} # Only if local installation was forced
```

After adding this and opening a new terminal, you should be able to
invoke `irace` as follows:

```bash
    $ irace --help
```


Windows
-------

If the installation directory of `irace` is `C:/R/irace/`, you can invoke
`irace` by opening a terminal (launch the program `cmd.exe`) and executing:

```bash
    C:\> C:\R\irace\bin\irace.bat --help
```

You can also launch irace by opening the R console and executing:

```R
    R> library(irace)
    R> irace.cmdline("--help")
```

Usage
=====

1. Create a directory for storing the tuning scenario setup
```bash
        $ mkdir ~/tuning
        $ cd ~/tuning
```

2. Copy the template and example files to the scenario directory
```bash
        $ cp $IRACE_HOME/templates/*.tmpl .
```
    where `$IRACE_HOME` is the path to the installation directory of
    `irace`. It can be obtained by doing:

```R
        $ R
        > library(irace)
        > system.file(package="irace")
```

3. For each template in your tuning directory, remove the `.tmpl`
   suffix, and modify them following the instructions in each file. In
   particular,
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
   `scenario.txt.tmpl` and `instances-list.tmpl` for examples. The command
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

where N is the number of repetitions. By default, the execution
directory of each run of irace will be set to `./execdir-dd`, where `dd` is a
number padded with zeroes.

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

This software is Copyright (C) 2011-2017 Manuel López-Ibáñez and Jérémie
Dubois-Lacoste.

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

> Manuel López-Ibáñez, Jérémie Dubois-Lacoste, Leslie Pérez Cáceres, Thomas
> Stützle, and Mauro Birattari.
> [**The irace package: Iterated Racing for Automatic Algorithm Configuration.**](http://dx.doi.org/10.1016/j.orp.2016.09.002)
> _Operations Research Perspectives_, 2016.

The race package is Copyright (C) 2003 Mauro Birattari, used under the GPL.

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
