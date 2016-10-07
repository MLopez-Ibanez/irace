---
title: "irace: Iterated Race for Automatic Algorithm Configuration
output:
  html_document:
    theme: journal
    toc: true
    toc_depth: 4
---
[![CRAN Downloads](http://cranlogs.r-pkg.org/badges/grand-total/irace)](https://CRAN.R-project.org/package=irace)

**Maintainers:** [Manuel López-Ibáñez](http://lopez-ibanez.eu), Jérémie Dubois-Lacoste, Leslie Pérez

**Contact:** <https://groups.google.com/d/forum/irace-package>

Introduction
------------

The irace package implements the Iterated Race method, which is a
generalization of the Iterated F-race method for the automatic configuration of
optimization algorithms, that is, the tuning of their parameters by finding the
most appropriate settings given a set of instances of an optimization problem.
It builds upon the race package by Birattari and it is implemented in R.

**Keywords**: automatic configuration, offline tuning, parameter tuning, racing, F-race.

Relevant literature:

 1. M. López-Ibáñez, J. Dubois-Lacoste, L. Pérez Cáceres, T. Stützle, and
    M. Birattari. [**The irace package: Iterated Racing for Automatic Algorithm Configuration.**](http://dx.doi.org/10.1016/j.orp.2016.09.002).
    *Operations Research Perspectives*, 2016.  doi:[10.1016/j.orp.2016.09.002](http://dx.doi.org/10.1016/j.orp.2016.09.002).

 2. Manuel López-Ibáñez, Jérémie Dubois-Lacoste, Thomas Stützle, and Mauro
    Birattari. [**The irace package, Iterated Race for Automatic Algorithm Configuration**](http://iridia.ulb.ac.be/IridiaTrSeries/IridiaTr2011-004.pdf).
    Technical Report TR/IRIDIA/2011-004, IRIDIA, Université libre de Bruxelles,
    Belgium, 2011.<br>
    [ [bibtex](http://iridia.ulb.ac.be/~manuel/LopezIbanez_bib.html#LopDubStu2011irace)
    | [PDF](http://iridia.ulb.ac.be/IridiaTrSeries/IridiaTr2011-004.pdf) ]

 3. Manuel López-Ibáñez. [**The irace
    software package: A tutorial**](http://iridia.ulb.ac.be/irace/files/irace-comex-tutorial.pdf). COMEX Workshop on Practical Automatic Algorithm Configuration, 2014.<br>
    [ [workshop webpage](http://iridia.ulb.ac.be/~manuel/comex_workshop/)
    | [PDF](http://iridia.ulb.ac.be/irace/files/irace-comex-tutorial.pdf) ]


Requisites
----------

 * R (<http://www.r-project.org>) is required for running irace, but
   you don't need to know the R language to use it.
   Versions that work: >= 2.15.0
   (See Installing R below for a quick install guide)

 * If you wish to use the command-line wrappers `irace` and
   `parallel-irace`, they require GNU Bash.


Installing R
------------

The official instructions are available at
<https://cran.r-project.org/doc/manuals/r-release/R-admin.html>

We give below a quick installation guide that will work in most cases.

### GNU/Linux ###

You should install R from your package manager. On a Debian/Ubuntu system it
will be something like:

    $ sudo apt-get install r-base

Once R is installed, you can launch R from the Terminal and from the R
prompt install the irace package. See instructions below.


### OS X ###

You can install R directly from a CRAN mirror
(<https://cran.r-project.org/bin/macosx/>)

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

### Windows ###

You can install R from a CRAN mirror
(<https://cran.r-project.org/bin/windows/>)

Once R is installed, you can launch the R console and install the
irace package from it. See instructions below.


Installing the irace package
----------------------------

Install the [irace](http://iridia.ulb.ac.be/irace) R package on your
computer. There are two methods:

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

1. Manually
   [download the package from CRAN](https://cran.r-project.org/package=irace)
   and invoke at the command-line:

        $ R CMD INSTALL <package>

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

### GNU/Linux and OS X ###

The last command tells you the installation directory of `irace`. Save
that path to a variable, and add it to your `.bash_profile`, `.bashrc`
or `.profile`:

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


### Windows ###

Unfortunately, the command-line wrapper does not work in Windows. To
launch irace, you need to open the R console and execute:

```R
    R> library(irace)
    R> irace.cmdline("--help")
```

Usage
-------

1. Create a directory for storing the tuning scenario setup

```bash
        $ mkdir ~/tuning
        $ cd ~/tuning
```

1. Copy the template and example files to the scenario directory
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

1. For each template in your tuning directory, remove the `.tmpl`
   suffix, and modify them following the instructions in each file. In
   particular,
    * The scripts `target-runner` and `targetEvaluator` (if you need it at all)
      should be executable. The output of `target-runner` (or
      `targetEvaluator` if you use a separate evaluation step) is minimized by
      default. If you wish to maximize it, just multiply the value by `-1`
      within the script.
    * In `scenario.txt`, uncomment and assign only the parameters for which
      you need a value different than the default one.
   
    There are examples in `$IRACE_HOME/examples/`.

2. Put the instances in `~/tuning/Instances/`. In addition, you can
   create a file that specifies which instances from that directory
   should be run and which instance-specific parameters to use. See
   `scenario.txt.tmpl` and `instances-list.tmpl` for examples. The command
   irace will not attempt to create the execution directory (execDir),
   so it must exist before calling irace. The default execDir is the
   current directory.

3. Calling the command:
```bash
        $ cd ~/tuning/ && $IRACE_HOME/bin/irace
```
    performs one run of Iterated Race. See the output of `irace --help` for
    additional irace parameters. Command-line parameters override the
    scenario setup specified in the scenario.txt file.


### Many tuning runs in parallel ###

For executing several repetitions of irace in parallel, call the
program

```bash
    $ cd ~/tuning/ && $IRACE_HOME/bin/parallel-irace N
```

where N is the number of repetitions. By default, the execution
directory of each run of irace will be set to `./execdir-dd`, where dd is a
number padded with zeroes.

**Be careful**, `parallel-irace` will create these directories from
scratch, deleting them first if they already exist.

Check the help of `parallel-irace` by running it without parameters.


### Parallelize one tuning ###


A single run of irace can be done much faster by executing the calls
to `targetRunner` (the runs of the algorithm being tuned) in
parallel. There are three ways to parallelize a single run of irace.

1. Parallel processes: The option `--parallel N` will use the
   `parallel` package to launch *locally* up to N calls of `targetRunner` in
   parallel.

2. MPI: The option `--mpi 1 --parallel N` will use the Rmpi package to
   launch N slaves + 1 master, in order to execute N calls of
   `targetRunner` in parallel. The user is responsible to set up MPI
   correctly.

    An example script for using MPI mode in an SGE cluster is given at
    `$IRACE_HOME/examples/mpi/`.

3. SGE cluster: This mode requires Grid Engine commands qsub and
   qstat. The command qsub should return a message that contains the
   string: `Your job JOBID`. The command `qstat -j JOBID` should return
   nonzero if JOBID has finished, otherwise it should return zero.

    The option `--sge-cluster 1` will launch as many calls of `targetRunner` as
    possible and use `qstat` to wait for cluster jobs. The user *must* call
    `qsub` from `targetRunner` with the appropriate settings for their cluster,
    otherwise `targetRunner` will not submit jobs to the cluster. In this mode,
    irace must run in the submission node, and hence, qsub should not be used
    to invoke irace.  You also need to create a separate targetEvaluator script
    to parse the results of the targetRunner and return them to irace. See the
    examples in `$IRACE_HOME/examples/sge-cluster/`.


Frequently Asked Questions
--------------------------

#### Is irace minimizing or maximizing the output of my algorithm? ####

By default, irace considers that the value returned by `target-runner` (or by
`targetEvaluator`, if used) should be *minimized*. In case of a maximization
problem, one can simply multiply the value by -1 before returning it to
irace. This is done, for example, when maximizing the hypervolume (see the last
lines in `$IRACE_HOME/examples/hypervolume/target-evaluator`).


#### Is it possible to configure a MATLAB algorithm with irace?  ####

Definitely. There are two main ways to achieve this:

1. Edit the target-runner script to call MATLAB in a non-interactive
   way. See the MATLAB documentation, or the following links:<br>
   <http://stackoverflow.com/questions/1518072/suppress-start-message-of-matlab/><br>
   <http://stackoverflow.com/questions/4611195/how-to-call-matlab-from-command-line-and-print-to-stdout-before-exiting>

    You would need to pass the parameter received by target-runner to your MATLAB
    script:
    <http://www.mathworks.nl/support/solutions/en/data/1-1BS5S/?solution=1-1BS5S>
    There is a minimal example in `$IRACE_HOME/examples/matlab/`.

2. Call MATLAB code directly from R using the
   [R.matlab package](https://cran.r-project.org/package=R.matlab). This
   is a better option if you are experienced in R. Define targetRunner as
   an R function instead of a path to a script. The function should
   call your MATLAB code with appropriate parameters.


#### My program works perfectly on its own, but not when running under irace. Is irace broken?  ####

Every time this was reported, it was a difficult-to-reproduce bug in
the program, not in irace.  We recommend that in `target-runner`, you use
`valgrind` to run your program. That is, if you program is called like:

```bash
    $EXE ${FIXED_PARAMS} -i $INSTANCE ${CAND_PARAMS} 1> ${STDOUT} 2> ${STDERR}
```

then replace that line with:

```bash
    valgrind --error-exitcode=1 $EXE ${FIXED_PARAMS} -i $INSTANCE ${CAND_PARAMS} 1> ${STDOUT} 2> ${STDERR}
```

If there are bugs in your program, they will appear in `${STDERR}`,
thus do not delete those files.


#### My program may be buggy and run into an infinite loop. Is it possible to set a maximum timeout?  ####

We are not aware of any way to achieve this using R. However, in
GNU/Linux, it is easy to implement by using the `timeout` command in
`target-runner` when invoking your program.


Contact
-------

If you find that you need to modify anything else, something is not
clear or if you have problems, please contact irace@iridia.ulb.ac.be


<!--
Local Variables:
mode: markdown
End:
-->
