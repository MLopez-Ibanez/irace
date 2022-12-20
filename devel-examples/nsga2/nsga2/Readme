This is the Readme file for NSGA-II code.


About the Algorithm
--------------------------------------------------------------------------
NSGA-II: Non-dominated Sorting Genetic Algorithm - II

Please refer to the following paper for details about the algorithm:

Authors: Dr. Kalyanmoy Deb, Sameer Agrawal, Amrit Pratap, T Meyarivan
Paper Title: A Fast and Elitist multi-objective Genetic Algorithm: NSGA-II
Journal: IEEE Transactions on Evolutionary Computation (IEEE-TEC)
Year: 2002
Volume: 6
Number: 2
Pages: 182-197
---------------------------------------------------------------------------


---------------------------------------------------------------------------
NOTE: This archive contains routines for ploting the objective data realtime
using gnuplot. The code has been written for posix compliant operating systems
and uses standard piping method provided by GNU C library. The routines should
work on any unix and unix like OS having gnuplot installed and which are posix
compliant.
---------------------------------------------------------------------------


How to compile and run the program
---------------------------------------------------------------------------
Makefile has been provided for compiling the program on linux (and unix-like)
systems. Edit the Makefile to suit your need. By default, provided Makefile
attempts to compile and link all the existing source files into one single
executable.

Name of the executable produced is: nsga2r

To run the program type: ./nsga2r random_seed
Here random_seed is a real number in (0,1) which is used as a seed for random
number generator.
You can also store all the input data in a text file and use a redirection
operator to give the inputs to the program in a convenient way.
You may use the following syntax: ./nsga2r random_seed <inp_file.in, where
"inp_file.in" is the file that stores all the input parameters
---------------------------------------------------------------------------


About the output files
---------------------------------------------------------------------------
initial_pop.out: This file contains all the information about initial population.
final_pop.out: This file contains the data of final population.
all_pop.out: This file containts the data of populations at all generations.
best_pop.out: This file contains the best solutions obtained at the end of simulation run.
params.out: This file contains the information about input parameters as read by the program.
---------------------------------------------------------------------------


About the input parameters
---------------------------------------------------------------------------
popsize: This variable stores the population size (a multiple of 4)
ngen: Number of generations
nobj: Number of objectives
ncon: Number of constraints
nreal: Number of real variables
min_realvar[i]: minimum value of i^{th} real variable
max_realvar[i]: maximum value of i^{th} real variable
pcross_real: probability of crossover of real variable
pmut_real: probability of mutation of real variable
eta_c: distribution index for real variable SBX crossover
eta_m: distribution index for real variable polynomial mutation
nbin: number of binary variables
nbits[i]: number of bits for i^{th} binary variable
min_binvar[i]: minimum value of i^{th} binary variable
max_binvar[i]: maximum value of i^{th} binary variable
pcross_bin: probability of crossover for binary variable
pmut_bin: probability of mutation for binary variable
choice: option to display the data realtime using gnuplot
obj1, obj2, obj3: index of objectives to be shown on x, y and z axes respectively
angle1, angle2: polar and azimuthal angle required for location of eye
---------------------------------------------------------------------------


Defining the Test Problem
---------------------------------------------------------------------------
Edit the source file problemdef.c to define your test problem. Some sample
problems (24 test problems from Dr. Deb's book - Multi-Objective Optimization
using Evolutionary Algorithms) have been provided as examples to guide you
define your own objective and constraint functions. You can also link other
source files with the code depending on your need.
Following points are to be kept in mind while writing objective and constraint
functions.
1. The code has been written for minimization of objectives (min f_i). If you want to
maximize a function, you may use negetive of the function value as the objective value.
2. A solution is said to be feasible if it does not violate any of the constraints.
Constraint functions should evaluate to a quantity greater than or equal to zero
(g_j >= 0), if the solution has to be feasible. A negetive value of constraint means,
it is being violated.
3. If there are more than one constraints, it is advisable (though not mandatory)
to normalize the constraint values by either reformulating them or dividing them
by a positive non-zero constant.
---------------------------------------------------------------------------


About the files
---------------------------------------------------------------------------
global.h: Header file containing declaration of global variables and functions
rand.h: Header file containing declaration of variables and functions for random
number generator
allocate.c: Memory allocation and deallocation routines
auxiliary.c: auxiliary routines (not part of the algorithm)
crossover.c: Routines for real and binary crossover
crowddist.c: Crowding distance assignment routines
decode.c: Routine to decode binary variables
display.c: Routine to display the data realtime using gnuplot
dominance.c: Routine to perofrm non-domination checking
eval.c: Routine to evaluate constraint violation
fillnds.c: Non-dominated sorting based selection
initialize.c: Routine to perform random initialization to population members
list.c: A custom doubly linked list implementation
merge.c: Routine to merge two population into one larger population
mutation.c: Routines for real and binary mutation
nsga2r.c: Implementation of main function and the NSGA-II framework
problemdef.c: Test problem definitions
rand.c: Random number generator related routines
rank.c: Rank assignment routines
report.c: Routine to write the population information in a file
sort.c: Randomized quick sort implementation
tourselect.c: Tournament selection routine
---------------------------------------------------------------------------

Please feel free to send questions/comments/doubts/suggestions/bugs
etc. to deb@iitk.ac.in

Dr. Kalyanmoy Deb
14th June 2005
http://www.iitk.ac.in/kangal/
---------------------------------------------------------------------------
