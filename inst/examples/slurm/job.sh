#!/bin/sh
#############################################################################
# Example of how to launch irace in a SLURM computing cluster. See the
# documentation of your computing cluster for the meaning of the SBATCH
# options.
#
# Launch this script with:
#   sbatch job.sh
#
# Contributed by Sarah Lee.
#############################################################################
#SBATCH --account=user_account
# All parallel processes run in the same node.
#SBATCH --nodes=1
# Number of desired cpus (SLURM_NTASKS_PER_NODE):
#SBATCH --ntasks-per-node=30
# Amount of RAM needed for this job:
#SBATCH --mem=0
# The name to show in queue lists for this job:
#SBATCH --job-name=my_irace_job
#SBATCH --mail-user=user@example.com
# The time the job will be running:
#SBATCH --time=12:00:00
# Set output and error files
#SBATCH --output=/mypath/irace-%j.out
#SBATCH --error=/mypath/irace-%j.err

# To load some software (you can show the list with 'module avail'):
module load r
# You may need to load more modules depending on your application.

## Option 1. Call the irace executable:
~/bin/irace --exec-dir=~/local/irace-execdir/ --parallel $((SLURM_NTASKS_PER_NODE))
# I usually add a symbolic link from the location where the irace executable is installed to ~/bin/irace. Otherwise, replace ~/bin/irace with the path to the irace executable. It will be something like: ~/local/R_libs/4.1/irace/bin/irace. How to find this path is explained in the user guide and in the quick-install intro: https://mlopez-ibanez.github.io/irace/#installing-the-irace-package
# You can add additional irace options to the line above (see the user-guide), for example --scenario "tuning-scenario.txt". I added --exec-dir above but you can define the execDir in the scenario file instead. It is important that it is a location that is available for writing from the computing nodes. You may also want to redirect the output of job.sh to some file you can read later. How to do that should be explained in the documentation of your cluster. Also, make sure that you are using the same R version when installing irace than the version that you are loading within job.sh. Otherwise, you may get strange errors.

# MPI: If you want to use mpi, you may need to add something like:
mpirun -np 1 ~/bin/irace --exec-dir=~/local/irace-execdir/ --parallel $((SLURM_NTASKS-1)) --mpi 1
# $SLURM_NTASKS is expanded automatically to the value you give to --ntasks=. We subtract one from that value because irace itself consumes one task.

## Option 2: Using an R script
cd /path_to_irace_R_script
Rscript ./run_irace.R
