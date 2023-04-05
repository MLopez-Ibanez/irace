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
cd /path_to_irace_R_script
Rscript ./run_irace.R
