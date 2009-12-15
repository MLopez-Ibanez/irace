#!/bin/bash
###############################################################################
# This file should be ifrace-master.sh or something similar.
#
###############################################################################

usage() {
    cat <<EOF
usage: submitjobs.sh N

Parameters:
 N an integer giving the number of repetitions of F-Race

EOF
    exit 1
}

# Issue usage if no parameters are given.
test $# -ne 1 && usage

## BEGIN configuration
# Number of repetitions of F-Race
REPETITIONS=$1
# Directory with R files and TUNE_CMD
EXP=TUNE

#TUNE_CMD="qsub tune.sh"
# This command is relative to $EXP dir
TUNE_CMD="./tune.sh"

## END of configuration (uou should not need to touch what is below)

for i in $(seq 1 $REPETITIONS); do
    TRY=$(printf '%s-%002d' $EXP $i)
    echo "try = $TRY"

    ## All this should be replaced by calling hrace.R with a parameter $i
    rm -rf $TRY
    cp -r $EXP $TRY
    perl -p -i -e "s/trailNum<-1/trailNum<-$i/g" $TRY/hrace.R
    cd $TRY

    ## This should be configurable
    eval ${TUNE_CMD}
    sleep 1

    cd ..

done
