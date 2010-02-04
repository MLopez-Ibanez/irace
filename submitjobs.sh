#!/bin/bash
###############################################################################
# This file should be ifrace-master.sh or something similar.
#
###############################################################################

error () {
    echo "submitjobs.sh: error: $@"
    exit 1
}

usage() {
    cat <<EOF
usage: submitjobs.sh N [IFRACE PARAMS]

Parameters:
 N                    an integer giving the number of repetitions of F-Race
 IFRACE PARAMS       (optional) parameters for IFRACE
EOF
    exit 1
}

# Issue usage if no parameters are given.
test $# -ge 1 || usage

## BEGIN configuration
# Number of repetitions of F-Race
REPETITIONS=$1
shift
# Directory with R files and TUNE_CMD
EXP=TUNE

TUNE_CMD="hooks/tune-main"

## END of configuration (uou should not need to touch what is below)

for i in $(seq 1 $REPETITIONS); do
    TRY=$(printf '%s-%002d' $EXP $i)
    echo "try = $TRY"
    rm -rf $TRY
    mkdir -p $TRY
    ## FIXME: In fact trailNum is not in hrace.R but in eval.R and is
    ## never used, so all this can be deleted.
    #perl -p -i -e "s/trailNum<-1/trailNum<-$i/g" $TRY/hrace.R
    #cd $TRY
    test -x ./${TUNE_CMD} || error "${TUNE_CMD} must be executable"
    ./${TUNE_CMD} $TRY $*
    sleep 1

done
