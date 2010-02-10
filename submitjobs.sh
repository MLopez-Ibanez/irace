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

TUNE_CMD="tune-main"
# Find our own location.
BINDIR=$(dirname "$(readlink -f "$(type -P $0 || echo $0)")")

## END of configuration (you should not need to touch what is below)

for i in $(seq 1 $REPETITIONS); do
    TRY=$(printf '%s-%002d' $EXP $i)
    echo "execution directory = ./$TRY"
    rm -rf $TRY
    mkdir -p $TRY
    test -x ./${TUNE_CMD} || error "${TUNE_CMD} must be executable"
    # FIXME: In fact there is a problem with the output files being
    # overwritten, specially when using qsub.
    ./${TUNE_CMD} $BINDIR $TRY $*
    sleep 1
done
