#!/bin/bash
###############################################################################
# This wrapper is used to run several irace in parallel.
###############################################################################

SEED=1234567

error () {
    echo "$0: error: $@"
    exit 1
}

usage() {
    cat <<EOF
usage: $0.sh N [EXECDIR] [IRACE PARAMS]

Parameters:
 N                    an integer giving the number of repetitions of Race
 EXECDIR             job M will use EXECDIR-M directory (default: TUNE-)
 IRACE PARAMS       (optional) parameters for IRACE
EOF
    exit 1
}

# Issue usage if no parameters are given.
test $# -ge 1 || usage

## BEGIN configuration
# Number of repetitions of Race
REPETITIONS=$1
shift

# Directory with R files and TUNE_CMD
EXECDIR=${1:-TUNE}
shift

TUNE_CMD="tune-main"
test -e ./${TUNE_CMD} || error "${TUNE_CMD} not found"
test -x ./${TUNE_CMD} || error "${TUNE_CMD} must be executable"

# Find our own location.
BINDIR=$(dirname "$(readlink -f "$(type -P $0 || echo $0)")")

## END of configuration (you should not need to touch what is below)

for i in $(seq 1 $REPETITIONS); do
    TRY=$(printf '%s-%002d' $EXECDIR $i)
    echo "execution directory = ./$TRY"
    rm -rf $TRY
    mkdir -p $TRY
    # FIXME: In fact there is a problem with the output files being
    # overwritten, specially when using qsub.
    ./${TUNE_CMD} $BINDIR $TRY --seed $SEED $* &
    sleep 1
    let SEED=SEED+1 
done
