#!/bin/bash
###############################################################################
# This wrapper is used to run several irace in parallel.
###############################################################################

SEED=1234567

error () {
    echo "$0: error: $@" >&2
    exit 1
}

usage() {
    cat <<EOF
usage: $0.sh N[-M] [EXECDIR] [IRACE PARAMS]

Parameters:
 N                  an integer giving the number of repetitions of Race
                    or a sequence N-M giving which repetitions to redo.
 EXECDIR            job M will use EXECDIR-M directory (default: TUNE-).
 IRACE PARAMS       (optional) parameters for IRACE.
EOF
    exit 1
}

# Issue usage if no parameters are given.
test $# -ge 1 || usage

## BEGIN configuration
# Number of repetitions of Race
REPETITIONS=$1
shift
START=1

if [[ "$REPETITIONS" =~ ^([0-9]+)-([0-9]+)$ ]] ; then
    START=${BASH_REMATCH[1]}
    REPETITIONS=${BASH_REMATCH[2]}
elif ! [[ "$REPETITIONS" =~ ^[0-9]+$ ]] ; then
    error "number of repetitions must be an integer"
fi

# execDir (--exec-dir) directory
EXECDIR=${1:-TUNE}
shift

IRACE_MAIN="tune-main"
test -e ./${IRACE_MAIN} || error "${IRACE_MAIN} not found"
test -x ./${IRACE_MAIN} || error "${IRACE_MAIN} must be executable"

# Find our own location.
BINDIR=$(dirname "$(readlink -f "$(type -P $0 || echo $0)")")

## END of configuration (you should not need to touch what is below)

for i in $(seq $START $REPETITIONS); do
    TRY=$(printf '%s-%002d' $EXECDIR $i)
    echo "execution directory = ./$TRY"
    rm -rf $TRY
    mkdir -p $TRY
    # FIXME: In fact there is a problem with the output files being
    # overwritten, specially when using qsub.
    let s=SEED+i-1 
    ./${IRACE_MAIN} $BINDIR $TRY --seed $s $* &
    sleep 1
done
