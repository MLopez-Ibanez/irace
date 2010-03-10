#!/bin/bash
###############################################################################
# FIXME: Describe this file.
#
###############################################################################

error () {
    echo "$0: error: $@"
    exit 1
}

usage() {
    cat <<EOF
usage: $0 N [EXECDIR] [IFRACE PARAMS]

Parameters:
 N                    an integer giving the number of repetitions of F-Race
 EXECDIR             job M will use EXECDIR-M directory (default: TUNE-)
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
EXECDIR=${1:-TUNE}
shift

TUNE_CMD="tune-main"
# Find our own location.
BINDIR=$(dirname "$(readlink -f "$(type -P $0 || echo $0)")")

## END of configuration (you should not need to touch what is below)

for i in $(seq 1 $REPETITIONS); do
    TRY=$(printf '%s-%002d' $EXECDIR $i)
    rm -rf $TRY
    mkdir -p $TRY
    test -x ./${TUNE_CMD} || error "${TUNE_CMD} must be executable"
    echo "ifrace: ./${TUNE_CMD} $BINDIR $TRY $*"
    # Run in parallel
    ./${TUNE_CMD} $BINDIR $TRY $* &
    sleep 1
done
