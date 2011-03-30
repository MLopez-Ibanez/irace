#!/bin/bash
set -e
set -u

cat > .excludes <<'EOF'
Makefile
.excludes
.*
dist.sh
TUNE*
Program
Instances/*
.svn
parameters.txt
tune-conf
*.diff
hooks/hook-run
hooks/hook-evaluate
hooks/tune-main
doc/
TODO
examples/beamaco
examples/hypervolume
web/
EOF

BASEDIR=`pwd`
VERSION=$(cat VERSION)
#VERSION=`date '+%F' | tr -d '\n'`
DIST_SRC=irace-${VERSION}
echo "irace: version $VERSION"
mkdir -p ../${DIST_SRC}
rsync -rlpC --exclude-from=.excludes . ../${DIST_SRC}/
rm -f .excludes
cd ..
(tar cf - ${DIST_SRC} | gzip -f9 > ${DIST_SRC}.tar.gz) && rm -rf ${DIST_SRC}
cd ${BASEDIR}
echo "${DIST_SRC}.tar.gz created."

