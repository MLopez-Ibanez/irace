#!/bin/bash
set -e
set -u

cat > .excludes <<EOF
.excludes
dist.sh
TUNE*
Program
Instances/*
.svn
parameters.txt
tune-conf
*.diff
hooks/hook-run
hooks/hook-instance-finished
hooks/tune-main
EOF

BASEDIR=`pwd`
VERSION=`svnversion -n .`
VERSION=svn${VERSION}
#VERSION=`date '+%F' | tr -d '\n'`
DIST_SRC=ifrace-${VERSION}
echo "ifrace: version $VERSION"
mkdir -p ../${DIST_SRC}
rsync -rlpC --exclude-from=.excludes . ../${DIST_SRC}/
rm -f .excludes
cd ..
tar cf - ${DIST_SRC} | gzip -f9 > ${DIST_SRC}.tar.gz 
cd ${BASEDIR}
echo "${DIST_SRC}.tar.gz created."

