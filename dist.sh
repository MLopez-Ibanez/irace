#!/bin/bash
set -e
set -u

BASEDIR=`pwd`
VERSION=$(cat VERSION)
#VERSION=`date '+%F' | tr -d '\n'`
DIST_SRC=irace-${VERSION}
echo "irace: version $VERSION"
mkdir -p ../${DIST_SRC}
rsync -rlpC --exclude-from=.Rbuildignore . ../${DIST_SRC}/
cd ..
(tar cf - ${DIST_SRC} | gzip -f9 > ${DIST_SRC}.tar.gz) && rm -rf ${DIST_SRC}
cd ${BASEDIR}
echo "${DIST_SRC}.tar.gz created."

