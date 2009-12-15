#!/bin/bash
set -e
set -u
BASEDIR=`pwd`
VERSION=`svnversion -n .`
VERSION=svn${VERSION}
#VERSION=`date '+%F' | tr -d '\n'`
DIST_SRC=ifrace-${VERSION}
echo "ifrace: version $VERSION"
mkdir -p ../${DIST_SRC}
rsync -rlpC --exclude=dist.sh --exclude=Program --exclude='Instances/*' --exclude=.svn . ../${DIST_SRC}/
cd ..
tar cf - ${DIST_SRC} | gzip -f9 > ${DIST_SRC}.tar.gz 
cd ${BASEDIR}
echo "${DIST_SRC}.tar.gz created."

