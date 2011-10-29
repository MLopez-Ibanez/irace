VERSION = 0.99svn$(SVN_REV)
RNODE=iridiacluster
DATE=$(shell date +%F)

## Do we have svnversion?
ifeq ($(shell sh -c 'which svnversion 1> /dev/null 2>&1 && echo y'),y)
  ## Is this a working copy?
  ifneq ($(shell sh -c 'LC_ALL=C svnversion -n .'),exported)
    $(shell sh -c 'svnversion -n . > svn_version')
  endif
endif
## Set version information:
SVN_REV = $(shell sh -c 'cat svn_version 2> /dev/null')

.PHONY : default rsync dist version

default : version
	@echo "Only 'rsync' and 'dist' are meaningful"
	@exit 1

version :
	echo $(VERSION) > VERSION

rsync : version
ifdef RNODE
	rsync -rlp -CIzc -L --delete --copy-unsafe-links --exclude=.svn --progress --relative \
	.     \
	$(RNODE):~/irace/
else
	@echo "ERROR: You must specify a remote node (e.g., RNODE=majorana)"
	@exit 1
endif

dist : version
	@sed -i 's/Version: .*/Version: $(VERSION)/' ./DESCRIPTION
	@sed -i 's/Date: .*/Date: $(DATE)/' ./DESCRIPTION
	./dist.sh
