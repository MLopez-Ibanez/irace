PACKAGE=irace
# FIXME: This Makefile only works in BINDIR=.. !
BINDIR=..
RNODE=iridiacluster
RDIR=~/$(PACKAGE)
INSTALL_FLAGS=
PACKAGEVERSION=0.99
VERSION = $(PACKAGEVERSION)r$(SVN_REV)
DATE=$(shell date +%F)
PACKAGEDIR=$(PWD)

## Do we have svnversion?
ifeq ($(shell sh -c 'which svnversion 1> /dev/null 2>&1 && echo y'),y)
  ## Is this a working copy?
  ifneq ($(shell sh -c 'LC_ALL=C svnversion -n .'),exported)
    $(shell sh -c 'svnversion -n . > svn_version')
  endif
endif
## Set version information:
SVN_REV = $(shell sh -c 'cat svn_version 2> /dev/null')

.PHONY : build check clean install pdf rsync version

install: version clean
	cd $(BINDIR) && R CMD INSTALL $(INSTALL_FLAGS) $(PACKAGEDIR)

build : version clean
	cd $(BINDIR) &&	R CMD build $(PACKAGEDIR)

check: clean 
	cd $(BINDIR) && _R_CHECK_FORCE_SUGGESTS_=false R CMD check $(PACKAGEDIR)

clean: 
	cd $(BINDIR) && $(RM) $(PACKAGEDIR)/src/*.o $(PACKAGEDIR)/src/*.so

pdf:
	$(RM) $(BINDIR)/$(PACKAGE).pdf
	cd $(BINDIR) &&	R CMD Rd2pdf $(PACKAGEDIR)

version :
	echo 'irace.version <- "$(VERSION)"' > $(PACKAGEDIR)/R/version.R
	@sed -i 's/Date: .*/Date: $(DATE)/' $(PACKAGEDIR)/DESCRIPTION
	@sed -i 's/Version: .*/Version: $(PACKAGEVERSION)/' $(PACKAGEDIR)/DESCRIPTION

rsync : version
ifdef RNODE
	rsync -rlp -CIzc -L --delete --copy-unsafe-links --exclude=.svn --progress --relative \
	.     \
	$(RNODE):$(RDIR)/
	ssh $(RNODE) "cd $(RDIR)/ && make install"
else
	@echo "ERROR: You must specify a remote node (e.g., RNODE=majorana)"
	@exit 1
endif

