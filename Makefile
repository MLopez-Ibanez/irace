PACKAGE=$(shell sh -c 'grep -F "Package: " DESCRIPTION | cut -f2 -d" "')
# FIXME: This Makefile only works with this BINDIR!
BINDIR=$(CURDIR)/..
RNODE=iridiacluster
RDIR=~/$(PACKAGE)
INSTALL_FLAGS=
MAJORVERSION=0.99
PACKAGEVERSION=$(MAJORVERSION).$(REVNUM)
VERSION=$(MAJORVERSION).$(SVN_REV)
DATE=$(shell date +%F)
PACKAGEDIR=$(CURDIR)

## Do we have svnversion?
ifeq ($(shell sh -c 'which svnversion 1> /dev/null 2>&1 && echo y'),y)
  ## Is this a working copy?
  ifneq ($(shell sh -c 'LC_ALL=C svnversion -n .'),exported)
    $(shell sh -c 'svnversion -n . > svn_version')
  endif
endif
## Set version information:
SVN_REV = $(shell sh -c 'cat svn_version 2> /dev/null')
REVNUM = $(shell sh -c 'cat svn_version | tr -d -c "[:digit:]" 2> /dev/null')

.PHONY : build check clean install pdf rsync version bumpdate

install: version clean
	cd $(BINDIR) && R CMD INSTALL $(INSTALL_FLAGS) $(PACKAGEDIR)

build : bumpdate clean
	cd $(BINDIR) &&	R CMD build $(PACKAGEDIR)

check: clean 
	cd $(BINDIR) && _R_CHECK_FORCE_SUGGESTS_=false R CMD check $(PACKAGEDIR)

clean: 
	cd $(BINDIR) && $(RM) $(PACKAGEDIR)/src/*.o $(PACKAGEDIR)/src/*.so

pdf:
	$(RM) $(BINDIR)/$(PACKAGE).pdf
	cd $(BINDIR) &&	R CMD Rd2pdf $(PACKAGEDIR)

bumpdate: version
	@sed -i 's/Date: .*/Date: $(DATE)/' $(PACKAGEDIR)/DESCRIPTION
	@sed -i 's/Date: .*/Date: \\tab $(DATE) \\cr/' $(PACKAGEDIR)/man/irace-package.Rd

version :
	echo 'irace.version <- "$(VERSION)"' > $(PACKAGEDIR)/R/version.R
	@sed -i 's/Version: .*/Version: $(PACKAGEVERSION)/' $(PACKAGEDIR)/DESCRIPTION
	@sed -i 's/Version: .*/Version: \\tab $(PACKAGEVERSION) \\cr/' $(PACKAGEDIR)/man/irace-package.Rd

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

