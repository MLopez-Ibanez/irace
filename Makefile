PACKAGE=$(shell sh -c 'grep -F "Package: " DESCRIPTION | cut -f2 -d" "')
# FIXME: This Makefile only works with this BINDIR!
BINDIR=$(CURDIR)/..
RNODE=iridiacluster
RDIR=~/$(PACKAGE)
INSTALL_FLAGS=
PACKAGEVERSION=1.03
REALVERSION=$(PACKAGEVERSION).$(SVN_REV)
DATE=$(shell date +%F)
PACKAGEDIR=$(CURDIR)
FTP_COMMANDS="user anonymous anonymous\nbinary\ncd incoming\nput $(PACKAGE)_$(PACKAGEVERSION).tar.gz\nquit\n"
WINBUILD_FTP_COMMANDS="user anonymous anonymous\nbinary\ncd R-devel\nput $(PACKAGE)_$(PACKAGEVERSION).tar.gz\nquit\n"

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

.PHONY : build check clean install pdf rsync version bumpdate submit cran winbuild

install: version clean
	cd $(BINDIR) && R CMD INSTALL $(INSTALL_FLAGS) $(PACKAGEDIR)

build : bumpdate clean
	cd $(BINDIR) &&	R CMD build $(PACKAGEDIR)

cran : build
	cd $(BINDIR) && _R_CHECK_FORCE_SUGGESTS_=false R CMD check --as-cran $(PACKAGE)_$(PACKAGEVERSION).tar.gz

check: clean
	cd $(BINDIR) && _R_CHECK_FORCE_SUGGESTS_=false R CMD check $(PACKAGEDIR)

clean: 
	cd $(BINDIR) && $(RM) $(PACKAGEDIR)/src/*.o $(PACKAGEDIR)/src/*.so

pdf:
	$(RM) $(BINDIR)/$(PACKAGE).pdf
	cd $(BINDIR) &&	R CMD Rd2pdf --no-preview --batch --output=$(PACKAGE).pdf $(PACKAGEDIR) 

bumpdate: version
	@sed -i 's/Date: .*/Date: $(DATE)/' $(PACKAGEDIR)/DESCRIPTION
	@sed -i 's/Date: .*$$/Date: \\tab $(DATE) \\cr/' $(PACKAGEDIR)/man/irace-package.Rd

version :
	echo 'irace.version <- "$(REALVERSION)"' > $(PACKAGEDIR)/R/version.R
	@sed -i 's/Version:.*$$/Version: $(PACKAGEVERSION)/' $(PACKAGEDIR)/DESCRIPTION
	@sed -i 's/Version:.*$$/Version: \\tab $(PACKAGEVERSION) \\cr/' $(PACKAGEDIR)/man/irace-package.Rd

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

submit: 
	@echo "Read http://cran.r-project.org/web/packages/policies.html"
	cd $(BINDIR) && echo $(FTP_COMMANDS) | ftp -v -e -g -i -n cran.r-project.org
	@echo "Don't forget to send email to cran@r-project.org !"

winbuild:
	@echo "Winbuild: http://win-builder.r-project.org/"
	cd $(BINDIR) && echo $(WINBUILD_FTP_COMMANDS) | ftp -v -p -e -g -i -n win-builder.r-project.org

