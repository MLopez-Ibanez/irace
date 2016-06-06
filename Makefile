PACKAGEVERSION=2.0
PACKAGE=$(shell sh -c 'grep -F "Package: " DESCRIPTION | cut -f2 -d" "')
# FIXME: This Makefile only works with this BINDIR!
BINDIR=$(CURDIR)/..
RNODE=iridiacluster
RDIR=~/
INSTALL_FLAGS=
REALVERSION=$(PACKAGEVERSION).$(SVN_REV)
DATE=$(shell date +%F)
PACKAGEDIR=$(CURDIR)
FTP_COMMANDS="user anonymous anonymous\nbinary\ncd incoming\nput $(PACKAGE)_$(PACKAGEVERSION).tar.gz\nquit\n"
WINBUILD_FTP_COMMANDS="user anonymous anonymous\nbinary\ncd R-devel\nput $(PACKAGE)_$(PACKAGEVERSION).tar.gz\nquit\n"

## Do we have svnversion?
ifeq ($(shell sh -c 'which svnversion 1> /dev/null 2>&1 && echo y'),y)
  ## Is this a working copy?
  ifeq ($(shell sh -c 'LC_ALL=C svnversion -n . | grep -q ^[0-9] && echo y'),y)
    $(shell sh -c 'svnversion -n . > svn_version')
  endif
endif
## Set version information:
SVN_REV = $(shell sh -c 'cat svn_version 2> /dev/null')
REVNUM = $(shell sh -c 'cat svn_version | tr -d -c "[:digit:]" 2> /dev/null')

.PHONY : help build check clean install pdf rsync version bumpdate submit cran winbuild vignettes


install: version clean
	cd $(BINDIR) && R CMD INSTALL $(INSTALL_FLAGS) $(PACKAGEDIR)

help:
	@echo "install    install the package"
	@echo "build      build the package as a tar.gz file"
	@echo "check      build the package and run 'R CMD check'"
	@echo "rsync      copy the package and install it on $(RNODE)"
	@echo "cran       build the package and run 'R CMD check --as-cran'"
	@echo "winbuild   submit the package to the windows builder service"
	@echo "submit     submit the package to CRAN (read DEVEL-README first)"

build : bumpdate clean
	cd $(BINDIR) &&	R CMD build $(PACKAGEDIR)

closeversion:
	svn ci NEWS -m " * NEWS: Close version $(PACKAGEVERSION)"
	svn cp ^/trunk ^/tags/$(PACKAGEVERSION) -m " * Tag version $(PACKAGEVERSION)"
	svn up
	make build # again to update version.R and svn_version


releasebuild:
	cd $(BINDIR) &&	R CMD build $(PACKAGEDIR) && tar -atvf $(PACKAGE)_$(PACKAGEVERSION).tar.gz

cran : build
	cd $(BINDIR) && _R_CHECK_FORCE_SUGGESTS_=false R CMD check --as-cran $(PACKAGE)_$(PACKAGEVERSION).tar.gz

check: build
	cd $(BINDIR) && _R_CHECK_FORCE_SUGGESTS_=false R CMD check $(PACKAGE)_$(PACKAGEVERSION).tar.gz

clean: 
	cd $(PACKAGEDIR) && ($(RM) ./$(PACKAGE)-Ex.R ./src/*.o ./src/*.so; \
		find . -name '*.orig' | xargs $(RM) )

vignettes: vignettes/irace-package.Rnw
# FIXME: How to do all this on a temporary directory to avoid people editing the .tex file directly?
	cd vignettes && \
	Rscript -e "library(knitr); knit('irace-package.Rnw', output='irace-package.tex', quiet = TRUE)" && \
	pdflatex irace-package.tex && pdflatex irace-package.tex && $(RM) irace-package.tex && cp irace-package.pdf ../inst/doc/

pdf: vignettes 
	$(RM) $(BINDIR)/$(PACKAGE).pdf
	cd $(BINDIR) &&	R CMD Rd2pdf --no-preview --batch --output=$(PACKAGE).pdf $(PACKAGEDIR) 



bumpdate: version
	@sed -i 's/Date: .*/Date: $(DATE)/' $(PACKAGEDIR)/DESCRIPTION
	@sed -i 's/Date: .*$$/Date: \\tab $(DATE) \\cr/' $(PACKAGEDIR)/man/$(PACKAGE)-package.Rd

version :
	echo 'irace.version <- "$(REALVERSION)"' > $(PACKAGEDIR)/R/version.R
	@sed -i 's/Version:.*$$/Version: $(PACKAGEVERSION)/' $(PACKAGEDIR)/DESCRIPTION
	@sed -i 's/Version:.*$$/Version: \\tab $(PACKAGEVERSION) \\cr/' $(PACKAGEDIR)/man/$(PACKAGE)-package.Rd

rsync : version
ifndef RDIR
	@echo "ERROR: You must specify a remote dir (e.g., RDIR=~/)"
endif
ifdef RNODE
	rsync -rlp -CIzc -L --delete --copy-unsafe-links --exclude=.svn --exclude=/examples/ --progress --relative \
	.     \
	$(RNODE):$(RDIR)/$(PACKAGE)/
	ssh $(RNODE) "cd $(RDIR)/$(PACKAGE) && make install"
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

