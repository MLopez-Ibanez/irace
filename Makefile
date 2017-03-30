PACKAGEVERSION=2.3
PACKAGE=$(shell sh -c 'grep -F "Package: " DESCRIPTION | cut -f2 -d" "')
# FIXME: This Makefile only works with this BINDIR!
BINDIR=$(CURDIR)/..
RNODE=iridiacluster
RDIR=~/
INSTALL_FLAGS=
BUILD_FLAGS=
REALVERSION=$(PACKAGEVERSION).$(SVN_REV)
DATE=$(shell date +%F)
PACKAGEDIR=$(CURDIR)
FTP_COMMANDS="user anonymous anonymous\nbinary\ncd incoming\nput $(PACKAGE)_$(PACKAGEVERSION).tar.gz\nquit\n"
WINBUILD_FTP_COMMANDS="user anonymous anonymous\nbinary\ncd R-release\nput $(PACKAGE)_$(PACKAGEVERSION).tar.gz\nquit\n"
PDFLATEX=pdflatex -shell-escape -file-line-error -halt-on-error -interaction=nonstopmode "\input"

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


install:
	$(MAKE) build
	cd $(BINDIR) && R CMD INSTALL $(INSTALL_FLAGS) $(PACKAGE)_$(PACKAGEVERSION).tar.gz

help:
	@echo "install    install the package"
	@echo "build      build the package as a tar.gz file"
	@echo "check      build the package and run 'R CMD check'"
	@echo "rsync      copy the package and install it on $(RNODE)"
	@echo "cran       build the package and run 'R CMD check --as-cran'"
	@echo "winbuild   submit the package to the windows builder service"
	@echo "submit     submit the package to CRAN (read DEVEL-README first)"

build : bumpdate clean
	cd $(PACKAGEDIR)/vignettes \
	&& sed -i 's/^%\+\\setboolean{Release}{true}/\\setboolean{Release}{true}/' $(PACKAGE)-package.Rnw \
	&& aux2bib irace-package.aux | grep -v '@comment' > irace-package.bib
	@if grep -q @ $(PACKAGEDIR)/vignettes/irace-package.bib; then true; \
	else echo "error: vignettes/irace-package.bib is empty: run 'make vignettes'"; false; fi
	cd $(BINDIR) &&	R CMD build $(BUILD_FLAGS) $(PACKAGEDIR)

closeversion: build
	svn ci -m " * NEWS: Close version $(PACKAGEVERSION)"
	svn rm ^/tags/$(PACKAGEVERSION) -m " * Delete previous tag for version $(PACKAGEVERSION)" || echo "OK: tag is new."
	svn cp ^/trunk ^/tags/$(PACKAGEVERSION) -m " * Tag version $(PACKAGEVERSION)"
	svn up
	make releasebuild # again to update version.R and svn_version


releasebuild: BUILD_FLAGS=--compact-vignettes=both
releasebuild:
	cd $(BINDIR) &&	R CMD build $(BUILD_FLAGS) $(PACKAGEDIR) && tar -atvf $(PACKAGE)_$(PACKAGEVERSION).tar.gz

cran : BUILD_FLAGS=--compact-vignettes=both
cran : build
	cd $(BINDIR) && _R_CHECK_FORCE_SUGGESTS_=false R CMD check --as-cran $(PACKAGE)_$(PACKAGEVERSION).tar.gz

check: build
	cd $(BINDIR) && _R_CHECK_FORCE_SUGGESTS_=false R CMD check $(PACKAGE)_$(PACKAGEVERSION).tar.gz

clean: 
	cd $(PACKAGEDIR) && ($(RM) ./$(PACKAGE)-Ex.R ./src/*.o ./src/*.so; \
		find . -name '*.orig' | xargs $(RM) )

vignettes: version vignettes/irace-package.Rnw vignettes/irace-package.bib
# FIXME: How to do all this on a temporary directory to avoid people editing the .tex file directly?
# R CMD Sweave --pdf --clean --verbose irace-package.Rnw
# but then there is no output, not sure if it uses knit or it is uses bibtex...
	cd $(PACKAGEDIR)/vignettes \
	&& sed -i 's/^\\setboolean{Release}{true}/%\\setboolean{Release}{true}/' $(PACKAGE)-package.Rnw ; \
	Rscript -e "library(knitr); knit('irace-package.Rnw', output='irace-package.tex', quiet = TRUE)" \
	&& $(PDFLATEX) irace-package.tex && bibtex irace-package && $(PDFLATEX) irace-package.tex && $(PDFLATEX) irace-package.tex && $(RM) irace-package.tex

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
	@sed -i 's/\\iraceversion}{.*}$$/\\iraceversion}{$(PACKAGEVERSION)}/' vignettes/$(PACKAGE)-package.Rnw

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
	@echo "*** You need to use http://cran.r-project.org/submit.html"
	@exit 1
	cd $(BINDIR) && echo $(FTP_COMMANDS) | ftp -v -e -g -i -n cran.r-project.org
	@echo "Don't forget to send email to cran@r-project.org !"

winbuild:
	@echo "Winbuild: http://win-builder.r-project.org/"
	cd $(BINDIR) && echo $(WINBUILD_FTP_COMMANDS) | ftp -v -p -e -g -i -n win-builder.r-project.org

