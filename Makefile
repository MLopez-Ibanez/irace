PACKAGEVERSION=2.5
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

.PHONY : help build check clean install pdf rsync version bumpdate submit cran winbuild vignettes examples

help:
	@echo "install    install the package"
	@echo "build      build the package as a tar.gz file"
	@echo "check      build the package and run 'R CMD check'"
	@echo "rsync      copy the package and install it on $(RNODE)"
	@echo "cran       build the package and run 'R CMD check --as-cran'"
	@echo "winbuild   submit the package to the windows builder service"
	@echo "examples   regenerate the examples used by vignettes"
	@echo "submit     submit the package to CRAN (read DEVEL-README first)"

install:
	$(MAKE) build
	cd $(BINDIR) && R CMD INSTALL $(INSTALL_FLAGS) $(PACKAGE)_$(PACKAGEVERSION).tar.gz

quick-install: version
	cd $(BINDIR) &&	R CMD build $(BUILD_FLAGS) --no-build-vignettes $(PACKAGEDIR) && R CMD INSTALL $(INSTALL_FLAGS) $(PACKAGE)_$(PACKAGEVERSION).tar.gz

build : bumpdate clean
	$(MAKE) releasevignette
	@if grep -q @ $(PACKAGEDIR)/vignettes/$(PACKAGE)-package.bib; then true; \
	else echo "error: vignettes/$(PACKAGE)-package.bib is empty: run 'make vignettes'"; false; fi
	cd $(BINDIR) &&	R CMD build $(BUILD_FLAGS) $(PACKAGEDIR)

closeversion: SVN_REL_URL=$(shell svn info --show-item relative-url)
closeversion: build
	svn ci -m " * NEWS: Close version $(PACKAGEVERSION)"
	svn rm ^/tags/$(PACKAGEVERSION) -m " * Delete previous tag for version $(PACKAGEVERSION)" || echo "OK: tag is new."
	svn cp $(SVN_REL_URL) ^/tags/$(PACKAGEVERSION) -m " * Tag version $(PACKAGEVERSION)"
	svn up
	make releasebuild # again to update version.R and svn_version

releasevignette:
	test -s $(PACKAGEDIR)/vignettes/$(PACKAGE)-package.aux || $(MAKE) nonreleasevignette vignettes
	cd $(PACKAGEDIR)/vignettes \
	&& aux2bib $(PACKAGE)-package.aux | grep -v '@comment' > $(PACKAGE)-package.bib \
	&& sed -i 's/^%\+\\setboolean{Release}{true}/\\setboolean{Release}{true}/' \
	$(PACKAGE)-package.Rnw

nonreleasevignette:
	sed -i 's/^\\setboolean{Release}{true}/%\\setboolean{Release}{true}/' \
	$(PACKAGEDIR)/vignettes/$(PACKAGE)-package.Rnw

releasebuild: BUILD_FLAGS=--compact-vignettes=both
releasebuild: bumpdate releasevignette
	cd $(BINDIR) &&	R CMD build $(BUILD_FLAGS) $(PACKAGEDIR) && tar -atvf $(PACKAGE)_$(PACKAGEVERSION).tar.gz

cran : BUILD_FLAGS=--compact-vignettes=both
cran : build
	cd $(BINDIR) && _R_CHECK_FORCE_SUGGESTS_=false R CMD check --as-cran $(PACKAGE)_$(PACKAGEVERSION).tar.gz

check: build
	cd $(BINDIR) && _R_CHECK_FORCE_SUGGESTS_=false R CMD check $(PACKAGE)_$(PACKAGEVERSION).tar.gz

clean: 
	cd $(PACKAGEDIR) && ($(RM) ./$(PACKAGE)-Ex.R ./src/*.o ./src/*.so; \
		find . -name '*.orig' | xargs $(RM) )

vignettes: version vignettes/$(PACKAGE)-package.Rnw
# FIXME: How to display the output of the latex and bibtex commands with R CMD?
# FIXME: How to halt on warning?
	cd $(PACKAGEDIR)/vignettes \
	&& R CMD Sweave --pdf $(PACKAGE)-package.Rnw \
	&& $(RM) $(PACKAGE)-package.tex
# Rscript -e "library(knitr); knit('$(PACKAGE)-package.Rnw', output='$(PACKAGE)-package.tex', quiet = TRUE)" \
# && $(PDFLATEX) $(PACKAGE)-package.tex && bibtex $(PACKAGE)-package && $(PDFLATEX) $(PACKAGE)-package.tex && $(PDFLATEX) $(PACKAGE)-package.tex && $(RM) $(PACKAGE)-package.tex

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
	@exit 1
endif
ifndef RNODE
	@echo "ERROR: You must specify a remote node (e.g., RNODE=majorana)"
	@exit 1
endif
ifdef RNODE
	rsync -rlp -CIzc -L --delete --copy-unsafe-links --exclude=.svn --exclude=/examples/ --progress --relative \
	.     \
	$(RNODE):$(RDIR)/$(PACKAGE)/
	ssh $(RNODE) "cd $(RDIR)/$(PACKAGE) && make quick-install"
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

examples: install
	@echo "*** Makefile: Regenerating vignette examples. This will take time..."
#	cd examples/vignette-example/ && nice -n 19 $(PACKAGEDIR)/inst/bin/$(PACKAGE) --parallel 2
	cd examples/vignette-example/ && R --vanilla --slave --file=create-example-file.R
	cp examples/vignette-example/irace-output.Rdata examples/vignette-example/examples.Rdata vignettes/
	$(MAKE) vignettes
