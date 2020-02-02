PACKAGEVERSION=3.4
PACKAGE=$(shell sh -c 'grep -F "Package: " DESCRIPTION | cut -f2 -d" "')
# FIXME: This Makefile only works with this BINDIR!
BINDIR=$(CURDIR)/..
RNODE=iridiacluster
RDIR=~/
INSTALL_FLAGS=
BUILD_FLAGS=
REALVERSION=$(PACKAGEVERSION).$(SVN_REV)
PACKAGEDIR=$(CURDIR)
FTP_COMMANDS="user anonymous anonymous\nbinary\ncd incoming\nput $(PACKAGE)_$(PACKAGEVERSION).tar.gz\nquit\n"
WINBUILD_DEVEL_FTP_COMMANDS="user anonymous anonymous\nbinary\ncd R-devel\nput $(PACKAGE)_$(PACKAGEVERSION).tar.gz\nquit\n"
WINBUILD_REL_FTP_COMMANDS="user anonymous anonymous\nbinary\ncd R-release\nput $(PACKAGE)_$(PACKAGEVERSION).tar.gz\nquit\n"
PDFLATEX=pdflatex -shell-escape -file-line-error -halt-on-error -interaction=nonstopmode "\input"
SED=sed -i.bak

Rversion=$(R --version | head -n -1 | grep -m 1 -o -e [0-9] | head -n 1)
ifeq ($(Rversion),2)
 NO_BUILD_VIGNETTES='--no-vignettes'
else
 NO_BUILD_VIGNETTES='--no-build-vignettes'
endif

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

.PHONY : help build check clean install pdf rsync version submit cran winbuild vignettes examples genoptions pkgdown

help:
	@echo "install    install the package"
	@echo "quick-install  install the package without rebuilding the vignettes"
	@echo "build      build the package as a tar.gz file"
	@echo "check      build the package and run 'R CMD check'"
	@echo "check TEST=x  run test called test-x.R"
	@echo "rsync      copy the package and install it on $(RNODE)"
	@echo "cran       build the package and run 'R CMD check --as-cran'"
	@echo "winbuild   submit the package to the windows builder service"
	@echo "examples   regenerate the examples used by vignettes"
	@echo "vignettes  generate PDF of the vignettes"
	@echo "submit     submit the package to CRAN (read DEVEL-README first)"

install:
	$(MAKE) build
	cd $(BINDIR) && R CMD INSTALL $(INSTALL_FLAGS) $(PACKAGE)_$(PACKAGEVERSION).tar.gz

quick-install: version
	cd $(BINDIR) &&	R CMD build $(BUILD_FLAGS) $(NO_BUILD_VIGNETTES)  $(PACKAGEDIR) && R CMD INSTALL $(INSTALL_FLAGS) $(PACKAGE)_$(PACKAGEVERSION).tar.gz

genoptions: R/irace-options.R vignettes/section/irace-options.tex scripts/irace_options_comment.R

R/irace-options.R vignettes/section/irace-options.tex scripts/irace_options_comment.R: scripts/irace_options.json scripts/generate-options.R
	cd scripts && R --slave -f generate-options.R && cd ..

gendoc: 
	R --slave -e 'devtools::document()'

pkgdown: gendoc
	R --slave -e 'pkgdown::build_site(run_dont_run = TRUE)'

build : version genoptions gendoc clean
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

vignettes/$(PACKAGE)-package.bib: vignettes/$(PACKAGE)-package.aux
	cd $(PACKAGEDIR)/vignettes \
	&& aux2bib $(PACKAGE)-package.aux | grep -v '@comment' > tmp-$(PACKAGE)-package.bib \
	&& mv tmp-$(PACKAGE)-package.bib $(PACKAGE)-package.bib

releasevignette:
	test -s $(PACKAGEDIR)/vignettes/$(PACKAGE)-package.aux || $(MAKE) nonreleasevignette
	$(MAKE) vignettes/$(PACKAGE)-package.bib
	$(SED) 's/^%\+\\setboolean{Release}{true}/\\setboolean{Release}{true}/' \
	$(PACKAGEDIR)/vignettes/$(PACKAGE)-package.Rnw

nonreleasevignette:
	$(SED) 's/^\\setboolean{Release}{true}/%\\setboolean{Release}{true}/' \
	$(PACKAGEDIR)/vignettes/$(PACKAGE)-package.Rnw
	$(MAKE) vignettes
	$(MAKE) vignettes/$(PACKAGE)-package.bib

releasebuild: BUILD_FLAGS=--compact-vignettes=qpdf
releasebuild: releasevignette
	cd $(BINDIR) &&	R CMD build $(BUILD_FLAGS) $(PACKAGEDIR) && tar -atvf $(PACKAGE)_$(PACKAGEVERSION).tar.gz
# Let's try to compact vignettes further (not needed for R > 3.6.1)
	./scripts/recompact.sh $(BINDIR)/$(PACKAGE)_$(PACKAGEVERSION).tar.gz \
		$(PACKAGE)/inst/doc/$(PACKAGE)-package.pdf
cran : releasebuild
	cd $(BINDIR) && _R_CHECK_FORCE_SUGGESTS_=false _R_CHECK_CRAN_INCOMING_=0 R CMD check --as-cran $(PACKAGE)_$(PACKAGEVERSION).tar.gz

check: build
ifdef TEST
	_R_CHECK_FORCE_SUGGESTS_=false NOT_CRAN=true R --slave -e 'devtools::test(filter="$(TEST)")'
else
	test -d ./GenericWrapper4AC/build || (cd GenericWrapper4AC && python3 setup.py install --user)
	cd $(BINDIR) && (_R_CHECK_FORCE_SUGGESTS_=false NOT_CRAN=true R CMD check --run-donttest --timings $(PACKAGE)_$(PACKAGEVERSION).tar.gz; cat $(PACKAGE).Rcheck/$(PACKAGE)-Ex.timings)
endif

clean: 
	cd $(PACKAGEDIR) && ($(RM) ./$(PACKAGE)-Ex.R ./src/*.o ./src/*.so \
		tests/testthat/*.log tests/testthat/*.Rout tests/testthat/irace.Rdata; \
		find . -name '*.orig' -o -name '.Rhistory' | xargs $(RM) )

## FIXME: building the vignettes is a bit complicated and sometimes fails.
# If \setboolean{Release}{false}, entries are taken from optbib and everything
# should work. However, we cannot build the package like this because we cannot
# distribute optbib. So when \setboolean{Release}{true}, we take entries from
# irace-package.bib, to obtain irace-package.bib, we need to build with
# \setboolean{Release}{false}, use aux2bib on the .aux file, then set
# \setboolean{Release}{true}, then rebuild again. Ideally:
#
# make vignettes' or 'make pdf' should build in whatever Release value we have
# and do whatever is necessary to get it working.
#
# make build/check/cran/install/releasebuild should build with Release as true
# and do whatever is necessary to get it working, but avoid doing extra
# work. For example, if irace-package.bib is non-empty, then do not build two
# times with Release false and then with Release true.
#
# make nonreleasevignette should build with Release as false (which is faster and should always work).
#
# It is ok to fail if something is missing or needs to be done and give a nice error. For example, if optbib is missing.
vignettes: version vignettes/$(PACKAGE)-package.Rnw vignettes/section/irace-options.tex
# FIXME: How to display the output of the latex and bibtex commands with R CMD?
# FIXME: How to halt on warning?
	@test -d $(PACKAGEDIR)/vignettes/optbib || (echo "ERROR: vignettes/optbib not found. You need to symlink or checkout https://github.com/iridia-ulb/references." && exit 1)
	cd $(PACKAGEDIR)/vignettes \
	&& R CMD Sweave --pdf $(PACKAGE)-package.Rnw \
	&& $(RM) $(PACKAGE)-package.tex
# Rscript -e "library(knitr); knit('$(PACKAGE)-package.Rnw', output='$(PACKAGE)-package.tex', quiet = TRUE)" \
# && $(PDFLATEX) $(PACKAGE)-package.tex && bibtex $(PACKAGE)-package && $(PDFLATEX) $(PACKAGE)-package.tex && $(PDFLATEX) $(PACKAGE)-package.tex && $(RM) $(PACKAGE)-package.tex

pdf: build
	R CMD Rd2pdf --force --no-preview --batch --output=$(BINDIR)/$(PACKAGE)_$(PACKAGEVERSION).pdf $(PACKAGEDIR)/

version :
	@printf "#' irace.version\n#'\n#' A character string containing the version of \\pkg{irace}.\n#'\n#' @export\nirace.version <- '$(REALVERSION)'\n" > $(PACKAGEDIR)/R/version.R
	@$(SED) 's/Version:.*$$/Version: $(PACKAGEVERSION)/' $(PACKAGEDIR)/DESCRIPTION
	@$(SED) 's/\\iraceversion}{.*}$$/\\iraceversion}{$(PACKAGEVERSION)}/' vignettes/$(PACKAGE)-package.Rnw

rsync : version
ifndef RDIR
	@echo "ERROR: You must specify a remote dir (e.g., RDIR=~/)"
	@exit 1
endif
ifndef RNODE
	@echo "ERROR: You must specify a remote node (e.g., RNODE=majorana)"
	@exit 1
else
	rsync -rlp -CIzc -L --delete --copy-unsafe-links --exclude=.svn --exclude=/examples/ --progress --relative \
	.     \
	$(RNODE):$(RDIR)/$(PACKAGE)/
	ssh $(RNODE) "cd $(RDIR)/$(PACKAGE) && make quick-install"
endif

remotecran: releasebuild
	R --slave -e "rhub::check_for_cran(path=\"$(BINDIR)/$(PACKAGE)_$(PACKAGEVERSION).tar.gz\", show_status = TRUE,  env_vars = c('_R_CHECK_FORCE_SUGGESTS_'='true', R_DEFAULT_SAVE_VERSION='2', R_DEFAULT_SERIALIZE_VERSION='2'))"

submit: 
	@echo "Read http://cran.r-project.org/web/packages/policies.html"
	@echo "*** You need to use http://cran.r-project.org/submit.html"
	@exit 1
	cd $(BINDIR) && echo $(FTP_COMMANDS) | ftp -v -e -g -i -n cran.r-project.org
	@echo "Don't forget to send email to cran@r-project.org !"

macbuild: releasebuild
	R --slave -e "rhub::check(platform='macos-elcapitan-release', path='$(BINDIR)/$(PACKAGE)_$(PACKAGEVERSION).tar.gz', env_vars = c('_R_CHECK_FORCE_SUGGESTS_'='false', R_DEFAULT_SAVE_VERSION='2', R_DEFAULT_SERIALIZE_VERSION='2'))"

winbuild: releasebuild
	@echo "Winbuild: http://win-builder.r-project.org/"
	cd $(BINDIR) && echo $(WINBUILD_DEVEL_FTP_COMMANDS) | ftp -v -p -e -g -i -n win-builder.r-project.org
	cd $(BINDIR) && echo $(WINBUILD_REL_FTP_COMMANDS) | ftp -v -p -e -g -i -n win-builder.r-project.org
	R --slave -e "rhub::check_on_windows(path='$(BINDIR)/$(PACKAGE)_$(PACKAGEVERSION).tar.gz', env_vars = c('_R_CHECK_FORCE_SUGGESTS_'='false', R_DEFAULT_SAVE_VERSION='2', R_DEFAULT_SERIALIZE_VERSION='2'))"


examples: install
	@echo "*** Makefile: Regenerating data for vignettes and examples. This will take time..."
	cd examples/vignette-example/ && nice -n 19 $(PACKAGEDIR)/src/iracebin/$(PACKAGE) --parallel 2 | tee irace-acotsp-stdout.txt \
		&& R --vanilla --slave --file=create-example-file.R
	cp examples/vignette-example/*.Rdata examples/vignette-example/irace-acotsp-stdout.txt vignettes/
	$(MAKE) vignettes
	$(MAKE) check
