#!/bin/sh
R --slave --quiet <<'EOF'
list_of_packages <- c("devtools", "testthat")
new_packages <- list_of_packages[!(list_of_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) {
 cat("Installing R packakages: ", paste0(new_packages, collapse=", "), "\n")
 install_packages(new_packages)
}
EOF
# How to make this more general for any Linux/MacOS ?
# Maybe use bibtool -x instead?
command -V aux2bib || (echo "aux2bib not found, installing..."; sudo apt install bibtex2html)

test -e ./vignettes/optbib || (echo "vignettes/optbib not found, cloning from github..."; git clone https://github.com/iridia-ulb/references.git vignettes/optbib)

