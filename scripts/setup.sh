#!/bin/sh
setup_apt ()
{
    apt-get update
    apt-get install -y \
        build-essential \
        r-base \
        git \
        texinfo \
        texlive \
        texlive-fonts-extra \
        texlive-latex-extra \
        texlive-science \
        cm-super \
        r-cran-devtools \
        libcurl4-gnutls-dev \
        libxml2-dev \
        libssl-dev \
        python3-pip
    command -V aux2bib || apt-get install -y bibtex2html
}

command -V apt-get && (setup_apt || echo "Unable to install the apt packages, please check if you are running the script as root.")

R --slave --quiet <<'EOF'
list_of_packages <- c("devtools", "testthat")
new_packages <- list_of_packages[!(list_of_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) {
 cat("Installing R packages: ", paste0(new_packages, collapse=", "), "\n")
 install.packages(new_packages, dependencies=TRUE, repos="https://cloud.r-project.org")
}
EOF
