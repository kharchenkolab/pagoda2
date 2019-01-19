FROM rocker/tidyverse:3.5.1

RUN apt-get update --yes && apt-get install --no-install-recommends --yes \
  build-essential \
  cmake \
  git \
  less \
  libcurl4-openssl-dev \
  libssl-dev \
  gsl-bin \
  libgsl0-dev \
  libeigen3-dev \
  libboost-all-dev \ 
  libssl-dev \
  libcurl4-openssl-dev \
  libssl-dev \
  libcairo2-dev \
  libxt-dev \
  libgtk2.0-dev \
  libcairo2-dev \
  xvfb  \
  xauth \ 
  xfonts-base \
  libz-dev 

RUN \
  R -e 'chooseCRANmirror(ind=52); install.packages(c("RcppEigen", "urltools"))' && \
  R -e 'chooseCRANmirror(ind=52); source("http://bioconductor.org/biocLite.R"); biocLite(c("GO.db", "org.Hs.eg.db","org.Mm.eg.db", "pcaMethods","DESeq2"), suppressUpdates=TRUE);' && \ 
  R -e 'chooseCRANmirror(ind=52); library(devtools); install_github("jkrijthe/Rtsne",ref="openmp")' && \
  R -e 'chooseCRANmirror(ind=52); library(devtools); install_github("hms-dbmi/pagoda2")' && \
