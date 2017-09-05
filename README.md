# Overview of pagoda 2

# Demo Web Application

10X sorted PBMCs
https://tinyurl.com/pagoda2demo

10X PBMC Dataset
http://pklab.med.harvard.edu/nikolas/pagoda2/frontend/current/pagodaURL/index.html?fileURL=http://pklab.med.harvard.edu/nikolas/pagoda2/staticDemo/10xPBMC.bin

# Installation Instructions

Installation of pagoda2 requires installation of the Non-metric space library (nmslib)
and a custom built largeVis package. Pagoda 2 has been tested with nmslib v1.5.3 although
it should work with any subsequent version. To install pagoda2 please follow the instructions below.


Install system dependencies for building nmslib, example here provided for Ubuntu
```
$ sudo apt-get update
$ sudo apt-get -y install build-essential cmake gsl-bin libgsl0-dev libeigen3-dev libboost-all-dev libcurl4-gnutls-dev libssl-dev libcurl4-openssl-dev libssl-dev libcairo2-dev libxt-dev libgtk2.0-dev libcairo2-dev xvfb xauth xfonts-base

# Download and build nmslib v1.5.3
# You will need to keep this directory after the installation
# so choose a location that will not be altered
# something like ~/lib/nmslib/ will be suitable
$ cd /path/of/choice
$ wget https://github.com/searchivarius/nmslib/archive/v1.5.3.tar.gz
$ tar xvzf v1.5.3.tar.gz
$ cd nmslib-1.5.3
$ cd similarity_search
$ cmake .
$ make
```

Install custom largeVis package.
```
# You just need a temporary directory for this. ( e.g. ~/tmp/largeVisCustom/)
$ cd /other/path/of/choice
$ wget http://pklab.med.harvard.edu/pagoda2dependencies/largeVis.tar.gz
$ sha1sum largeVis.tar.gz # (optional) should be: 0eb51b7322d795f3f6cc35aec03e5bdd3189fa1e
$ tar xvzf largeVis.tar.gz
# Start R and install largeVis
$ R
> install.packages('devtools')
> library(devtools)
> install('largeVis/')
> q()
```

Install pagoda2
```
# Specify the NMSLIB_PATH enviroment to the location that you installed
# and built nmslib above
$ export NMSLIB_PATH=/path/to/nmslib-1.5.3/
# Start R
$ R
# Install Bioconductor dependencies
> source("http://bioconductor.org/biocLite.R")
> biocLite(c("GO.db", "org.Hs.eg.db","pcaMethods"), suppressUpdates=TRUE)
> library(devtools)
> install_github("igraph/rigraph") # Don't install with install.packages()
> install_github("pkharchenko/Rtsne.multicore",ref="precomputed_distance")
> install.packages(‘Cairo’)
> install.packages('urltools')

# Install pagoda
> install_github("hms-dbmi/pagoda2")
> library('pagoda2')
# Pagoda2 is now ready to use
```

# Install padoga2 as a docker container
[Instructions for installing docker container with pagoda2](vignettes/Docker.md)

# Basic analysis walkthrough
[PCA-based basic walkthrough](http://pklab.med.harvard.edu/peterk/p2.walkthrough.html)

# Basic analysis with generation of pagoda2 web application
[Pagoda2 Web Application analysis](vignettes/pagoda2.Rmd)
