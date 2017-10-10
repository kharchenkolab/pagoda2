# Demo Web Application

10X sorted PBMCs
https://tinyurl.com/pagoda2demo

10X PBMC Dataset
http://pklab.med.harvard.edu/nikolas/pagoda2/frontend/current/pagodaURL/index.html?fileURL=http://pklab.med.harvard.edu/nikolas/pagoda2/staticDemo/10xPBMC.bin

# Basic analysis walkthrough
[PCA-based basic walkthrough](http://pklab.med.harvard.edu/peterk/p2.walkthrough.html)

# Basic analysis with generation of pagoda2 web application
[Pagoda2 Web Application analysis](vignettes/pagoda2.Rmd)

# Installation Instructions

Installation of pagoda2 requires installation of the Non-metric space library (nmslib)
and a custom built largeVis package. Pagoda 2 has been tested with nmslib v1.5.3 and nmslib v1.6 and it should work with any subsequent version. To install pagoda2 please follow the instructions below.

## System dependencies

### Ubuntu
Install system dependencies for building nmslib, example here provided for Ubuntu
```sh
sudo apt-get update
sudo apt-get -y install build-essential cmake gsl-bin libgsl0-dev libeigen3-dev libboost-all-dev libcurl4-gnutls-dev libssl-dev libcurl4-openssl-dev libssl-dev libcairo2-dev libxt-dev libgtk2.0-dev libcairo2-dev xvfb xauth xfonts-base
```

### Mac 
You need R >=3.4.0 to install this package on a mac. 
For installation please refer to [cran](https://cran.r-project.org/)  

You need the [homebrew package manager](https://brew.sh/)  
Run these commands in a terminal:

```sh
brew update
brew install cmake boost eigen gsl curl openssl
```
To enable R to use the parallelized functions in the C++ code, you need another version of the clang++-Compiler for mac.   
This is compatible with Mac OS >= 10.11 

Follow these instructions to install clang4 with openmp support:
#### GUI-Installer:
- Download the clang4-r.pkg (263.6 mb) from https://uofi.box.com/v/r-macos-clang-pkg  
MD5 Hash: `f49df42ccc84ec529c489e8e3f02248`
- Install it!

Here is a more in depth explanation what is going on and a script to do what the GUI-Installer does by yourself. For Pagoda2 you only need the clang part, not the gfortran part. [openmp in r on Mac OS](http://thecoatlessprofessor.com/programming/openmp-in-r-on-os-x/#after-3-4-0)

## Installing NMSLIB (Non-Metric Space Library)

```sh
# Download and build nmslib v1.6
# You will need to keep this directory after the installation
# so choose a location that will not be altered
# something like ~/lib/nmslib/ will be suitable
cd /path/of/choice
wget https://github.com/searchivarius/nmslib/archive/v1.6.tar.gz
tar xvzf v1.6.tar.gz
cd nmslib-1.6
cd similarity_search
cmake .
make
```

Install custom largeVis package.
```sh
# You just need a temporary directory for this. ( e.g. ~/tmp/largeVisCustom/)
cd /other/path/of/choice
wget http://pklab.med.harvard.edu/pagoda2dependencies/largeVis.tar.gz
sha1sum largeVis.tar.gz # (optional) should be: 0eb51b7322d795f3f6cc35aec03e5bdd3189fa1e
tar xvzf largeVis.tar.gz
```
Start R and install largeVis
```r
install.packages('devtools')
library(devtools)
install('largeVis/')
q()
```

Install pagoda2 inside of R:
```r
# Install Bioconductor dependencies
source("http://bioconductor.org/biocLite.R")
biocLite(c("GO.db", "org.Hs.eg.db","pcaMethods"), suppressUpdates=TRUE)
library(devtools)
install_github("igraph/rigraph") # Don't install with install.packages()
install_github("jkrijthe/Rtsne",ref="openmp")
install.packages(c("Cairo","urltools"))

# Specify the location where nms lib was installed in a NMSLIB_PATH environment variable
Sys.setenv("NMSLIB_PATH"="/path/to/nmslib-1.6/")
# Install pagoda
install_github("hms-dbmi/pagoda2")
library('pagoda2')
# Pagoda2 is now ready to use
```

## Alternative Install padoga2 as a docker container
[Instructions for installing docker container with pagoda2](vignettes/Docker.md)
