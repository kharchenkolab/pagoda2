# Demo Web Application

[10X PBMC Dataset](https://tinyurl.com/pagoda2demo)

# Analysis walkthroughs

[Basic Walkthough -- October 2018](vignettes/pagoda2.walkthrough.oct2018.Rmd)

[PCA-based basic walkthrough](http://pklab.med.harvard.edu/peterk/p2/walkthrough.nb.html)

# Installation Instructions

## Installing Pagoda2 as Docker Container
The fastest and most efficient way to get pagoda on a mac or windows system is through a docker container. The docker distribution is current as of October 2018 and also includes the [Conos package](https://github.com/hms-dbmi/conos). To start a docker container, first (install docker)[https://docs.docker.com/install/] on your platform and then start the pagoda container with the following command in the shell:

```
docker run -p 8787:8787 docker.io/barkasn/pagoda2
```
The first time you run the command it will download several images so make sure that you have fast internet access setup. You can then point your browser to http://localhost:8787/ to get an Rstudio environment with pagoda2 (and conos) installed. Explore the docker [--mount option](https://docs.docker.com/storage/volumes/) to allow access of the docker image to your local files.

## Installing pagoda natively

### Ubuntu Dependencies
Install system dependencies, example here provided for Ubuntu
```sh
sudo apt-get update
sudo apt-get -y install build-essential cmake gsl-bin libgsl0-dev libeigen3-dev libboost-all-dev libssl-dev libcurl4-openssl-dev libssl-dev libcairo2-dev libxt-dev libgtk2.0-dev libcairo2-dev xvfb xauth xfonts-base
```

### Red-Hat-based distributions Dependencies
Was tested on AWS linux
```sh
yum install cairo-devel pango-devel libXt-devel openssl-devel gsl-devel boost-devel
```

### Mac Dependencies
You need R >=3.4.0 to install this package on a mac. 
For installation please refer to [cran](https://cran.r-project.org/)  

You need the [homebrew package manager](https://brew.sh/)  
Run these commands in a terminal:

```sh
brew update
brew install cmake boost eigen gsl curl openssl wget
```
To enable R to use the parallelized functions in the C++ code, you need another version of the clang++-Compiler for mac.   
This is compatible with OS X >= 10.11 

Follow these instructions to install clang4 with openmp support:
#### GUI-Installer:
- Download the clang4-r.pkg (263.6 mb) from https://uofi.box.com/v/r-macos-clang-pkg  
MD5 Hash: `f49df42ccc84ec529c489e8e3f02248`
- Install it!

Here is a more in depth explanation what is going on and a script to do what the GUI-Installer does by yourself. For Pagoda2 you only need the clang part, not the gfortran part. [openmp in r on OS X](http://thecoatlessprofessor.com/programming/openmp-in-r-on-os-x/#after-3-4-0)

#### Install X windows system for mac
Go to https://www.xquartz.org/ and follow the instructions for installing the package.

#### Install gfortran for Mac
```sh
curl -O http://r.research.att.com/libs/gfortran-4.8.2-darwin13.tar.bz2
sudo tar fvxz gfortran-4.8.2-darwin13.tar.bz2 -C /
```
### Install pagoda2 package
Inside R:
```r
# Install Bioconductor dependencies
source("http://bioconductor.org/biocLite.R")
biocLite(c("GO.db", "org.Hs.eg.db","org.Mm.eg.db", "pcaMethods"), suppressUpdates=TRUE)
library(devtools)
install_github("igraph/rigraph") # Don't install with install.packages()
install_github("jkrijthe/Rtsne",ref="openmp")
install.packages(c("Cairo","urltools"))

# Install pagoda
install_github("hms-dbmi/pagoda2")
library('pagoda2')
# Pagoda2 is now ready to use
```
