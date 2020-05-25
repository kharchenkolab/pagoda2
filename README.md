## Table of Content

- [Demo Web Application](#demo-web-application)
- [Analysis walkthroughs](#analysis-walkthroughs)
- [Installation Instructions](#installation-instructions)
  * [Installing Mac Dependencies](#installing-mac-dependencies)
  * [Installing Linux dependencies](#installing-linux-dependencies)
  * [Installing Pagoda2 as Docker Container](#installing-pagoda2-as-docker-container)


## Demo Web Application

[10X PBMC Dataset](https://tinyurl.com/pagoda2demo)

## Analysis walkthroughs

[Basic Walkthough -- October 2018](vignettes/pagoda2.walkthrough.oct2018.md)

[PCA-based basic walkthrough](http://pklab.med.harvard.edu/peterk/p2/walkthrough.nb.html)

## Installation Instructions

On most linux-based installations, Pagoda should be available simply by running the 
following code in R console:

```r
install.packages(c("devtools", "BiocManager"))
BiocManager::install(c("AnnotationDbi", "BiocGenerics", "GO.db", "pcaMethods"))
devtools::install_github("hms-dbmi/pagoda2")
library('pagoda2')
```

If you have Mac, or it doesn't work on your Linux, please see instructions below. 
*Currently, there is no way to install Pagoda 2 on Windows.*

### Installing Mac Dependencies

You need R >=3.4.0 to install this package on a mac. 
For installation please refer to [cran](https://cran.r-project.org/)  

You need the [homebrew package manager](https://brew.sh/)  
Run these commands in a terminal:

```sh
brew update
brew install curl openssl wget
```
To enable R to use the parallelized functions in the C++ code, you need another version of the clang++-Compiler for mac.   
This is compatible with OS X >= 10.11 

Follow these instructions to install clang4 with openmp support:

#### GUI-Installer:

- Download the clang4-r.pkg (263.6 mb) from https://uofi.box.com/v/r-macos-clang-pkg  
MD5 Hash: `f49df42ccc84ec529c489e8e3f02248`
- Install it!

Here is a more in depth explanation what is going on and a script to do what the GUI-Installer does by yourself. For Pagoda2 you only need the clang part, not the gfortran part. [openmp in r on OS X](http://thecoatlessprofessor.com/programming/openmp-in-r-on-os-x/#after-3-4-0)

#### Install gfortran for Mac

```sh
curl -O https://mac.r-project.org/libs/gfortran-4.8.2-darwin13.tar.bz2
sudo tar fvxz gfortran-4.8.2-darwin13.tar.bz2 -C /
```

### Installing Linux dependencies

Installation for Debian-based distributions (e.g. Ubuntu):

```sh
sudo apt-get update
sudo apt-get -y install libcurl4-openssl-dev libssl-dev
```

Installation for Red-Hat-based distributions (e.g. CentOS or Fedora)

```sh
yum install openssl-devel libcurl-devel
```

### Installing Pagoda2 as Docker Container

If you are having trouble setting up pagoda2 on your system, an alternative approach to get pagoda on a mac or windows system is through a docker container. The docker distribution is current as of October 2018 and also includes the [Conos package](https://github.com/hms-dbmi/conos). To start a docker container, first [install docker](https://docs.docker.com/install/) on your platform and then start the pagoda container with the following command in the shell:

```
docker run -p 8787:8787 vpetukhov/pagoda2:latest
```

The first time you run the command it will download several images so make sure that you have fast internet access setup. You can then point your browser to http://localhost:8787/ to get an Rstudio environment with pagoda2 installed (log in using credentials rstudio/pass). Explore the docker [--mount option](https://docs.docker.com/storage/volumes/) to allow access of the docker image to your local files.

