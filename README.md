[![Build Status](https://travis-ci.com/kharchenkolab/pagoda2.svg?branch=master)](https://travis-ci.com/github/kharchenkolab/pagoda2)

# pagoda2

## Table of Contents

- [Analysis Walkthroughs](#analysis-walkthroughs)
- [Web Demo of Application](#web-demo-of-application)
- [Installation](#installation)
  * [Installing Linux Dependencies](#installing-linux-dependencies)
  * [Installing with Mac OS](#installing-with-mac-os)
  * [Pagoda2 via Docker](#pagoda2-via-docker)


## Analysis Walkthroughs

[Basic Walkthrough, HTML](https://htmlpreview.github.io/?https://raw.githubusercontent.com/kharchenkolab/pagoda2/master/doc/pagoda2.walkthrough.html) 

[Basic Walkthrough, Markdown](https://github.com/kharchenkolab/pagoda2/blob/master/vignettes/pagoda2.walkthrough.md)

[PCA-based Basic Walkthrough](http://pklab.med.harvard.edu/peterk/p2/walkthrough.nb.html)


## Web Demo of Application

[10X PBMC Dataset](https://tinyurl.com/pagoda2demo)


## Installation 

To install the latest version of `pagoda2`, use:

```r
install.packages('devtools')
devtools::install_github('kharchenkolab/pagoda2', build_vignettes = TRUE)
```

If you are using Mac OS, please see instructions below. 



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



### Installing with Mac OS

We recommend the [Homebrew package manager](https://brew.sh/) to install require dependencies on Mac OS. Please run the following commands in the terminal:

```sh
brew update
brew install curl openssl wget
```

As of version 0.1.3, Pagoda2 should sucessfully install on Mac OS. However, if there are issues, please refer to the following wiki page for further instructions on installing Pagoda2 with Mac OS: [Installing Pagoda2 for Mac OS](https://github.com/kharchenkolab/pagoda2/wiki/Installing-Pagoda2-for-Mac-OS)



### Pagoda2 via Docker 

If you are having trouble setting up Pagoda2 on your system, an alternative approach to work with Pagoda2 is via a docker container. To use the docker container, first [install docker](https://docs.docker.com/install/) on your platform and then run the Pagoda2 image with the following command in the shell:

```
docker run -p 8787:8787 pkharchenkolab/pagoda2:latest
```

The first time you run this command, it will pull/download several images---please make sure that you have reliable internet access. You can then point your browser to http://localhost:8787/ to access an Rstudio environment with Pagoda2 installed (log in using credentials rstudio/pass). Explore the docker [--mount option](https://docs.docker.com/storage/volumes/) to allow the docker image to access your local files.

