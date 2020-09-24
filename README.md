## Table of Contents

- [Demo Web Application](#demo-web-application)
- [Analysis walkthroughs](#analysis-walkthroughs)
- [Installation Instructions](#installation-instructions)
  * [Installing Linux dependencies](#installing-linux-dependencies)
  * [Installing with Mac OS](#installing-with-mac-os)
  * [Pagoda2 via Docker](#pagoda2-via-docker)


## Demo Web Application

[10X PBMC Dataset](https://tinyurl.com/pagoda2demo)

## Analysis Walkthroughs

[Basic Walkthrough](https://htmlpreview.github.io/?https://raw.githubusercontent.com/kharchenkolab/pagoda2/master/doc/pagoda2.walkthrough.html)

[PCA-based Basic Walkthrough](http://pklab.med.harvard.edu/peterk/p2/walkthrough.nb.html)

## Installation Instructions

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

For more details regarding how to successfully install Pagoda2 on Mac OS, please refer to the following wiki page: [Installing Pagoda2 for Mac OS](https://github.com/kharchenkolab/pagoda2/wiki/Installing-Pagoda2-for-Mac-OS)



### Pagoda2 via Docker 

If you are having trouble setting up pagoda2 on your system, an alternative approach to get pagoda2 via a docker container, which also include the [Conos package](https://github.com/hms-dbmi/conos). To create a docker container, first [install docker](https://docs.docker.com/install/) on your platform and then pull the pagoda2 image with the following command in the shell:

```
docker run -p 8787:8787 pkharchenkolab/pagoda2:latest
```

The first time you run the command it will download several images so make sure that you have fast internet access setup. You can then point your browser to http://localhost:8787/ to get an Rstudio environment with pagoda2 installed (log in using credentials rstudio/pass). Explore the docker [--mount option](https://docs.docker.com/storage/volumes/) to allow access of the docker image to your local files.

