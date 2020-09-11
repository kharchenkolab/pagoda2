## Table of Contents

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
devtools::install_github("kharchenkolab/pagoda2")
library('pagoda2')
```

If you have Mac, or it doesn't work on your Linux, please see instructions below. 
*Currently, there is no way to install Pagoda 2 on Windows.*

### Installing Mac Dependencies

We recommend the [Homebrew package manager](https://brew.sh/) to install require dependencies on Mac OS. Please run the following commands in the terminal:

```sh
brew update
brew install curl openssl wget
```

For more details regarding how to successfully install Pagoda2 on Mac OS, please refer to the following wiki page: [Installing Pagoda2 for Mac OS](https://github.com/kharchenkolab/pagoda2/wiki/Installing-Pagoda2-for-Mac-OS)


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
docker run -p 8787:8787 pkharchenkolab/pagoda2:latest
```

The first time you run the command it will download several images so make sure that you have fast internet access setup. You can then point your browser to http://localhost:8787/ to get an Rstudio environment with pagoda2 installed (log in using credentials rstudio/pass). Explore the docker [--mount option](https://docs.docker.com/storage/volumes/) to allow access of the docker image to your local files.

