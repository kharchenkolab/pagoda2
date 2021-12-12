[![<kharchenkolab>](https://circleci.com/gh/kharchenkolab/pagoda2.svg?style=svg)](https://app.circleci.com/pipelines/github/kharchenkolab/pagoda2)
[![CRAN status](https://www.r-pkg.org/badges/version/pagoda2)](https://cran.r-project.org/package=pagoda2)
[![CRAN downloads](https://cranlogs.r-pkg.org/badges/pagoda2)](https://cran.r-project.org/package=pagoda2)

<img src="https://github.com/kharchenkolab/pagoda2/blob/devel/inst/pagoda_logo.png" align="right" height="140">

# pagoda2

- [Tutorials](#tutorials)
- [Web Demo of Application](#web-demo-of-application)
- [Installation](#installation)
  * [Installing Linux Dependencies](#installing-linux-dependencies)
  * [Installing with Mac OS](#installing-with-mac-os)
  * [pagoda2 via Docker](#pagoda2-via-docker)

## pagoda2: Rapid Processing and Interactive Analysis of Large Datasets

Pagoda2 is an R package for analyzing and interactively exploring large-scale single-cell RNA-seq datasets. The methods were optimized to rapidly process modern scRNAseq datasets, which are both large (approximately 1e6 cells or greater) and sparse. The package provides methods for quality control, filtering, clustering, visualization, differential expression, cross-cutting aspects/states, and geneset/pathway overdispersion analysis. The companion frontend application allows users to figure out which gene expression patterns give rise to different subpopulations within the data. The application allows users to inspect the gene expression patterns of subpopulations through annotated gene sets and pathways, including Gene Ontology (GO) categories. Users may also highlight certain clusters and perform differential expression from their browsers via the frontend application. 

Note that `pagoda2` is an R package developed for analyzing standalone scRNAseq datasets. For joint analysis of multiple datasets, please see the package [conos](https://github.com/kharchenkolab/conos). (The package `pagoda2` is primarily used to preprocess input datasets for conos.)

Several methods within this package were developed based on the originals implemented within [SCDE](http://hms-dbmi.github.io/scde/) and PAGODA1.


## Tutorials

**Basic Walkthrough**
* [HTML version](https://htmlpreview.github.io/?https://raw.githubusercontent.com/kharchenkolab/pagoda2/main/doc/pagoda2.walkthrough.html) 
* [Markdown version](https://github.com/kharchenkolab/pagoda2/blob/main/doc/pagoda2.walkthrough.md)

[PCA-based Basic Walkthrough](http://pklab.med.harvard.edu/peterk/p2/walkthrough.nb.html)


## Web Demo of Application

[10X PBMC Dataset](https://tinyurl.com/demopagoda2)


## Installation 


To install the stable version from [CRAN](https://CRAN.R-project.org/package=pagoda2), use:

```r
install.packages('pagoda2')
```


To install the latest version of `pagoda2`, use:

```r
install.packages('devtools')
devtools::install_github('kharchenkolab/pagoda2')
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



### Installing with Mac OS

We recommend the [Homebrew package manager](https://brew.sh/) to install require dependencies on Mac OS. Please run the following commands in the terminal:

```sh
brew update
brew install curl openssl wget
```

As of version 0.1.3, `pagoda2` should sucessfully install on Mac OS. Furthermore, we encourage Mac OS users to install the package via the binaries on CRAN. However, if there are issues, please refer to the following wiki page for further instructions on installing `pagoda2` with Mac OS: [Installing `pagoda2` for Mac OS](https://github.com/kharchenkolab/pagoda2/wiki/Installing-Pagoda2-for-Mac-OS)



### Pagoda2 via Docker 

If you are having trouble setting up `pagoda2` on your system, an alternative approach to work with `pagoda2` is via a Docker container. To use the Docker container, first [install docker](https://docs.docker.com/get-docker/) on your platform and then run the `pagoda2` image with the following command in the shell:

```
docker run -p 8787:8787 -e PASSWORD=pass pkharchenkolab/pagoda2:latest
```

The first time you run this command, it will pull/download several images---please make sure that you have reliable internet access. You can then point your browser to http://localhost:8787/ to access an Rstudio environment with `pagoda2` installed (please log in using credentials username=`rstudio`, password=`pass`). Explore the Docker [--mount option](https://docs.docker.com/storage/volumes/) to allow the Docker image to access your local files.

### Citation

If you find `pagoda2` useful for your publication, please cite:

```
Nikolas Barkas, Viktor Petukhov, Peter Kharchenko and Evan
Biederstedt (2021). pagoda2: Single Cell Analysis and Differential
Expression. R package version 1.0.8.
```
