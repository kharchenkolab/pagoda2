[![Build Status](https://travis-ci.com/kharchenkolab/pagoda2.svg?branch=master)](https://travis-ci.com/github/kharchenkolab/pagoda2)

<img src="https://github.com/kharchenkolab/pagoda2/blob/devel/inst/pagoda_logo.png" align="right" height="140">

# Pagoda2

- [Tutorials](#tutorials)
- [Web Demo of Application](#web-demo-of-application)
- [Installation](#installation)
  * [Installing Linux Dependencies](#installing-linux-dependencies)
  * [Installing with Mac OS](#installing-with-mac-os)
  * [Pagoda2 via Docker](#pagoda2-via-docker)

## Pagoda2: Rapid Processing and Interactive Analysis of Large Datasets

Pagoda2 is an R package for analyzing and interactively exploring large-scale single-cell RNA-seq datasets. The methods were optimized to rapidly process modern scRNAseq datasets, which are both large (approximately 1e6 cells or greater) and sparse. The package provides methods for quality control, filtering, clustering, visualization, differential expression, cross-cutting aspects/states, and geneset/pathway overdispersion analysis. The companion frontend application allows users to figure out which gene expression patterns give rise to different subpopulations within the data. The application allows users to inspect the gene expression patterns of subpopulations through annotated gene sets and pathways, including Gene Ontology (GO) categories. Users may also highlight certain clusters and perform differential expression from their browsers via the frontend application. 

Note that `pagoda2` is an R package developed for analyzing standalone scRNAseq datasets. For joint analysis of multiple datasets, please see the package [Conos](https://github.com/kharchenkolab/conos). (The package `pagoda2` is primarily used to preprocess input datasets for Conos.)

Several methods within this package were developed based on the originals implemented within [SCDE](http://hms-dbmi.github.io/scde/) and PAGODA1.


## Tutorials

**Basic Walkthrough**
* [HTML version](https://htmlpreview.github.io/?https://raw.githubusercontent.com/kharchenkolab/pagoda2/master/doc/pagoda2.walkthrough.html) 
* [Markdown version](https://github.com/kharchenkolab/pagoda2/blob/master/vignettes/pagoda2.walkthrough.md)

[PCA-based Basic Walkthrough](http://pklab.med.harvard.edu/peterk/p2/walkthrough.nb.html)


## Web Demo of Application

[10X PBMC Dataset](https://tinyurl.com/demopagoda2)


## Installation 

To install the latest version of `pagoda2`, use:

```r
install.packages('devtools')
devtools::install_github('kharchenkolab/pagoda2', build_vignettes = TRUE)
```

The package `pagoda2` depends on data in a data package (`p2data`) that is available through a `drat` repository on GitHub. To use the `pagoda2` package, you will need to install `p2data`. There are two equally valid options to install this package:

A) Users could install `p2data` by adding the `drat` archive to the list of repositories your system will query when adding and updating R packages. Once you do this, you can install `p2data` with `install.packages()`, using the command:

```r
library(drat)
addRepo("kharchenkolab")
install.packages("p2data")
```

The following command is also a valid approach:

```r
install.packages('p2data', repos='https://kharchenkolab.github.io/drat/', type='source')
```

Please see the [drat documentation](https://dirk.eddelbuettel.com/code/drat.html) for more comprehensive explanations and vignettes.


B) Another way to install the package `p2data` is to use `devtools::install_github()`:

```r
library(devtools)
install_github("kharchenkolab/p2data")
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

As of version 0.1.3, `pagoda2` should sucessfully install on Mac OS. However, if there are issues, please refer to the following wiki page for further instructions on installing `pagoda2` with Mac OS: [Installing `pagoda2` for Mac OS](https://github.com/kharchenkolab/pagoda2/wiki/Installing-Pagoda2-for-Mac-OS)



### Pagoda2 via Docker 

If you are having trouble setting up `pagoda2` on your system, an alternative approach to work with `pagoda2` is via a Docker container. To use the Docker container, first [install docker](https://docs.docker.com/install/) on your platform and then run the `pagoda2` image with the following command in the shell:

```
docker run -p 8787:8787 pkharchenkolab/pagoda2:latest
```

The first time you run this command, it will pull/download several images---please make sure that you have reliable internet access. You can then point your browser to http://localhost:8787/ to access an Rstudio environment with `pagoda2` installed (log in using credentials rstudio/pass). Explore the Docker [--mount option](https://docs.docker.com/storage/volumes/) to allow the Docker image to access your local files.

### Citation

If you find `pagoda2` useful for your publication, please cite:

```
Nikolas Barkas, Viktor Petukhov, Peter Kharchenko and Evan
Biederstedt (2020). pagoda2: Single Cell Analysis and Differential
Expression. R package version 1.0.0.
https://github.com/kharchenkolab/pagoda2
```
