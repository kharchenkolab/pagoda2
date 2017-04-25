# Overview of pagoda 2

# Installation Instructions

Installation of pagoda2 requires installation of the Non-metric space library (nmslib) and a custom built largeVis package. Pagoda 2 has been tested with nmslib v1.5.3 although it should work with any subsequent version. To install pagoda2 please follow the instructions below:

```bash
# Install system dependencies for building nmslib
sudo apt-get update
sudo apt-get install build-essential cmake gsl-bin libgsl0-dev libeigen3-dev libboost-all-dev

# Download and build nmslib v1.5.3
# You will need to keep this after the installation
# something like ~/lib/nmslib/ will be suitable
cd /path/of/choice
wget https://github.com/searchivarius/nmslib/archive/v1.5.3.tar.gz
tar xvzf v1.5.3.tar.gz
cd nmslib-1.5.3
cd similarity_search
cmake .
make # Get a coffee

# Install custom largeVis package
# Go to a temporary directory
# e.g. ~/tmp/largeVisCustom/
cd /other/path/of/choice
wget http://pklab.hms.harvard.edu:/pagoda2dependencies/largeVis.tar.gz
sha1sum largeVis.tar.gz # = 0eb51b7322d795f3f6cc35aec03e5bdd3189fa1e
tar xvzf largeVis.tar.gz
R
```

```R
library(devtools)
install('largeVis')
```

```bash
# Get a copy of the package from git
# Suitable directory would be ~/tmp/pagoda2/
cd /another/temp/dir
git clone https://github.com/barkasn/pagoda2.git

# Install the package
# Specify NMSLIB_PATH to the location of nmslib
export NMSLIB_PATH=/path/to/nmslib-1.5.3/
R
```
```R
library(devtools)
install('pagoda2')
```
