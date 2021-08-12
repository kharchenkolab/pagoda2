FROM rocker/tidyverse:4.0.1

LABEL authors="Viktor Petukhov <viktor.s.petuhov@ya.ru>, Evan Biederstedt <evan.biederstedt@gmail.com>" \
    version.image="1.0.0" \
    version.pagoda2="1.0.0" \
    version.image="1.0.1" \
    version.pagoda2="1.0.1" \
    description="tidyverse image R 4.0.1 to run pagoda2 with Rstudio"

## Cairo dependencies
RUN apt-get update && apt-get install libxt-dev mesa-common-dev -y

## Cairo needed for scde
## RUN R -e "install.packages('Cairo',dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e 'chooseCRANmirror(ind=52); install.packages("BiocManager")'
RUN R -e 'BiocManager::install(c("AnnotationDbi", "BiocGenerics", "GO.db", "pcaMethods", "org.Dr.eg.db", "org.Hs.eg.db", "org.Mm.eg.db", "scde", "BiocParallel"))'


RUN R -e "install.packages('p2data',dependencies=TRUE, repos='https://kharchenkolab.github.io/drat/', type='source')"

RUN R -e "install.packages('pagoda2',dependencies=TRUE, repos='http://cran.rstudio.com/')"
