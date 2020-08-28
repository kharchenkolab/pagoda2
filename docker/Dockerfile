FROM rocker/tidyverse:3.5.1

RUN R -e 'chooseCRANmirror(ind=52); install.packages("BiocManager")'
RUN R -e 'BiocManager::install(c("AnnotationDbi", "BiocGenerics", "GO.db", "pcaMethods", "org.Dr.eg.db", "org.Hs.eg.db", "org.Mm.eg.db"))'
RUN R -e 'devtools::install_github("hms-dbmi/pagoda2")'
