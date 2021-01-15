
## drat
if (!requireNamespace("p2data", quietly = TRUE)) {
    install.packages('p2data', repos='https://kharchenkolab.github.io/drat/', type='source')
}

library(pagoda2)

testthat::test_check("pagoda2")