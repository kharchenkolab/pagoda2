
if (!requireNamespace("p2data", quietly = TRUE)) {
    library(drat)
    addRepo("kharchenkolab")
    install.packages("p2data")
}

library(pagoda2)

testthat::test_check("pagoda2")