
library(pagoda2)
library(dplyr)
library(Matrix)
library(igraph)

test_that("namedNames() functionality", {
    expect_equal(namedNames(c(1, 2, 3)), NULL)
})

