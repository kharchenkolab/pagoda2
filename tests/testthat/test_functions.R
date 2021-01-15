
library(pagoda2)
library(dplyr)

test_that("namedNames() functionality", {
    expect_equal(namedNames(c(1, 2, 3)), NULL)
})


test_that("check cm dims", {
	## We have pre-generated a dataset of 50 bone marrow cells that you can load as a matrix directly
    cm <- readRDS(system.file("extdata", "sample_BM1_50.rds", package="pagoda2"))
    expect_equal(dim(cm)[1], 33694)
    expect_equal(dim(cm)[2], 50)
})


test_that("check basic qc of counts", {
	## remove two cells
    cm <- readRDS(system.file("extdata", "sample_BM1_50.rds", package="pagoda2"))
    counts <- gene.vs.molecule.cell.filter(cm, min.cell.size=500)
    expect_equal(dim(counts)[1], 33694)
    expect_equal(dim(counts)[2], 50)
    ## Filter and check the size of the resulting matrix:
    counts <- counts[rowSums(counts)>=10,]
    expect_equal(dim(counts)[1], 1432)
    expect_equal(dim(counts)[2], 50)
})


test_that("basic check of Pagoda2 class", {
    cm <- readRDS(system.file("extdata", "sample_BM1_50.rds", package="pagoda2"))
    counts <- gene.vs.molecule.cell.filter(cm, min.cell.size=500)
    counts <- counts[rowSums(counts)>=10,]
    rownames(counts) <- make.unique(rownames(counts))
    r <- Pagoda2$new(counts,log.scale=TRUE, n.cores=2)
    expect_equal(dim(counts)[1], 1432)
    expect_equal(dim(counts)[2], 50)
    r$adjustVariance(plot=TRUE, gam.k=10)
    r$calculatePcaReduction(nPcs=50, n.odgenes=3e3)
    r$makeKnnGraph(k=40, type='PCA', center=TRUE, distance='cosine')
    ## number of OD genes is now 0
    expect_equal(length(r$misc$odgenes), 0)
    expect_true(r$misc$log.scale)
    expect_equal(r$misc$model.type, "plain")
    expect_equal(r$misc$depthScale, 1000)
})
