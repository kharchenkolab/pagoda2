
library(pagoda2)
library(dplyr)

test_that("namedNames() functionality", {
    expect_equal(namedNames(c(1, 2, 3)), NULL)
})


test_that("check cm dims", {
	## We have pre-generated a dataset of 3000 bone marrow cells that you can load as a matrix directly
    cm <- p2data::sample_BM1
    expect_equal(dim(cm)[1], 33694)
    expect_equal(dim(cm)[2], 3000)
})


test_that("check basic qc of counts", {
	## remove two cells
    cm <- p2data::sample_BM1
    counts <- gene.vs.molecule.cell.filter(cm, min.cell.size=500)
    expect_equal(dim(counts)[1], 33694)
    expect_equal(dim(counts)[2], 2998)
    ## Filter and check the size of the resulting matrix:
    counts <- counts[rowSums(counts)>=10,]
    expect_equal(dim(counts)[1], 12693)
    expect_equal(dim(counts)[2], 2998)
})


test_that("basic check of Pagoda2 class", {
    cm <- p2data::sample_BM1
    counts <- gene.vs.molecule.cell.filter(cm, min.cell.size=500)
    counts <- counts[rowSums(counts)>=10,]
    rownames(counts) <- make.unique(rownames(counts))
    r <- Pagoda2$new(counts,log.scale=TRUE, n.cores=2)
    expect_equal(dim(counts)[1], 12693)
    expect_equal(dim(counts)[2], 2998)
    r$adjustVariance(plot=TRUE, gam.k=10)
    r$calculatePcaReduction(nPcs=50, n.odgenes=3e3)
    r$makeKnnGraph(k=40, type='PCA', center=TRUE, distance='cosine')
    ## number of od genes, 187
    expect_equal(length(r$misc$odgenes), 187)
    expect_true(r$misc$log.scale)
    expect_equal(r$misc$model.type, "plain")
    expect_equal(r$misc$depthScale, 1000)
})
