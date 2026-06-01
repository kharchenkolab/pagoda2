
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
    expect_warning(
        counts <- gene.vs.molecule.cell.filter(cm, min.cell.size=500),
        "p2\\$run"
    )
    expect_equal(dim(counts)[1], 33694)
    expect_equal(dim(counts)[2], 50)
    ## Filter and check the size of the resulting matrix:
    counts <- counts[rowSums(counts)>=10,]
    expect_equal(dim(counts)[1], 1432)
    expect_equal(dim(counts)[2], 50)
})

write_10x_fixture <- function(path, matrix, version = "V3") {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
    Matrix::writeMM(matrix, file.path(path, "matrix.mtx"))
    if (version == "V3") {
        write.table(
            data.frame(
                id = paste0("ens", seq_len(nrow(matrix))),
                symbol = rownames(matrix),
                type = "Gene Expression"
            ),
            file.path(path, "features.tsv"),
            quote = FALSE,
            sep = "\t",
            row.names = FALSE,
            col.names = FALSE
        )
    } else {
        write.table(
            data.frame(id = paste0("ens", seq_len(nrow(matrix))), symbol = rownames(matrix)),
            file.path(path, "genes.tsv"),
            quote = FALSE,
            sep = "\t",
            row.names = FALSE,
            col.names = FALSE
        )
    }
    write.table(
        colnames(matrix),
        file.path(path, "barcodes.tsv"),
        quote = FALSE,
        sep = "\t",
        row.names = FALSE,
        col.names = FALSE
    )
}

test_that("10x readers return dgCMatrix without deprecated Matrix coercion warnings", {
    cm <- Matrix::Matrix(
        c(
            1, 0, 2,
            0, 3, 0
        ),
        nrow = 2,
        ncol = 3,
        sparse = TRUE,
        dimnames = list(c("geneA", "geneB"), c("cell1", "cell2", "cell3"))
    )

    td <- tempfile("p2_10x")
    dir.create(td)
    oldwd <- setwd(td)
    on.exit(setwd(oldwd), add = TRUE)
    on.exit(unlink(td, recursive = TRUE), add = TRUE)
    write_10x_fixture(".", cm, version = "V3")

    expect_silent(read <- read10xMatrix(".", version = "V3", transcript.id = "SYMBOL", verbose = FALSE))
    expect_true(inherits(read, "dgCMatrix"))
    expect_identical(rownames(read), rownames(cm))
    expect_identical(colnames(read), colnames(cm))
    expect_equal(as.matrix(read), as.matrix(cm))

    expect_silent(read.named <- read.10x.matrices(".", version = "V3", verbose = FALSE))
    expect_true(inherits(read.named, "dgCMatrix"))
    expect_identical(rownames(read.named), rownames(cm))
    expect_identical(colnames(read.named), paste0("one_", colnames(cm)))
    expect_equal(unname(as.matrix(read.named)), unname(as.matrix(cm)))
})

test_that("Pagoda2 constructor preserves dense input values through sparse coercion", {
    cm <- matrix(
        c(
            1, 0, 2,
            0, 3, 0
        ),
        nrow = 2,
        ncol = 3,
        dimnames = list(c("geneA", "geneB"), c("cell1", "cell2", "cell3"))
    )

    expect_silent(p2 <- Pagoda2$new(
        cm,
        verbose = FALSE,
        n.cores = 1,
        min.cells.per.gene = 0,
        min.transcripts.per.cell = 0,
        log.scale = FALSE,
        trim = 0
    ))
    expect_true(inherits(p2$misc$rawCounts, "dgCMatrix"))
    expect_equal(as.matrix(t(p2$misc$rawCounts)), cm)
})
