library(pagoda2)

make_api_version_matrix <- function() {
  cm <- Matrix::Matrix(
    c(
      5, 0, 2,
      0, 4, 1,
      3, 0, 6
    ),
    nrow = 3,
    ncol = 3,
    sparse = TRUE,
    dimnames = list(paste0("g", 1:3), paste0("c", 1:3))
  )
  as(cm, "dgCMatrix")
}

test_that("Pagoda2 objects expose the pagoda2.1 API version", {
  p2 <- Pagoda2$new(
    make_api_version_matrix(),
    verbose = FALSE,
    n.cores = 1,
    min.cells.per.gene = 0,
    min.transcripts.per.cell = 0,
    trim = 0
  )

  expect_identical(p2$apiVersion, "2.1")
  expect_identical(Pagoda2$new(p2)$apiVersion, "2.1")
  expect_identical(unserialize(serialize(p2, NULL))$apiVersion, "2.1")
})
