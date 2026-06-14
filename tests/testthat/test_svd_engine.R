library(pagoda2)

## The reduction SVD engine: .pagoda2_truncated_svd prefers RSpectra (faster + more accurate than irlba),
## centering implicitly via a matrix operator so the sparse block is never densified. Guard that the
## centered operator reproduces irlba's centered SVD (this is the subtle path PCA/joint reductions use).
test_that(".pagoda2_truncated_svd centered operator matches irlba centered SVD", {
  skip_if_not_installed("RSpectra")
  set.seed(2)
  X <- as(Matrix::rsparsematrix(800, 120, 0.1, rand.x = function(n) rpois(n, 3) + 1), "CsparseMatrix")
  cm <- Matrix::colMeans(X)
  k <- 15L
  rs <- pagoda2:::.pagoda2_truncated_svd(X, nv = k, center = cm)
  ir <- irlba::irlba(X, nv = k, nu = 0, center = cm, maxit = 300)
  expect_identical(dim(rs$v), c(120L, k))
  expect_equal(rs$d, ir$d, tolerance = 1e-4)                       # singular values match
  for (j in 1:10) expect_gt(abs(stats::cor(rs$v[, j], ir$v[, j])), 0.999) # loadings match up to sign
})

test_that(".pagoda2_truncated_svd uncentered matches irlba uncentered (LSI path)", {
  skip_if_not_installed("RSpectra")
  set.seed(3)
  X <- as(Matrix::rsparsematrix(600, 100, 0.12, rand.x = function(n) rpois(n, 2) + 1), "CsparseMatrix")
  k <- 12L
  rs <- pagoda2:::.pagoda2_truncated_svd(X, nv = k, center = NULL)
  ir <- irlba::irlba(X, nv = k, nu = 0, maxit = 300)
  expect_equal(rs$d, ir$d, tolerance = 1e-4)
  for (j in 1:8) expect_gt(abs(stats::cor(rs$v[, j], ir$v[, j])), 0.999)
})
