
library(pagoda2)


test_that("setMinMax() functionality", {
	example_matrix =  matrix(rep(c(1:5), 3), 5)
	expect_equal(unique(setMinMax(example_matrix, 2, 4)[1,]), 2)
})


