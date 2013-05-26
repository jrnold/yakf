## context("dlm_arima")

## ## Koopman SsfPack 3.0 p. 16
## test_that("ARMA(2, 1) example works", {
##   result <- dlm_arima(ar = c(0.6, 0.2), ma = -0.2, sigma = sqrt(0.9))
##   expect_true(all.equal(result@T, Matrix(c(0.6, 0.2, 1, 0), 2, 2)))
##   expect_true(all.equal(result@Z, Matrix(c(1, 0), 1, 2)))
##   expect_true(all.equal(as(result@HH, "dsyMatrix"),
##                         Matrix(c(0.90, -0.180, -0.180, 0.036), 2, 2)))
##   expect_true(all(result@GG == 0))
##   expect_true(all(result@a1 == 0))
##   expect_true(all.equal(result@P1,
##                         Matrix(c(1.5857, 0.012857, 0.012857, 0.099429), 2, 2))[1] == "TRUE")
## })

## ## Koopman SsfPack 3.0 p. 18
## test_that("ARIMA(2, 1) example works", {
##   result <- dlm_arima(ar = c(0.6, 0.2), ma = -0.2, d = 1, sigma = sqrt(0.9))
##   expect_true(all.equal(result@T, Matrix(c(1, 0, 0, 1, 0.6, 0.2, 0, 1, 0), 3, 3)))
##   expect_true(all.equal(result@Z, Matrix(c(1, 1, 0), 1, 3)))
##   expect_true(all.equal(result@HH, 
##                         Matrix(c(0, 0, 0, 0, 0.90, -0.180, 0, -0.180, 0.036), 3, 3)))
##   expect_true(all(result@GG == 0))
##   expect_true(all(result@a1 == 0))
##   expect_true(all.equal(result@P1,
##                         Matrix(c(10e6, 0, 0, 0, 1.5857, 0.012857, 0, 0.012857, 0.099429), 3, 3))[1] == "TRUE")
## })
