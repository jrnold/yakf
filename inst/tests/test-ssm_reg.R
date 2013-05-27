context("ssm_reg")

local({
  X <- Matrix(1:4, 2, 2)

  test_that("ssm_reg: no intercept works", {
    result <- ssm_reg(X, intercept = FALSE)
    expect_equal(result@Z[[1]], Matrix(c(1, 3), 1, 2))
    expect_equal(result@Z[[2]], Matrix(c(2, 4), 1, 2))
    expect_equal(result@T, Diagonal(2))
  })

  test_that("ssm_reg: with intercept works", {
    result <- ssm_reg(X, intercept = TRUE)
    expect_equal(result@Z[[1]], Matrix(c(1, 1, 3), 1))
    expect_equal(result@Z[[2]], Matrix(c(1, 2, 4), 1))
    expect_equal(result@T, Diagonal(3))
  })

  test_that("ssm_reg: with rho works", {
    rho <- seq_len(ncol(X) + 1) * 0.1
    result <- ssm_reg(X, rho = rho)
    expect_equal(result@Z[[1]], Matrix(c(1, 1, 3), 1))
    expect_equal(result@Z[[2]], Matrix(c(1, 2, 4), 1))
    expect_equal(result@T, Diagonal(3, rho))
  })
  
})
