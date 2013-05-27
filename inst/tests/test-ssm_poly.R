context("ssm_poly")

test_that("ssm_poly with defaults works", {
  expect_is(ssm_poly(), "SSM")
})

test_that("ssm_poly with order = 1 works", {
  result <- ssm_poly(1L)
  expect_equivalent(result@T, Matrix(1))
  expect_equivalent(result@Z, Matrix(1))
  expect_equivalent(result@Q, Matrix(1))
})

test_that("ssm_poly with order = 3 works", {
  result <- ssm_poly(3L)
  expect_equivalent(result@T, Matrix(c(1, 0, 0, 1, 1, 0, 0, 1, 1), 3, 3))
  expect_equivalent(result@Z, Matrix(c(1, 0, 0), 1, 3))
  expect_equivalent(result@Q, Matrix(c(0, 0, 0, 0, 0, 0, 0, 0, 1), 3, 3))
})
