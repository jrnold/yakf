context("SsmSamples")

test_that("SsmSamples function works", {
  foo <- SsmSamples(a = t(Matrix(runif(4))), y = t(Matrix(runif(3))))
  expect_is(foo, "SsmSamples")
})

test_that("SsmSamples function catches error if object@a and object@y are wrong size", {
  expect_error(SsmSamples(a = t(Matrix(runif(3))), y = t(Matrix(runif(3)))),
               "invalid class")
})


