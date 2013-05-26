context("MatrixOrMatrixList class")

test_that("MatrixOrMatrixList function works: list of Matrix", {
  foo <- MatrixOrMatrixList(list(Matrix(1), Matrix(2)))
  expect_is(foo, "MatrixList")
  expect_equal(length(foo), 2L)
})

test_that("MatrixOrMatrixList function works: single Matrix", {
  foo <- MatrixOrMatrixList(Matrix(1))
  expect_is(foo, "Matrix")
})

test_that("MatrixOrMatrixList function works: multiple Matrix args", {
  foo <- MatrixOrMatrixList(Matrix(1), Matrix(2))
  expect_is(foo, "MatrixList")
  expect_equal(length(foo), 2L)
})
