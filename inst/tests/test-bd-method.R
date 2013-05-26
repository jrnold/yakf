context("test %bd%-methods")

test_that("e1 = Matrix, e2 = Matrix works", {
  foo <- Matrix(1, 2, 2)
  bar <- Matrix(2, 1, 1)
  expect_equivalent(foo %bd% bar, Matrix::bdiag(foo, bar))
})

test_that("e1 = MatrixList, e2 = MatrixList works", {
  foo <- MatrixList(Matrix(1, 2, 2), Matrix(2, 2, 2))
  bar <- MatrixList(Matrix(1, 1, 1), Matrix(2))
  expect_equivalent(foo %bd% bar,
                    MatrixList(bdiag(foo[[1]], bar[[1]]),
                               bdiag(foo[[2]], bar[[2]])))
})

test_that("e1 = Matrix, e2 = MatrixList works", {
  foo <- MatrixList(Matrix(1, 2, 2), Matrix(2, 2, 2))
  bar <- Matrix(1, 1, 1)
  expect_equivalent(foo %bd% bar,
                    MatrixList(bdiag(foo[[1]], bar),
                               bdiag(foo[[2]], bar)))
})

test_that("e1 = MatrixList, e2 = Matrix works", {
  foo <- MatrixList(Matrix(1, 2, 2), Matrix(2, 2, 2))
  bar <- Matrix(1, 1, 1)
  expect_equivalent(bar %bd% foo,
                    MatrixList(bdiag(bar, foo[[1]]),
                               bdiag(bar, foo[[2]])))
})
