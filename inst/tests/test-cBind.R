context("test cBind")

test_that("x = MatrixList, y = MatrixList works", {
  expected <- MatrixList(Matrix(c(1, 3), 1, 2),
                         Matrix(c(2, 4), 1, 2))
  expect_equivalent(cBind(MatrixList(Matrix(1), Matrix(2)),
                          MatrixList(Matrix(3), Matrix(4))),
                    expected)
})

test_that("x = MatrixList, y = Matrix works", {
  expected <- MatrixList(Matrix(c(1, 3), 1, 2),
                         Matrix(c(2, 3), 1, 2))
  expect_equivalent(cBind(MatrixList(Matrix(1), Matrix(2)), Matrix(3)), 
                    expected)
})

test_that("x = Matrix, y = MatrixList works", {
  expected <- MatrixList(Matrix(c(1, 3), 1, 2),
                         Matrix(c(1, 4), 1, 2))
  expect_equivalent(cBind(Matrix(1), MatrixList(Matrix(3), Matrix(4))),
                    expected)
})
