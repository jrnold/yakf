context("Ops methods")
library(Matrix)

foo <- MatrixList(Matrix(1:2), Matrix(3:4))
bar <- MatrixList(Matrix(5:6), Matrix(7:8))

test_that("e1 = MatrixList, e2 = MatrixList works", {
  expected <- MatrixList(Matrix(c(6, 8)), Matrix(c(10, 12)))
  expect_equivalent(foo + bar, expected)
})

test_that("e1 = MatrixList, e2 = Matrix works", {
  expected <- MatrixList(Matrix(c(2, 4)), Matrix(c(5, 6)))
  expect_equivalent(foo + foo[[1]], expected)
})

test_that("e1 = Matrix, e2 = MatrixList works", {
  expected <- MatrixList(Matrix(c(2, 4)), Matrix(c(5, 6)))
  expect_equivalent(foo[[1]] + foo, expected)
})
