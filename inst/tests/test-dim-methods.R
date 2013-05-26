context("dim")

test_that("x = MatrixList works", {
  expect_equivalent(dim(MatrixList(replicate(3, Matrix(1, 2, 3)))), c(2L, 3L))
})

test_that("x = SSM works, time-invariant", {
  m <- 3L
  p <- 2L
  r <- 1L
  foo <- new("SSM",
             T = Matrix(diag(m)), Z = Matrix(1, p, m),
             H = Matrix(diag(p)), R = Matrix(diag(1, m, r)), Q = Matrix(diag(r)),
             a1 = Matrix(rep(0, m)), P1 = Matrix(diag(m)),
             cc = Matrix(rep(0, p)), dd = Matrix(rep(0, m)))
  expect_equal(dim(foo), c(p = p, m = m, r = r, n = 0L))
})

test_that("x = SSM works, time-varying", {
  m <- 3L
  p <- 2L
  r <- 1L
  n <- 3L
  foo <- new("SSM",
             T = Matrix(diag(m)), Z = MatrixList(replicate(n, Matrix(1, p, m))),
             H = Matrix(diag(p)), R = Matrix(diag(1, m, r)), Q = Matrix(diag(r)),
             a1 = Matrix(rep(0, m)), P1 = Matrix(diag(m)),
             cc = Matrix(rep(0, p)), dd = Matrix(rep(0, m)))
  expect_equal(dim(foo), c(p = p, m = m, r = r, n = n))
})
