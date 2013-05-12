# easy construction of DLM for testing purposes
construct_dlm <- function(T = Matrix(c(1, 0, 1, 1), 2, 2),
                          Z = Matrix(c(1, 1, 0, 0), 2, 2),
                          HH = Matrix(diag(1, 2, 2)),
                          GG = Matrix(1, 2, 2),
                          dd = Matrix(0, 2, 1),
                          cc = Matrix(0, 2, 1),
                          a1 = Matrix(0, 2, 1),
                          P1 = Matrix(diag(1e6, 2, 2)),
                          HG = Matrix(0, 2, 2),
                          tv_T = matrix(nrow = 0, ncol = 3),
                          tv_Z = matrix(nrow = 0, ncol = 3),
                          tv_HH = matrix(nrow = 0, ncol = 3),
                          tv_GG = matrix(nrow = 0, ncol = 3),
                          tv_HG = matrix(nrow = 0, ncol = 3),
                          tv_cc = matrix(nrow = 0, ncol = 3),
                          tv_dd = matrix(nrow = 0, ncol = 3),
                          X = Matrix(nrow = 0, ncol = 0)) {
  new("DLM",
      T = T, Z = Z,
      HH = HH, GG = GG,
      cc = cc, dd = dd,
      a1 = a1, P1 = P1,
      HG = HG,
      tv_T = tv_T, tv_Z = tv_Z,
      tv_HH = tv_HH, tv_GG = tv_GG,
      tv_cc = tv_cc, tv_dd = tv_dd,
      tv_HG = tv_HG,
      X = X)
}

test_that("new DLM works", {
  expect_is(construct_dlm(), "DLM")
})

test_that("Errors if asymmetric covariance matrices", {
  expect_error(construct_dlm(HH = Matrix(1:4, 2, 2)),
               "is not symmetric")
  expect_error(construct_dlm(GG = Matrix(1:4, 2, 2)),
               "is not symmetric")
  expect_error(construct_dlm(P1 = Matrix(1:4, 2, 2)),
               "is not symmetric")
})

construct_dlm_2 <- function(T = Matrix(c(1, 0, 1, 1), 2, 2),
                            Z = Matrix(c(1, 1, 0, 0), 2, 2),
                            HH = Diagonal(2),
                            GG = Diagonal(2), ...) {
  DLM(T = T, Z = Z, HH = HH, GG = GG, ...)
}

test_that("DLM function works with minimal args", {
  construct_dlm_2()  
})

test_that("dlm_T works with TV values", {
  foo <- construct_dlm(tv_T = matrix(c(1, 1, 1), 1, 3),
                       X = Matrix(1:10, ncol = 1))
  expect_equal(dlm_T(foo, 2)[1 , 1], 2)
})

test_that("dlm_Omega works", {
  foo <- construct_dlm()
  expect_identical(dim(dlm_Omega(foo)), c(4L, 4L))
})

test_that("dlm_Phi works", {
  foo <- construct_dlm()
  expect_identical(dim(dlm_Phi(foo)), c(4L, 2L))
})

test_that("dlm_delta works", {
  foo <- construct_dlm()
  delta <- dlm_delta(foo)
  expect_is(delta, "Matrix")
  expect_identical(dim(delta), c(4L, 1L))
})

test_that("dlm_Sigma works", {
  foo <- construct_dlm()
  Sigma <- dlm_Sigma(foo)
  expect_is(Sigma, "Matrix")
  expect_identical(dim(Sigma), c(3L, 2L))
})

