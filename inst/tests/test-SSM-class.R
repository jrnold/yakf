context("SSM-class")

test_that("SSM class with valid entries works", {
  m <- 2
  p <- 1
  r <- m
  new("SSM",
      T = Matrix(diag(m)), Z = Matrix(1, p, m),
      H = Matrix(diag(p)), R = Matrix(diag(m)), Q = Matrix(diag(r)),
      a1 = Matrix(rep(0, m)), P1 = Matrix(diag(m)),
      cc = Matrix(rep(0, p)), dd = Matrix(rep(0, m)))
})

test_that("SSM function with only required args works", {
  m <- 2
  p <- 1
  SSM(T = Matrix(diag(m)), Z = Matrix(1, p, m), H = Matrix(diag(p)), Q = Matrix(diag(m)))
})

test_that("SSM catches invalid matrices", {
  m <- 3
  p <- 3
  args <- list(T = Matrix(diag(m)), Z = Matrix(1, p, m), H = Matrix(diag(p)), Q = Matrix(diag(m)))
  for (i in c("T", "Z", "H", "R", "Q", "a1", "P")) {
    args2 <- args
    args2[[i]] <- Matrix(1)
    expect_error(do.call(SSM, args2), "Dimensions of slot")
  }
})

test_that("SSM catches different length time-varying matrices", {
  m <- 3
  p <- 3
  args <- list(T = replicate(3, Matrix(diag(m))),
               Z = replicate(2, Matrix(1, p, m)),
               H = Matrix(diag(p)),
               Q = Matrix(diag(m)))
  expect_error(do.call(SSM, args), "invalid class")
})
