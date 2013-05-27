context("ssm_samples")

test_that("ssm_samples works", {
  n <- 5
  foo <- SSM(Z = Matrix(c(1, 0), 1, 2), H = 1,
             T = Matrix(c(1, 0, 1, 1), 2, 2), Q = Matrix(diag(2)))
  results <- ssm_samples(foo, n = n, a1= rep(0, 2))
  expect_is(results, "SsmSamples")
  expect_equal(ncol(results@a), n + 1)
})

test_that("ssm_samples works, time-varying SSM", {
  n <- 3
  foo_tv <- SSM(Z = replicate(n, Matrix(c(1, 0), 1, 2)),
                H = replicate(n, Matrix(1)),
                T = replicate(n, Matrix(c(1, 0, 1, 1), 2, 2)),
                Q = replicate(n, Matrix(diag(2))))
  results <- ssm_samples(foo_tv, a1 = rep(0, 2))
  expect_is(results, "SsmSamples")
  expect_equivalent(ncol(results@a), dim(foo_tv)['n'] + 1)
})

test_that("ssm_samples error if no n and time-invariant", {
  foo <- SSM(Z = Matrix(c(1, 0), 1, 2), H = 1,
             T = Matrix(c(1, 0, 1, 1), 2, 2), Q = Matrix(diag(2)))
  expect_error(ssm_samples(foo), "n must be specified")
})

test_that("ssm_samples warning if n specified and when time-variant SSM", {
  foo <- SSM(Z = replicate(2, Matrix(c(1, 0), 1, 2)), H = 1,
             T = Matrix(c(1, 0, 1, 1), 2, 2), Q = Matrix(diag(2)))
  expect_warning(ssm_samples(foo, n = 5))
})





