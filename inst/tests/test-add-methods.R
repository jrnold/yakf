context("+ methods")

test_that("+,SSM,SSM-method works", {
  foo <- SSM(T = Matrix(1), Z = Matrix(1),
             H = Matrix(1), Q = Matrix(1),
             cc = Matrix(1), dd = Matrix(1),
             a1 = Matrix(1), P1 = Matrix(1))
  
  bar <- SSM(T = Matrix(2), Z = Matrix(2),
             H = Matrix(2), Q = Matrix(2),
             cc = Matrix(2), dd = Matrix(2),
             a1 = Matrix(2), P1 = Matrix(2))
  
  baz <- foo + bar
  expect_equivalent(baz@T, Matrix(c(1, 0, 0, 2), 2, 2))
  expect_equivalent(baz@Z, Matrix(c(1, 2), 1, 2))
  expect_equivalent(baz@H, Matrix(3))
  expect_equivalent(baz@Q, Matrix(c(1, 0, 0, 2), 2, 2))
  expect_equivalent(baz@cc, Matrix(c(3)))
  expect_equivalent(baz@dd, Matrix(c(1, 2)))
  expect_equivalent(baz@a1, Matrix(c(1, 2)))
  expect_equivalent(baz@P1, Matrix(c(1, 0, 0, 2), 2, 2))
})

test_that("+,SSM,SSM-method throws error if dim(e1) ! = dim(e2)", {
  foo <- SSM(T = Matrix(1), Z = replicate(3, Matrix(1)),
             H = Matrix(1), Q = Matrix(1))
  bar <- SSM(T = Matrix(2), Z = replicate(2, Matrix(2)),
             H = Matrix(2), Q = Matrix(2))
  expect_error(foo + bar,
               "number of time periods in objects are not equal")
})
