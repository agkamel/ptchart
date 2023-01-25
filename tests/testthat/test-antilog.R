test_that("function works properly", {
  expect_equal(antilog(1), 10)
  expect_equal(antilog(2), 100)
})


test_that("function raises an error for input type", {
  expect_error(antilog(x = "string"), "x must be numeric")
  expect_error(antilog(x = 1, base = "string"), "base must be numeric")
})


test_that("function raises an error for getting NaN", {
  expect_error(antilog(x = -1.5, base = -1.5))
})


test_that("function raises an error for getting Inf", {
  expect_error(antilog(x = -1, base = 0))
})
