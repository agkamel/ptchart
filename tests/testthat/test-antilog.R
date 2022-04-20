test_that("function works properly", {
  expect_equal(antilog(1), 10)
  expect_equal(antilog(2), 100)
})

test_that("function returns an error", {
  expect_error(antilog(x = "string"), "x must be numeric")
  expect_error(antilog(x = 1, base = "string"), "base must be numeric")
})
