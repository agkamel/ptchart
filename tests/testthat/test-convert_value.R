test_that("value greater that 1 return the same value", {
  expect_equal(convert_value(2), 2)
})

test_that("value smaller that 1 return its multiplicative inverse", {
  expect_equal(convert_value(0.5), 2)
})

test_that("negative value raise an error", {
  expect_error(convert_value(-1))
})

test_that("NA_real_ return NA_real_", {
  expect_equal(convert_value(NA_real_), NA_real_)
})

test_that("NA stay NA within numeric vector", {
  expect_equal(convert_value(c(0.5, NA)), c(2, NA))
})
