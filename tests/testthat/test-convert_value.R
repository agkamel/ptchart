test_that("value greater that 1 return the same value", {
  expect_equal(convert_value(2), 2)
})

test_that("value smaller that 1 return its multiplicative inverse", {
  expect_equal(convert_value(0.5), 2)
})

test_that("negative value raise an error", {
  expect_error(convert_value(-1))
})
