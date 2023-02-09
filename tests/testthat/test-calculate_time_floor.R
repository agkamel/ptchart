test_that("value greater that 0 works", {
  expect_equal(calculate_time_floor(5), 0.2)
  expect_equal(calculate_time_floor(0.2), 5)
})

test_that("non-numeric value raise an error", {
  expect_error(calculate_time_floor("x"))
})

test_that("value smaller or equal than zero raise an error", {
  expect_error(calculate_time_floor(0))
  expect_error(calculate_time_floor(-5))
})

