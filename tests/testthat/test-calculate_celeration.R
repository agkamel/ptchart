test_that("function raises an error if `day` is not integer", {
  expect_error(calculate_celeration(day = "a", log10_freq = 0.30103))
  expect_error(calculate_celeration(day = 1.0, log10_freq = 0.30103))
  expect_error(calculate_celeration(day = TRUE, log10_freq = 0.30103))
})

test_that("function raises an error if `log10_freq` is not numeric", {
  expect_error(calculate_celeration(day = 1, log10_freq = "0.30103"))
  expect_error(calculate_celeration(day = 1, log10_freq = TRUE))
})
