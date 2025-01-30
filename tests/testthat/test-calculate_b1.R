# test_that("function raises an error if `day` is not integer", {
#   expect_error(calculate_b1(day = "a", log10_freq = 0.30103))
#   expect_error(calculate_b1(day = 1.0, log10_freq = 0.30103))
#   expect_error(calculate_b1(day = TRUE, log10_freq = 0.30103))
# })
#
#
# test_that("function raises an error if `log10_freq` is not numeric", {
#   expect_error(calculate_b1(day = 1, log10_freq = "0.30103"))
#   expect_error(calculate_b1(day = 1, log10_freq = TRUE))
# })
#
#
test_that("function raises an error if `antilog` is a logical TRUE or FALSE", {
  expect_error(calculate_b1(day = 1, log10_freq = 0.30103, antilog = NA))
  expect_error(calculate_b1(day = 1, log10_freq = 0.30103, antilog = "TRUE"))
})


test_that("function works correctly", {
  expect_equal(calculate_b1(day = c(1, 8),
                            log10_freq = log10(c(1, 2))) |>
                 round(8),
               expected = 0.04300429)

  expect_equal(calculate_b1(day = c(1, 8),
                            log10_freq = log10(c(1, 0.5))) |>
                 round(8),
               expected = -0.04300429)
})


# test_that("function raises an error if `antilog` is a logical TRUE or FALSE", {
#   expect_error(calculate_b1(day = 1, log10_freq = 0.30103))
#   expect_error(calculate_b1(day = 1, log10_freq = 0.30103))
# })
