# test_that("function raises an error if `day` is not integer", {
#   expect_error(calculate_b0(day = "a", log10_freq = 0.30103))
#   expect_error(calculate_b0(day = 1.0, log10_freq = 0.30103))
#   expect_error(calculate_b0(day = TRUE, log10_freq = 0.30103))
# })
#
# test_that("function raises an error if `log10_freq` is not numeric", {
#   expect_error(calculate_b0(day = 1, log10_freq = "0.30103"))
#   expect_error(calculate_b0(day = 1, log10_freq = TRUE))
# })
#
# test_that("function raises an error if `antilog` is a logical TRUE or FALSE", {
#   expect_error(calculate_b0(day = 1, log10_freq = 0.30103, antilog = NA))
#   expect_error(calculate_b0(day = 1, log10_freq = TRUE, antilog = "TRUE"))
# })

# test_that("test", {
#   calculate_b0(day = c(1, 2), log10_freq = log10(c(1, 2)))
# })
