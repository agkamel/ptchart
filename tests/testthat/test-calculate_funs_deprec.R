# calculate_b0() ----

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


# calculate_b1() ----

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

# calculate_bounce() ----

# # Bounce
# test_that("function raises an error if `day` is not integer", {
#   expect_error(calculate_bounce(day = "a", log10_freq = 0.30103))
#   expect_error(calculate_bounce(day = 1.0, log10_freq = 0.30103))
#   expect_error(calculate_bounce(day = TRUE, log10_freq = 0.30103))
# })
#
#
# test_that("function raises an error if `log10_freq` is not numeric", {
#   expect_error(calculate_bounce(day = 1, log10_freq = "0.30103"))
#   expect_error(calculate_bounce(day = 1, log10_freq = TRUE))
# })
#
#
# # Bounce up
# test_that("function raises an error if `day` is not integer", {
#   expect_error(calculate_bounce_up(day = "a", log10_freq = 0.30103))
#   expect_error(calculate_bounce_up(day = 1.0, log10_freq = 0.30103))
#   expect_error(calculate_bounce_up(day = TRUE, log10_freq = 0.30103))
# })
#
#
# test_that("function raises an error if `log10_freq` is not numeric", {
#   expect_error(calculate_bounce_up(day = 1, log10_freq = "0.30103"))
#   expect_error(calculate_bounce_up(day = 1, log10_freq = TRUE))
# })
#
#
# # Bounce down
# test_that("function raises an error if `day` is not integer", {
#   expect_error(calculate_bounce_down(day = "a", log10_freq = 0.30103))
#   expect_error(calculate_bounce_down(day = 1.0, log10_freq = 0.30103))
#   expect_error(calculate_bounce_down(day = TRUE, log10_freq = 0.30103))
# })
#
#
# test_that("function raises an error if `log10_freq` is not numeric", {
#   expect_error(calculate_bounce_down(day = 1, log10_freq = "0.30103"))
#   expect_error(calculate_bounce_down(day = 1, log10_freq = TRUE))
# })
#
#
# # Bounce total
# test_that("function raises an error if `day` is not integer", {
#   expect_error(calculate_bounce_total(day = "a", log10_freq = 0.30103))
#   expect_error(calculate_bounce_total(day = 1.0, log10_freq = 0.30103))
#   expect_error(calculate_bounce_total(day = TRUE, log10_freq = 0.30103))
# })
#
#
# test_that("function raises an error if `log10_freq` is not numeric", {
#   expect_error(calculate_bounce_total(day = 1, log10_freq = "0.30103"))
#   expect_error(calculate_bounce_total(day = 1, log10_freq = TRUE))
# })


# calculate_celeration() ----

# test_that("function raises an error if `day` is not integer", {
#   expect_error(calculate_celeration(day = "a", log10_freq = 0.30103))
#   expect_error(calculate_celeration(day = 1.0, log10_freq = 0.30103))
#   expect_error(calculate_celeration(day = TRUE, log10_freq = 0.30103))
# })
#
# test_that("function raises an error if `log10_freq` is not numeric", {
#   expect_error(calculate_celeration(day = 1, log10_freq = "0.30103"))
#   expect_error(calculate_celeration(day = 1, log10_freq = TRUE))
# })


#
