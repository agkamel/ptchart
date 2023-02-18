test_that("Arg `data` in `ptstat()` throw an error if is not a data.frame", {
  expect_error(validate_arg_data("x"))
  expect_error(validate_arg_data(list(1,2)))
  expect_error(validate_arg_data(1))
  expect_error(validate_arg_data(1L))
})



test_that("Arg `day` in `ptstat()` throw an error if is not numeric", {
  expect_error(validate_arg_day(data = data.frame(x = c("a", "b", "c")),
                                day = "x"))
})
test_that("Arg `day` in `ptstat()` throw an error if is not greater than zero", {
  expect_error(validate_arg_day(data = data.frame(x = c(-1, 0, 1)),
                                day = "x"))
})
test_that("Arg `day` in `ptstat()` throw an error if is not an integer", {
  expect_error(validate_arg_day(data = data.frame(x = c(0.1, 1)),
                                day = "x"))
})



test_that("Arg `freq` in `ptstat()` throw an error if is not numeric", {
  expect_error(validate_arg_freq(data = data.frame(x = c("a", "b", "c")),
                                freq = "x"))
})
test_that("Arg `freq` in `ptstat()` throw an error if is not greater than zero", {
  expect_error(validate_arg_freq(data = data.frame(x = c(-1, 0, 1)),
                                freq = "x"))
})
test_that("Arg `freq` in `ptstat()` throw an error if contains Inf or -Inf values", {
  expect_error(validate_arg_freq(data = data.frame(x = c(0, 1, Inf)),
                                 freq = "x"))
  expect_error(validate_arg_freq(data = data.frame(x = c(0, 1, -Inf)),
                                 freq = "x"))
})



test_that("Arg `phase` in `ptstat()` throw an error if is not character or class `factor`", {
  expect_error(validate_arg_phase(data = data.frame(x = c(0, 1, 2)),
                                  phase = "x"))
  expect_error(validate_arg_phase(data = data.frame(x = c(TRUE, FALSE)),
                                  phase = "x"))
})
