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
test_that("Arg `freq` in `ptstat()` throw an error if is not greater of equal than zero", {
  expect_error(validate_arg_freq(data = data.frame(x = c(-1, 0, 1)),
                                freq = "x"))
})
test_that("Arg `freq` in `ptstat()` throw an error if contains Inf values", {
  expect_error(validate_arg_freq(data = data.frame(x = c(0, 1, Inf)),
                                 freq = "x"))
})


test_that("Arg `freq_err` in `ptstat()` throw an error if is not numeric", {
  expect_error(validate_arg_freq_err(data = data.frame(x = c("a", "b", "c")),
                                 freq_err = "x"))
})
test_that("Arg `freq_err` in `ptstat()` throw an error if is not greater of equal than zero", {
  expect_error(validate_arg_freq_err(data = data.frame(x = c(-1, 0, 1)),
                                 freq_err = "x"))
})
test_that("Arg `freq_err` in `ptstat()` throw an error if contains Inf values", {
  expect_error(validate_arg_freq_err(data = data.frame(x = c(0, 1, Inf)),
                                 freq_err = "x"))
})





test_that("Arg `phase` in `ptstat()` throw an error if is not character or class `factor`", {
  expect_error(validate_arg_phase(data = data.frame(x = c(0, 1, 2)),
                                  phase = "x"))
  expect_error(validate_arg_phase(data = data.frame(x = c(TRUE, FALSE)),
                                  phase = "x"))
})



test_that("Arg `date` in `ptstat()` throw an error if is not class `Date`", {
  expect_error(validate_arg_date(data = data.frame(x = c(0, 1, 2)),
                                 date = "x"))
})



test_that("Arg `date_zero` in `ptstat()` throw an error if is not class `Date`", {
  expect_error(validate_arg_date_zero(date_zero = "2023-02-18"))
  expect_error(validate_arg_date_zero(date_zero = 1))
})
test_that("Arg `date_zero` in `ptstat()` throw an error if is length greater than 1", {
  expect_error(validate_arg_date_zero(date_zero = as.Date(c("2023-02-18", "2023-02-19"))))
})



test_that("Arg `count` in `ptstat()` throw an error if is not numeric", {
  expect_error(validate_arg_count(data = data.frame(x = c("a", "b", "c")),
                                count = "x"))
})
test_that("Arg `count` in `ptstat()` throw an error if is not greater than zero", {
  expect_error(validate_arg_count(data = data.frame(x = c(-1, 0, 1)),
                                count = "x"))
})
test_that("Arg `count` in `ptstat()` throw an error if is not an integer", {
  expect_error(validate_arg_count(data = data.frame(x = c(0.1, 1)),
                                count = "x"))
})


test_that("Arg `count_err` in `ptstat()` throw an error if is not numeric", {
  expect_error(validate_arg_count_err(data = data.frame(x = c("a", "b", "c")),
                                  count_err = "x"))
})
test_that("Arg `count_err` in `ptstat()` throw an error if is not greater than zero", {
  expect_error(validate_arg_count_err(data = data.frame(x = c(-1, 0, 1)),
                                  count_err = "x"))
})
test_that("Arg `count_err` in `ptstat()` throw an error if is not an integer", {
  expect_error(validate_arg_count_err(data = data.frame(x = c(0.1, 1)),
                                  count_err = "x"))
})


test_that("Arg `time` in `ptstat()` throw an error if is not numeric", {
  expect_error(validate_arg_time(data = data.frame(x = c("a", "b", "c")),
                                 time = "x"))
})
test_that("Arg `time` in `ptstat()` throw an error if is not greater than zero", {
  expect_error(validate_arg_time(data = data.frame(x = c(-1, 0, 1)),
                                 time = "x"))
})



test_that("Arg `time` in `ptstat()` throw an error if is not greater than zero", {
  expect_equal(validate_xy_length(x = c(1, 2, 3),
                                  y = c("a", "b", "c")),
               TRUE)

  expect_equal(validate_xy_length(x = c(1, 2, 3),
                                  y = c("a", "b")),
               FALSE)

})
