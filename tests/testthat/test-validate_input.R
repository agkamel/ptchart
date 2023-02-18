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


