test_that("it works", {
  x <- c(1,2,NA, 4)
  y <- c(10, 15, 16, NA)
  y_incor <- c(20, 15, 12, NA)

  expect_equal(b1(x, y), NA)
  expect_equal(b0(x, y), NA)
  expect_equal(celeration(x, y), NA_real_)
  #expect_equal(accuracy_ratio(x, y, y_incor), NA) # TODO
  expect_equal(accuracy(x, y, y_incor), NA)
  expect_equal(predicted_values(x, y), NA)
  expect_equal(res(x, y), NA)
  expect_equal(bounce_up(x, y), NA_real_)
  expect_equal(bounce_down(x, y), NA_real_)
  expect_equal(bounce_total(x, y), NA_real_)
})


test_that("it works", {
  some_time <- c(1, 2, 100, NA)
  expect_equal(record_floor(some_time, type = "minute"),
               c(1, 0.5, 0.01, NA))
})

test_that("out of bound output values raise an error", {
  some_time_a <- c(1441)
  some_time_b <- c(0.000693)
  expect_error(record_floor(some_time_a, type = "minute"))
  expect_error(record_floor(some_time_b, type = "minute"))

})
