test_that("data param is of class Date", {
  expect_error(pt_is_sunday(date = "2022-04-29"))
})

test_that("data param is of class Date", {
  expect_error(pt_find_last_sunday(date = "2022-04-29"))
})
