test_that("data param is of class Date", {
  expect_error(pt_is_sunday(date = "2022-04-29"))
})

test_that("data param is of class Date", {
  expect_error(pt_find_last_sunday(date = "2022-04-29"))
})

test_that("output is a sunday", {
  expect_equal(pt_find_last_sunday(date = as.Date("2022-04-29")),
               expected = as.Date("2022-04-24")
  )
})
