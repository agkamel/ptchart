test_that("function return a s3 class 'ptstat'", {
  expect_s3_class(
    ptstat(example_pt_data,
           day = "jour",
           freq = "frequence"),
    class = "ptstat")
})


# Conditions for supplying day, date and date_zero
test_that("function return an error because of day or date", {
  expect_error(
    ptstat(example_pt_data,
           #day = "jour",
           freq = "frequence",
           #date = "date",
           #date_zero = example_pt_date_zero
           )
    )

  expect_error(
    ptstat(example_pt_data,
           #day = "jour",
           freq = "frequence",
           #date = "date",
           date_zero = example_pt_date_zero
           )
  )
})


# Conditions for supplying freq, count and time
test_that("function return an error because of freq or count and time", {
  expect_error(
    ptstat(example_pt_data,
           day = "jour",
           #freq = "frequence",
           #count = "reponse",
           #time = "minute"
             )
  )

  expect_error(
    ptstat(example_pt_data,
           day = "jour",
           #freq = "frequence",
           count = "reponse",
           #time = "minute"
    )
  )

  expect_error(
    ptstat(example_pt_data,
           day = "jour",
           #freq = "frequence",
           #count = "reponse",
           time = "minute"
    )
  )

})



