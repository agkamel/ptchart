test_that("function return a s3 class 'ptstat'", {
  expect_s3_class(
    ptstat(example_pt_data,
           day = "jour",
           freq = "frequence",
           time = "minute"),
    class = "ptstat")
})


# Conditions for supplying day, date and date_zero
test_that("function return an error because of day or date", {
  expect_error(
    ptstat(example_pt_data,
           #day = "jour",
           freq = "frequence",
           time = "minute",
           #date = "date",
           #date_zero = example_pt_date_zero
           ), regexp = "! One of these combinaisons must be supplied:\n    `day`\n    `date`\n    `date` and `date_zero`"
    )

  expect_error(
    ptstat(example_pt_data,
           #day = "jour",
           freq = "frequence",
           time = "minute",
           #date = "date",
           date_zero = example_pt_date_zero
           ), regexp = "! One of these combinaisons must be supplied:\n    `day`\n    `date`\n    `date` and `date_zero`"
  )
})


# Conditions for supplying time
test_that("function return an error because of time", {
  expect_error(
    ptstat(example_pt_data,
           day = "jour",
           freq = "frequence",
           #time = "minute"
           ), regexp = "! `time` must be supplied."
  )
})


# Conditions for supplying freq, count, freq_err and count_err
test_that("function return an error because of freq or count", {
  expect_error(
    ptstat(example_pt_data,
           day = "jour",
           #freq = "frequence",
           #count = "reponse",
           time = "minute"
             ), regexp = "! One of these combinaisons must be supplied:\n    `freq`\n    `count`\n    `freq_err`\n    `count_err`"
    )
})



# Condition if not supplying phase
test_that("function return phase = a if not supplied", {
  expect_message(
    ptstat(example_pt_data,
           day = "jour",
           count_err = "reponse_nc",
           time = "minute"
           #freq_err = "frequence_nc"
           ), regexp = "i `phase` not supplied. Default phase set to `A` for all observation"
  )
})
