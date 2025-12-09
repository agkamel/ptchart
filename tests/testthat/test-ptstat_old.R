test_that("function return a s3 class 'ptstat'", {
  withr::local_options(lifecycle_verbosity = "quiet")
  expect_s3_class(
    ptstat_old(ptdata01,
           day = "jour",
           freq = "frequence",
           time = "minute"),
    class = "ptstat")
})


# Conditions for supplying day, date and date_zero
test_that("function return an error because of day or date", {
  withr::local_options(lifecycle_verbosity = "quiet")
  expect_error(
    ptstat_old(ptdata01,
           #day = "jour",
           freq = "frequence",
           time = "minute",
           #date = "date",
           #date_zero = example_pt_date_zero
           ), regexp = "! One of these combinaisons must be supplied:\n    `day`\n    `date`\n    `date` and `date_zero`"
    )

  expect_error(
    ptstat_old(ptdata01,
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
  withr::local_options(lifecycle_verbosity = "quiet")
  expect_error(
    ptstat_old(ptdata01,
           day = "jour",
           freq = "frequence",
           #time = "minute"
           ), regexp = "! `time` must be supplied."
  )
})


# Conditions for supplying freq, count, freq_err and count_err
test_that("function return an error because of freq or count", {
  withr::local_options(lifecycle_verbosity = "quiet")
  expect_error(
    ptstat_old(ptdata01,
           day = "jour",
           #freq = "frequence",
           #count = "reponse",
           time = "minute"
             ), regexp = "! One of these combinaisons must be supplied:\n    `freq`\n    `count`\n    `freq_err`\n    `count_err`"
    )
})



# Condition if not supplying phase
test_that("function return phase = a if not supplied", {
  withr::local_options(lifecycle_verbosity = "quiet")
  expect_message(
    ptstat_old(ptdata01,
           day = "jour",
           count_err = "reponse_nc",
           time = "minute"
           #freq_err = "frequence_nc"
           ), regexp = "i `phase` not supplied. Default phase set to `A` for all observation"
  )
})

