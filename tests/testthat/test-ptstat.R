df_test <- ptstat(ptdata01,
       #date = date,
       day = jour,
       #freq = t_frequency,
       #freq_e = nt_frequency,
       count = t_response,
       count_e = nt_response,
       time = minute,
       phase = phase,
       )

df_test$main_df

df_test |>
  ptchart(zoom_x = c(0, 28), zoom_y = c(0.4, 20))

test_that("a S3 object of class ptstat is returned", {
  expect_s3_class(ptstat(ptdata01,
                         day = jour,
                         freq = t_frequency,
                         phase = phase,
                         #freq_e = nt_frequency
                         ),
                  "ptstat")
})

test_that("an error is raised because day, date, or date and date_zero is missing", {
  expect_error(ptstat(ptdata01,
                         # day, date, or day and date missing
                         freq = t_frequency,
                         phase = phase,
                         freq_e = nt_frequency
                      ))
})





test_that("an error is raised because count, freq, count_e, or freq_e is missing.", {
  expect_error(ptstat(ptdata01,
                      day = jour,
                      #freq = t_frequency,
                      #count = t_response,
                      #count_e = nt_response,
                      #freq_e = nt_frequency,
                      #time = minute,
                      phase = phase
  ))

  expect_error(ptstat(ptdata01,
                      day = jour,
                      #freq = t_frequency,
                      #count = t_response,
                      #count_e = nt_response,
                      #freq_e = nt_frequency,
                      time = minute,
                      phase = phase
  ))
})



test_that("is_missing() and is_provided() works properly", {

  expect_equal(is_missing(c(1, 2, 3)),
               FALSE)
  expect_equal(is_missing(c(NA, 2, 3)),
               FALSE)
  expect_equal(is_missing(c(NA, NA, NA)),
               TRUE)

  expect_equal(is_provided(c(1, 2, 3)),
               TRUE)
  expect_equal(is_provided(c(NA, 2, 3)),
               TRUE)
  expect_equal(is_provided(c(NA, NA, NA)),
               FALSE)

})


# Test recoding algorithm

