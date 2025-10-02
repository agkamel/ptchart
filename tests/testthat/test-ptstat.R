df_test <- ptstat(example_pt_data,
       #date = date,
       day = jour,
       #freq = frequence,
       #freq_e = frequence_nc,
       count = reponse,
       count_e = reponse_nc,
       time = minute,
       phase = phase,
       )

df_test$main_df

df_test |>
  ptchart(zoom_x = c(0, 28), zoom_y = c(0.4, 20))

test_that("a S3 object of class ptstat is returned", {
  expect_s3_class(ptstat(example_pt_data,
                         day = jour,
                         freq = frequence,
                         phase = phase,
                         #freq_e = frequence_nc
                         ),
                  "ptstat")
})

test_that("an error is raised because day, date, or date and date_zero is missing", {
  expect_error(ptstat(example_pt_data,
                         # day, date, or day and date missing
                         freq = frequence,
                         phase = phase,
                         freq_e = frequence_nc
                      ))
})





test_that("an error is raised because count, freq, count_e, or freq_e is missing.", {
  expect_error(ptstat(example_pt_data,
                      day = jour,
                      #freq = frequence,
                      #count = reponse,
                      #count_e = reponse_nc,
                      #freq_e = frequence_nc,
                      #time = minute,
                      phase = phase
  ))

  expect_error(ptstat(example_pt_data,
                      day = jour,
                      #freq = frequence,
                      #count = reponse,
                      #count_e = reponse_nc,
                      #freq_e = frequence_nc,
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

