test_that("all works properly", {

  df_test <- tibble::tibble(
    day = c(1, 2, NA, 4, 8),
    freq_cor = c(10, 15, NA, 18, 20),
    freq_incor = c(40, 30, 32, 26, 20)
  )

  # b1
  expect_equal(df_b1(df_test, day, freq_cor),
               lm(df_log10(freq_cor) ~ day, data = df_test)$coefficients[[2]])

  expect_equal(pt_vct_b1(df_test$day, df_test$freq_cor),
               lm(df_log10(freq_cor) ~ day, data = df_test)$coefficients[[2]])

  # b0
  expect_equal(df_b0(df_test, day, freq_cor),
               lm(df_log10(freq_cor) ~ day, data = df_test)$coefficients[[1]])

  expect_equal(pt_vct_b0(df_test$day, df_test$freq_cor),
               lm(df_log10(freq_cor) ~ day, data = df_test)$coefficients[[1]])

  # celeration
  expect_equal(df_celeration(df_test, day, freq_cor),
               (10^(lm(df_log10(freq_cor) ~ day, data = df_test)$coefficients[[2]]))^7)



  # predicted_values
  expect_equal(df_predicted_values(df_test, day, freq_cor),
               lm(df_log10(freq_cor) ~ day, data = df_test) |> predict() |> unname())

  expect_equal(pt_vct_predicted_values(df_test$day, df_test$freq_cor),
               lm(df_log10(freq_cor) ~ day, data = df_test) |> predict() |> unname())

  # errors
  expect_equal(df_errors(df_test, day, freq_cor),
               lm(df_log10(freq_cor) ~ day, data = df_test) |> residuals() |> unname())

  expect_equal(pt_vct_errors(df_test$day, df_test$freq_cor),
               lm(df_log10(freq_cor) ~ day, data = df_test) |> residuals() |> unname())



  # bounce up
  expect_equal(df_bounce_up(df_test, day, freq_cor),
               lm(df_log10(freq_cor) ~ day, data = df_test) |>
                 residuals() |>
                 max() |>
                 antilog()
  )

  # bounce down
  expect_equal(df_bounce_down(df_test, day, freq_cor),
               lm(df_log10(freq_cor) ~ day, data = df_test) |>
                 residuals() |>
                 min() |>
                 antilog()
  )

  # bounce total
  expect_equal(df_bounce_total(df_test, day, freq_cor),
               (lm(df_log10(freq_cor) ~ day, data = df_test) |>
                 residuals() |>
                 min() |>
                 antilog()) * (lm(df_log10(freq_cor) ~ day, data = df_test) |>
                 residuals() |>
                 max() |>
                 antilog()))
})
