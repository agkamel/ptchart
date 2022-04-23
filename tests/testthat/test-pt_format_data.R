test_that("different types of dataframe works", {
  my_date_zero <- as.Date("2022-09-18")

  # tibble()
  my_data <- tibble::tibble(my_date = as.Date("2022-09-23"),
                            my_count = 2,
                            my_time = 1,
                            my_phase = factor("a")
                            )
  expect_true("tbl_df" %in% class(pt_format_data(data = my_data,
                                                     date = my_date,
                                                     count = my_count,
                                                     time = my_time,
                                                     phase = my_phase,
                                                     date_zero = my_date_zero)
                                )
  )

  # data.frame()
  my_data <- data.frame(my_date = as.Date("2022-09-23"),
                        my_count = 2,
                        my_time = 1,
                        my_phase = factor("a")
                        )
  expect_true("data.frame" %in% class(pt_format_data(data = my_data,
                                                 date = my_date,
                                                 count = my_count,
                                                 time = my_time,
                                                 phase = my_phase,
                                                 date_zero = my_date_zero)
                                  )
  )
})

test_that("input data type are in the right format", {

  # date error
  my_date_zero <- as.Date("2022-09-18")
  my_data <- data.frame(my_date = "2022-09-23", # error
                        my_count = 2,
                        my_time = 1,
                        my_phase = factor("a")
  )
  expect_error(pt_format_data(data = my_data,
                              date = my_date, # error
                              count = my_count,
                              time = my_time,
                              phase = my_phase,
                              date_zero = my_date_zero)
               )

  # date_zero error
  my_date_zero <- "2022-09-18" # error
  my_data <- data.frame(my_date = as.Date("2022-09-23"),
                        my_count = 2,
                        my_time = 1,
                        my_phase = factor("a")
  )
  expect_error(pt_format_data(data = my_data,
                              date = my_date,
                              count = my_count,
                              time = my_time,
                              phase = my_phase,
                              date_zero = my_date_zero) #error
  )

  # count error
  my_date_zero <- as.Date("2022-09-18")
  my_data <- data.frame(my_date = as.Date("2022-09-23"),
                        my_count = "2", # error
                        my_time = 1,
                        my_phase = factor("a")
  )
  expect_error(pt_format_data(data = my_data,
                              date = my_date,
                              count = my_count, # error
                              time = my_time,
                              phase = my_phase,
                              date_zero = my_date_zero)
               )

  # count error 2
  my_date_zero <- as.Date("2022-09-18")
  my_data <- data.frame(my_date = as.Date("2022-09-23"),
                        my_count = TRUE, # error
                        my_time = 1,
                        my_phase = factor("a")
  )
  expect_error(pt_format_data(data = my_data,
                              date = my_date,
                              count = my_count, # error
                              time = my_time,
                              phase = my_phase,
                              date_zero = my_date_zero)
  )

  # time error
  my_date_zero <- as.Date("2022-09-18")
  my_data <- data.frame(my_date = as.Date("2022-09-23"),
                        my_count = 2,
                        my_time = "1", # error
                        my_phase = factor("a")
  )
  expect_error(pt_format_data(data = my_data,
                              date = my_date,
                              count = my_count,
                              time = my_time, # error
                              phase = my_phase,
                              date_zero = my_date_zero)
  )

})

