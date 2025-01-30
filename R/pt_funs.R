
# mini_test_data <- tibble::tribble(
#   ~id, ~phase, ~time, ~x_aug, ~x_dim, ~y_aug, ~y_dim,
#   1, "p01", 1, 1, 1, 50, 20,
#   1, "p01", 1, NA, 2, NA, 10,
#    1, "p01", 1, 3, 3, NA, 18,
#    1, "p01", 1, 6, 6, 53, 5,
#    1, "p01", 1, 7, 7, 69, 0.5,
#  1, "p01", 1, 8, 8, 80, 0.5,
#  1, "p02", 1, 11, 11, 50, 20,
#  # 1, "p02", 1, NA, 12, NA, 10,
#  # 1, "p02", 1, 13, 13, 60, 18,
#  # 1, "p02", 1, 16, 16, 53, 5,
#  # 1, "p02", 1, 17, 17, 69, 0.5,
#  # 1, "p02", 1, 18, 18, 80, 0.5,
#  1, "p03", 1, 31, 31, 40, 20,
#  1, "p03", 1, NA, 32, NA, 10,
#  1, "p03", 1, 33, 33, 46, 18,
#  1, "p03", 1, 36, 36, 38, 5,
#  1, "p03", 1, 37, 37, 45, 0.5,
#  1, "p03", 1, 38, 38, 39, 0.5
# )
#
# test_data <- tibble::tribble(
#   ~graph_id,	~phase, 	~obs_no, 	~time, 	~timefloor, 	~x_aug, 	~x_dim, 	~y_aug,	~y_dim,
#   "jptc_1980_v01_i01_a02_g01", "p01",  1L, 1.00244952572313, 0.997556459791471,   9L,   9L, 236.236770886629, 0.475394140347914,
#   "jptc_1980_v01_i01_a02_g01", "p01",  2L, 1.00244952572313, 0.997556459791471,  10L,  10L, 215.111913416979, 0.484788610341206,
#   "jptc_1980_v01_i01_a02_g01", "p01",  3L, 1.00244952572313, 0.997556459791471,  11L,  11L,  232.85823207203, 0.484788610341206,
#   "jptc_1980_v01_i01_a02_g01", "p01",  4L, 1.00244952572313, 0.997556459791471,  12L,  12L, 246.423660654484, 0.484788610341206,
#   "jptc_1980_v01_i01_a02_g01", "p01",  5L, 1.00244952572313, 0.997556459791471,  15L,  15L, 238.192410463543, 0.484788610341206,
#   "jptc_1980_v01_i01_a02_g01", "p01",  6L, 1.00244952572313, 0.997556459791471,  16L,  16L, 215.111913416979, 0.484788610341206,
#   "jptc_1980_v01_i01_a02_g01", "p01",  7L, 1.00244952572313, 0.997556459791471,  17L,  17L, 246.423660654484, 0.484788610341206,
#   "jptc_1980_v01_i01_a02_g01", "p01",  8L, 1.00244952572313, 0.997556459791471,  18L,  18L, 302.140759772642, 0.484788610341206,
#   "jptc_1980_v01_i01_a02_g01", "p01",  9L, 1.00244952572313, 0.997556459791471,  19L,  19L, 272.863760979911, 0.484788610341206,
#   "jptc_1980_v01_i01_a02_g01", "p01", 10L, 1.00244952572313, 0.997556459791471,  22L,  22L, 238.192410463543, 0.484788610341206,
#   "jptc_1980_v01_i01_a02_g01", "p01", 11L, 1.00244952572313, 0.997556459791471,  23L,  23L,  282.29315413436, 0.484788610341206,
#   "jptc_1980_v01_i01_a02_g01", "p01", 12L, 1.00244952572313, 0.997556459791471,  24L,  24L, 346.120448995201, 0.475394140347914,
#   "jptc_1980_v01_i01_a02_g01", "p02",  1L, 1.00244952572313, 0.997556459791471,  44L,  44L, 177.441709521826,  2.02278166534593,
#   "jptc_1980_v01_i01_a02_g01", "p02",  2L, 1.00244952572313, 0.997556459791471,  45L,  45L, 194.267882859334, 0.466181720973328,
#   "jptc_1980_v01_i01_a02_g01", "p02",  3L, 1.00244952572313, 0.997556459791471,  46L,  46L, 222.545567473972, 0.466181720973328,
#   "jptc_1980_v01_i01_a02_g01", "p02",  4L, 1.00244952572313, 0.997556459791471,  47L,  47L, 192.080308890181, 0.466181720973328,
#   "jptc_1980_v01_i01_a02_g01", "p02",  5L, 1.00244952572313, 0.997556459791471,  50L,  50L, 183.573588797661, 0.484788610341206,
#   "jptc_1980_v01_i01_a02_g01", "p02",  6L, 1.00244952572313, 0.997556459791471,  51L,  51L,  192.08030889018, 0.980621512566722,
#   "jptc_1980_v01_i01_a02_g01", "p02",  7L, 1.00244952572313, 0.997556459791471,  52L,  52L, 225.080105726068, 0.475394140347913,
#   "jptc_1980_v01_i01_a02_g01", "p02",  8L, 1.00244952572313, 0.997556459791471,  53L,  53L, 207.926564519538, 0.475394140347913,
#   "jptc_1980_v01_i01_a02_g01", "p02",  9L, 1.00244952572313, 0.997556459791471,  54L,  54L,  232.85823207203, 0.980621512566722,
#   "jptc_1980_v01_i01_a02_g01", "p02", 10L, 1.00244952572313, 0.997556459791471,  57L,  57L, 295.374495856684,  2.02278166534593,
#   "jptc_1980_v01_i01_a02_g01", "p02", 11L, 1.00244952572313, 0.997556459791471,  58L,  58L, 243.648781052684,                 1,
#   "jptc_1980_v01_i01_a02_g01", "p02", 12L, 1.00244952572313, 0.997556459791471,  59L,  59L, 302.140759772642, 0.980621512566722,
#   "jptc_1980_v01_i01_a02_g01", "p03",  1L, 1.00244952572313, 0.997556459791471,  79L,  79L, 181.506439053878,   3.0508388170556,
#   "jptc_1980_v01_i01_a02_g01", "p03",  2L, 1.00244952572313, 0.997556459791471,  80L,  80L, 220.039569656983,                 1,
#   "jptc_1980_v01_i01_a02_g01", "p03",  3L, 1.00244952572313, 0.997556459791471,  81L,  81L, 122.111445973427,   3.0508388170556,
#   "jptc_1980_v01_i01_a02_g01", "p03",  4L, 1.00244952572313, 0.997556459791471,  82L,  82L, 205.585185526039,  1.98358321626376,
#   "jptc_1980_v01_i01_a02_g01", "p03",  5L, 1.00244952572313, 0.997556459791471,  85L,  85L, 222.545567473972, 0.484788610341206,
#   "jptc_1980_v01_i01_a02_g01", "p03",  6L, 1.00244952572313, 0.997556459791471,  86L,  86L, 212.689623044944, 0.484788610341206,
#   "jptc_1980_v01_i01_a02_g01", "p03",  7L, 1.00244952572313, 0.997556459791471,  87L,  87L, 285.508148733351, 0.484788610341206,
#   "jptc_1980_v01_i01_a02_g01", "p03",  8L, 1.00244952572313, 0.997556459791471,  88L,  88L, 227.643509456026, 0.484788610341206,
#   "jptc_1980_v01_i01_a02_g01", "p03",  9L, 1.00244952572313, 0.997556459791471,  89L,  89L, 215.111913416979,                 1,
#   "jptc_1980_v01_i01_a02_g01", "p03", 10L, 1.00244952572313, 0.997556459791471,  92L,  92L, 222.545567473972, 0.475394140347913,
#   "jptc_1980_v01_i01_a02_g01", "p03", 11L, 1.00244952572313, 0.997556459791471,  93L,  93L,  232.85823207203, 0.475394140347913,
#   "jptc_1980_v01_i01_a02_g01", "p03", 12L, 1.00244952572313, 0.997556459791471,  94L,  94L,  240.90514827509, 0.475394140347913,
#   "jptc_1980_v01_i01_a02_g01", "p04",  1L, 1.00244952572313, 0.997556459791471, 114L, 114L, 167.673683060106,  2.02278166534593,
#   "jptc_1980_v01_i01_a02_g01", "p04",  2L, 1.00244952572313, 0.997556459791471, 115L, 115L, 225.080105726068,  5.27690898228132,
#   "jptc_1980_v01_i01_a02_g01", "p04",  3L, 1.00244952572313, 0.997556459791471, 116L, 116L, 160.247866760062,  6.41749352405326,
#   "jptc_1980_v01_i01_a02_g01", "p04",  4L, 1.00244952572313, 0.997556459791471, 117L, 117L, 153.150919884937,  7.35962225610275,
#   "jptc_1980_v01_i01_a02_g01", "p04",  5L, 1.00244952572313, 0.997556459791471, 120L, 120L,  171.51465243038,  6.17118082306573,
#   "jptc_1980_v01_i01_a02_g01", "p04",  6L, 1.00244952572313, 0.997556459791471, 121L, 121L,  200.98122668405,  3.11112776739947,
#   "jptc_1980_v01_i01_a02_g01", "p04",  7L, 1.00244952572313, 0.997556459791471, 122L, 122L, 158.443378767621,  4.17250245199086,
#   "jptc_1980_v01_i01_a02_g01", "p04",  8L, 1.00244952572313, 0.997556459791471, 123L, 123L, 194.267882859334,  2.02278166534593,
#   "jptc_1980_v01_i01_a02_g01", "p04",  9L, 1.00244952572313, 0.997556459791471, 124L, 124L, 183.573588797661,  2.06275473199788,
#   "jptc_1980_v01_i01_a02_g01", "p04", 10L, 1.00244952572313, 0.997556459791471, 127L, 127L, 279.114362320884,   3.0508388170556,
#   "jptc_1980_v01_i01_a02_g01", "p04", 11L, 1.00244952572313, 0.997556459791471, 128L, 128L, 222.545567473972,  3.11112776739947,
#   "jptc_1980_v01_i01_a02_g01", "p04", 12L, 1.00244952572313, 0.997556459791471, 129L, 129L, 246.423660654484,  2.06275473199788
#   )




#' Get base 10 logarithm
#'
#' @param x A numeric vector.
#'
#' @return A numeric Vector.
#' @export
#'
#' @examples
#' #NOT YET
pt_log10 <- function(x) {
  log10(x)
}


.remove_NAs <- function(.df, x, y) {
  .df_original <- .df |> dplyr::select(x = {{ x }}, y = {{ y }})
  .df_no_NAs <- .df_original |> dplyr::filter(!is.na(x), !is.na(y))

  if (nrow(.df_no_NAs) < 3) {
    warning("Number of non NA rows are smaller than 3. Returning NA.")
    return(NA)
  }

  .df_no_NAs
}


#' Get predicted values
#'
#' @param .df A dataframe.
#' @param x Day of calendar.
#' @param y Frequency.
#'
#' @return Predicted values
#' @export
#'
#' @examples
#' # NOT YET
pt_predicted_values <- function(.df, x, y) {

  .df_no_NAs <- .remove_NAs(.df, {{ x }}, {{ y }}) |> dplyr::mutate(log10_y = pt_log10(y))

  b0 <- pt_b0(.df, {{ x }}, {{ y }})
  b1 <- pt_b1(.df, {{ x }}, {{ y }})

  b0 + b1 * .df_no_NAs$x
}


#' Get errors
#'
#' @param .df A dataframe.
#' @param x Day of calendar.
#' @param y Frequency.
#'
#' @return Errors
#' @export
#'
#' @examples
#' # NOT YET
pt_errors <- function(.df, x, y) {

  .df_no_NAs <- .remove_NAs(.df, {{ x }}, {{ y }}) |> dplyr::mutate(log10_y = pt_log10(y))
  predicted_values <- pt_predicted_values(.df, {{ x }}, {{ y }})
  .df_no_NAs$log10_y - predicted_values
}

#' Get b1
#'
#' @param .df A dataframe.
#' @param x Day of calendar.
#' @param y Frequency.
#'
#' @return b1
#' @export
#'
#' @examples
#' #NOT YET
pt_b1 <- function(.df, x, y) {

  .df_no_NAs <- .remove_NAs(.df, {{ x }}, {{ y }}) |> dplyr::mutate(log10_y = pt_log10(y))

  .df_no_NAs <- .df_no_NAs |> dplyr::mutate(
    x = x,
    y = y,
    x_deviation_from_mean = x - mean(x),
    x_deviation_from_mean_squared = x_deviation_from_mean ^ 2,
    log10_y_deviation_from_mean = log10_y - mean(log10_y)
  )

  sum(.df_no_NAs$x_deviation_from_mean * .df_no_NAs$log10_y_deviation_from_mean) / sum(.df_no_NAs$x_deviation_from_mean_squared)
}






#' Get b0
#'
#' @param .df A dataframe.
#' @param x Day of calendar.
#' @param y Frequency.
#'
#' @return b0
#' @export
#'
#' @examples
#' # NOT YET
pt_b0 <- function(.df, x, y) {

  b1 <- pt_b1(.df, {{ x }}, {{ y }})

  .df_no_NAs <- .remove_NAs(.df, {{ x }}, {{ y }}) |> dplyr::mutate(log10_y = pt_log10(y))
  .df_means <- .df_no_NAs |> dplyr::summarise(mean_x = mean(x), mean_log10_y = mean(log10_y))

  .df_means$mean_log10_y - (b1 * .df_means$mean_x)
}


#' Get celeration
#'
#' @param .df A dataframe.
#' @param x Day of calendar.
#' @param y Frequency.
#'
#' @return A dataframe.
#' @export
#'
#' @examples
#' # NOT YET
pt_celeration <- function(.df, x, y) {
  b1 <- pt_b1(.df, {{ x }}, {{ y }})
  (10^b1)^7
}

#  load_all()
#  pt_b1(mini_test_data, x_aug, y_aug)
#  pt_celeration(mini_test_data, x_aug, y_aug)
#  pt_b0(mini_test_data, x_aug, y_aug)


pt_antilog <- function(x, base = 10) {
  base^x
}

#' Get bounce up.
#'
#' @param .df A dataframe.
#' @param x Day of calendar.
#' @param y Frequency.
#'
#' @return Bounce up
#' @export
#'
#' @examples
#' # NOT YET
pt_bounce_up <- function(.df, x, y) {
  errors <- pt_errors(.df, {{ x }}, {{ y }})
  pt_antilog(max(errors))
}

#' Get bounce down.
#'
#' @param .df A dataframe.
#' @param x Day of calendar.
#' @param y Frequency.
#'
#' @return Bounce down
#' @export
#'
#' @examples
#' # NOT YET
pt_bounce_down <- function(.df, x, y) {
  errors <- pt_errors(.df, {{ x }}, {{ y }})
  pt_antilog(min(errors))
}

#' Get bounce total.
#'
#' @param .df A dataframe.
#' @param x Day of calendar.
#' @param y Frequency.
#'
#' @return Bounce total
#' @export
#'
#' @examples
#' # NOT YET
pt_bounce_total <- function(.df, x, y) {
  pt_bounce_up(.df, {{ x }}, {{ y }}) * pt_bounce_down(.df, {{ x }}, {{ y }})
}


#' Get accuracy ratio
#'
#' @param .df A dataframe.
#' @param y_cor Correct frequency.
#' @param y_incor Incorrect frequency.
#'
#' @return Accuracy ratio
#' @export
#'
#' @examples
#' # NOT YET
pt_accuracy_ratio <- function(.df, y_cor, y_incor) {
  .df_no_NAs <- .remove_NAs(.df, {{ y_cor }}, {{ y_incor }}) |>
    dplyr::rename(y_cor = x, y_incor = y)

  .df_no_NAs$y_cor / .df_no_NAs$y_incor

}


#' Get accuracy
#'
#' @param .df A dataframe.
#' @param x Day of calendar.
#' @param y_cor Correct frequency.
#' @param y_incor Incorrect frequency.
#'
#' @return Accuracy
#' @export
#'
#' @examples
#' # NOT YET
pt_accuracy <- function(.df, x, y_cor, y_incor){

  .df_original <- .df |> dplyr::select(x = {{ x }}, y_cor = {{ y_cor }}, y_incor = {{ y_incor }})
  .df_no_NAs <- .df_original |> dplyr::filter(!is.na(x), !is.na(y_cor), !is.na(y_incor))

  accuracy_ratio <- pt_accuracy_ratio(.df_no_NAs, y_cor, y_incor)

  .df_no_NAs <- .df_no_NAs |> dplyr::mutate(accuracy_ratio = accuracy_ratio)

  b1 <- pt_b1(.df_no_NAs, x, accuracy_ratio)

  pt_antilog(b1)^7

}

pt_jump <- function(.df, x, y, phase) {

  .df_original <- .df |> dplyr::select(x = {{ x }}, y = {{ y }}, phase = {{ phase }})
  .df_no_NAs <- .df_original |> dplyr::filter(!is.na(x), !is.na(y), !is.na(phase))

  print(.df_original)
  print(.df_no_NAs)

  .phase_list <- split(.df_no_NAs,
  dplyr::select(.df_no_NAs, phase) |> dplyr::pull())

  print(.phase_list)

  jump_output <- vector(mode = "numeric", length = (length(.phase_list) - 1))

  for (i in 1:(length(.phase_list) - 1)) {

    jump_output[i] <- pt_antilog(
      pt_predicted_values(.phase_list[[i + 1]], x, y) |> dplyr::first() -
        pt_predicted_values(.phase_list[[i]], x, y) |> dplyr::last()
    )
  }

  jump_output
}


