#' Vectorized functions for summary PT measures
#'
#' @description
#' These funtions are summary PT measures that returns only a single value.
#'
#' @param x Integer. Day of calendar
#' @param y Double. Frequency
#' @param y_cor Double. Correct frequency
#' @param y_incor Double. Incorrect frequency
#'
#' @return A single value as a double scalar.
#'
#' @details
#'
#' # Slope
#'
#' The slope \eqn{b_1} obtained with `b1()` is calculated with
#'
#' \deqn{\frac{\sum (x - \bar{x})(\log_{10}(y) - \log_{10}(\bar{y}))}{\sum(x - \bar{x})^2}}
#'
#' where \eqn{x} is the day and \eqn{y} is the frequency.
#'
#'
#'
#' # Intercept
#'
#' The intercept \eqn{b_0} obtained with `b0()` is calculated with
#'
#' \deqn{\log_{10}(\bar{y}) - b_1 \bar{x}}
#'
#'
#'
#' # Predicted values
#'
#' The predicted values \eqn{\log_{10}(\hat{y})} obtained with `predicted_values()` is calculated with
#'
#' \deqn{b_0 + b_1 x}
#'
#'
#'
#' # Residuals
#'
#' The residuals (errors) \eqn{\epsilon} obtained with `errors()` is calculated with
#'
#' \deqn{\log_{10}(y) - \log_{10}(\hat{y})}
#'
#'
#'
#' # Celeration
#'
#' The celeration \eqn{\mathcal{C}} obtained with `celeration()` is calculated with
#'
#' \deqn{(10^{b_1})^7}
#'
#'
#'
#' # Accuracy ratios
#'
#' The accuracy ratios \eqn{a} obtained with `accuracy_ratio()` is calculated with
#'
#' \deqn{\frac{y_{\text{correct}}}{y_{\text{incorrect}}}}
#'
#'
#'
#' # Accuracy
#'
#' The accuracy \eqn{\mathcal{A}} obtained with `accuracy()` is calculated with
#'
#' \deqn{(10^{(\frac{\sum (x - \bar{x})(a - \bar{a})}{\sum(x - \bar{x})^2})})^7}
#'
#'
#'
#' # Bounce up
#'
#' The bounce up \eqn{\mathcal{B}_{\text{up}}} obtained with `bounce_up()` is calculated with
#'
#' \deqn{10^{\max(\epsilon)}}
#'
#'
#' # Bounce down
#'
#' The bounce down \eqn{\mathcal{B}_{\text{down}}} obtained with `bounce_down()` is calculated with
#'
#' \deqn{10^{\min(\epsilon)}}
#'
#'
#'
#' # Bounce total
#'
#' The bounce total \eqn{\mathcal{B}_{\text{total}}} obtained with `bounce_total()` is calculated with
#'
#' \deqn{\mathcal{B}_{\text{up}} \mathcal{B}_{\text{down}}}
#'
#'
#'
#' @examples
#' # NOT YET
#'
#' @export
#' @describeIn summary_funs Calculate b1
b1 <- function(x, y) {

  if (validate_xy(x, y)$value == FALSE) { stop(validate_xy(x, y)$message) }

  index_to_keep <- (!is.na(x) & !is.na(y))
  if (sum(index_to_keep) >= 3) {
    x <- x[index_to_keep]
    y <- y[index_to_keep]
    log10_y <- log10(y)
  } else {
    return(NA)
  }

  x_deviation_from_mean <- x - mean(x)
  x_deviation_from_mean_squared <- x_deviation_from_mean ^ 2
  log10_y_deviation_from_mean = log10_y - mean(log10_y)

  sum(x_deviation_from_mean * log10_y_deviation_from_mean) / sum(x_deviation_from_mean_squared)
}

#' @export
#' @describeIn summary_funs Calculate b0
b0 <- function(x, y) {

  if (validate_xy(x, y)$value == FALSE) { stop(validate_xy(x, y)$message) }

  index_to_keep <- (!is.na(x) & !is.na(y))
  if (sum(index_to_keep) >= 3) {
    x <- x[index_to_keep]
    y <- y[index_to_keep]
    log10_y <- log10(y)}
  else {
    return(NA)
  }

  b1 <- b1(x, y)
  mean(log10_y) - (b1 * mean(x))
}



#' @export
#' @describeIn summary_funs Calculate celeration
celeration <- function(x, y) {

  if (validate_xy(x, y)$value == FALSE) { stop(validate_xy(x, y)$message) }

  b1 <- b1(x, y)
  (10^b1)^7

}



#' @export
#' @describeIn summary_funs Calculate celeration intercept
celeration_0 <- function(x, y) {

  if (validate_xy(x, y)$value == FALSE) { stop(validate_xy(x, y)$message) }

  b0 <- b0(x, y)
  10^b0

}



#' @export
#' @describeIn summary_funs Calculate bounce up
bounce_up <- function(x, y) {

  if (validate_xy(x, y)$value == FALSE) { stop(validate_xy(x, y)$message) }

  errors <- errors(x, y)
  10^(max(errors))
}

#' @export
#' @describeIn summary_funs Calculate bounce down
bounce_down <- function(x, y) {

  if (validate_xy(x, y)$value == FALSE) { stop(validate_xy(x, y)$message) }

  errors <- errors(x, y)
  10^(min(errors))
}



#' @export
#' @describeIn summary_funs Calculate bounce total
bounce_total <- function(x, y) {

  if (validate_xy(x, y)$value == FALSE) { stop(validate_xy(x, y)$message) }

  bounce_up(x, y) * (1 / bounce_down(x, y))
}



#' @export
#' @describeIn summary_funs Calculate accuracy
accuracy <- function(x, y_cor, y_incor) {

  if (validate_xy(x, y_cor)$value == FALSE) { stop(validate_xy(x, y_cor)$message) }
  if (validate_xy(x, y_incor)$value == FALSE) { stop(validate_xy(x, y_incor)$message) }

  index_to_keep <- (!is.na(x) & !is.na(y_cor) & !is.na(y_incor))
  if (sum(index_to_keep) >= 3) {
    x <- x[index_to_keep]
    y_cor <- y_cor[index_to_keep]
    y_incor <- y_incor[index_to_keep]
  } else {
    return(NA)
  }

  accuracy_ratio <- accuracy_ratio(x, y_cor, y_incor)

  b1 <- b1(x, accuracy_ratio)

  (10^(b1))^7

}








#' Vectorized functions for PT measures
#'
#' @description
#' These funtions are summary PT measures that returns a vector of multiple values.
#'
#' @param x Integer. Day of calendar
#' @param y Double. Frequency
#' @param y_cor Double. Correct frequency
#' @param y_incor Double. Incorrect frequency
#'
#' @return A vector of values.
#' @export
#' @describeIn vct_funs Calculate accuracy ratio
accuracy_ratio <- function(x, y_cor, y_incor) {

  if (validate_xy(x, y_cor)$value == FALSE) { stop(validate_xy(x, y_cor)$message) }
  if (validate_xy(x, y_incor)$value == FALSE) { stop(validate_xy(x, y_incor)$message) }

  index_to_keep <- (!is.na(x) & !is.na(y_cor) & !is.na(y_incor))
  if (sum(index_to_keep) >= 3) {
    x <- x[index_to_keep]
    y_cor <- y_cor[index_to_keep]
    y_incor <- y_incor[index_to_keep]
  } else {
    return(NA)
  }

  y_cor / y_incor

}



#' @export
#' @describeIn vct_funs Calculate predicted values
predicted_values <- function(x, y) {

  if (validate_xy(x, y)$value == FALSE) { stop(validate_xy(x, y)$message) }

  index_to_keep <- (!is.na(x) & !is.na(y))
  if (sum(index_to_keep) >= 3) {
    x <- x[index_to_keep]
    y <- y[index_to_keep]
    log10_y <- log10(y)
  } else {
    return(NA)
  }

  b0 <- b0(x, y)
  b1 <- b1(x, y)

  b0 + b1 * x

}

#' @export
#' @describeIn vct_funs Calculate errors
errors <- function(x, y) {

  if (validate_xy(x, y)$value == FALSE) { stop(validate_xy(x, y)$message) }

  index_to_keep <- (!is.na(x) & !is.na(y))
  if (sum(index_to_keep) >= 3) {
    x <- x[index_to_keep]
    y <- y[index_to_keep]
    log10_y <- log10(y)
  } else {
    return(NA)
  }

  predicted_values <- predicted_values(x, y)
  log10_y - predicted_values
}















# ## Il serait probablement très utile d'ajouter un argument `phase`,
# # pour chacune de ces fonctions.
#
#
# turn <- function(x, y, phase) {
#
#   tibble::tibble(x, y, phase) |>
#     dplyr::group_by(phase) |>
#     dplyr::summarise(cel = celeration(x, y)) |>
#     dplyr::mutate(turn = dplyr::lead(cel) / cel) |>
#     dplyr::pull()
#
# }
#
#
#
#
#
# turn(example_pt_data$jour,
#             example_pt_data$frequence,
#             example_pt_data$phase)
#
# example_pt_data |>
#   dplyr::group_by(phase) |>
#   dplyr::mutate(turn = turn(jour, frequence, phase))
#
#
# temp_df <- tibble::tibble(x = example_pt_data$jour,
#                           y = example_pt_data$frequence,
#                           phase = example_pt_data$phase
#                           )
# #temp_df2 <-
# temp_df |>
#   dplyr::group_by(phase) |>
#   dplyr::summarise(
#     cel = celeration(x, y),
#     last = dplyr::last(predicted_values(x, y)),
#     first = dplyr::first(predicted_values(x, y))
#   ) |>
#   dplyr::mutate(to = dplyr::lead(phase), .after = phase) |>
#   dplyr::mutate(lead_last = dplyr::lead(last),
#                 lead_cel = dplyr::lead(cel)) |>
#   dplyr::mutate(jump = 10^log10(lead_last) / 10^log10(first),
#                 turn = lead_cel / cel)
#
#
# predict_freq <- function(day, b0, b1) {
#   b0 + day * b1
# }
#
# predict_freq(5, 1, 0.02)
#
# (1.5^(1/7))^
#
# 7 * 1.059634
#
# 5^2
#
# 25^(1/2)
#
# predicted_values(example_pt_data$jour, example_pt_data$frequence)
