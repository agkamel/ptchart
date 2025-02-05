#' Functions for calculating pt values with two vectors
#'
#' @param x Day of calendar
#' @param y Frequency
#' @param y_cor Correct frequency
#' @param y_incor Incorrect frequency
#'
#' @return A single value or a vector
#'
#' @details
#'
#' # Slope
#'
#' The slope \eqn{b_1} obtained with `pt_vct_b1()` is calculated with
#'
#' \deqn{\frac{\sum (x - \bar{x})(\log_{10}(y) - \log_{10}(\bar{y}))}{\sum(x - \bar{x})^2}}
#'
#' where \eqn{x} is the day and \eqn{y} is the frequency.
#'
#'
#'
#' # Intercept
#'
#' The intercept \eqn{b_0} obtained with `pt_vct_b0()` is calculated with
#'
#' \deqn{\log_{10}(\bar{y}) - b_1 \bar{x}}
#'
#'
#'
#' # Predicted values
#'
#' The predicted values \eqn{\log_{10}(\hat{y})} obtained with `pt_vct_predicted_values()` is calculated with
#'
#' \deqn{b_0 + b_1 x}
#'
#'
#'
#' # Residuals
#'
#' The residuals (errors) \eqn{\epsilon} obtained with `pt_vct_errors()` is calculated with
#'
#' \deqn{\log_{10}(y) - \log_{10}(\hat{y})}
#'
#'
#'
#' # Celeration
#'
#' The celeration \eqn{\mathcal{C}} obtained with `pt_vct_celeration()` is calculated with
#'
#' \deqn{(10^{b_1})^7}
#'
#'
#'
#' # Accuracy ratios
#'
#' The accuracy ratios \eqn{a} obtained with `pt_vct_accuracy_ratio()` is calculated with
#'
#' \deqn{\frac{y_{\text{correct}}}{y_{\text{incorrect}}}}
#'
#'
#'
#' # Accuracy
#'
#' The accuracy \eqn{\mathcal{A}} obtained with `pt_vct_accuracy()` is calculated with
#'
#' \deqn{(10^{(\frac{\sum (x - \bar{x})(a - \bar{a})}{\sum(x - \bar{x})^2})})^7}
#'
#'
#'
#' # Bounce up
#'
#' The bounce up \eqn{\mathcal{B}_{\text{up}}} obtained with `pt_vct_bounce_up()` is calculated with
#'
#' \deqn{10^{\max(\epsilon)}}
#'
#'
#' # Bounce down
#'
#' The bounce down \eqn{\mathcal{B}_{\text{down}}} obtained with `pt_vct_bounce_down()` is calculated with
#'
#' \deqn{10^{\min(\epsilon)}}
#'
#'
#'
#' # Bounce total
#'
#' The bounce total \eqn{\mathcal{B}_{\text{total}}} obtained with `pt_vct_bounce_total()` is calculated with
#'
#' \deqn{\mathcal{B}_{\text{up}} \mathcal{B}_{\text{down}}}
#'
#'
#'
#' @examples
#' # NOT YET
#'
#' @export
#' @describeIn pt_vct_funs Calculate b1
pt_vct_b1 <- function(x, y) {

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
#' @describeIn pt_vct_funs Calculate b0
pt_vct_b0 <- function(x, y) {

  if (validate_xy(x, y)$value == FALSE) { stop(validate_xy(x, y)$message) }

  index_to_keep <- (!is.na(x) & !is.na(y))
  if (sum(index_to_keep) >= 3) {
    x <- x[index_to_keep]
    y <- y[index_to_keep]
    log10_y <- log10(y)}
  else {
    return(NA)
  }

  b1 <- pt_vct_b1(x, y)
  mean(log10_y) - (b1 * mean(x))
}

#' @export
#' @describeIn pt_vct_funs Calculate predicted values
pt_vct_predicted_values <- function(x, y) {

  if (validate_xy(x, y)$value == FALSE) { stop(validate_xy(x, y)$message) }

  index_to_keep <- (!is.na(x) & !is.na(y))
  if (sum(index_to_keep) >= 3) {
    x <- x[index_to_keep]
    y <- y[index_to_keep]
    log10_y <- log10(y)
  } else {
    return(NA)
  }

  b0 <- pt_vct_b0(x, y)
  b1 <- pt_vct_b1(x, y)

  b0 + b1 * x

}

#' @export
#' @describeIn pt_vct_funs Calculate errors
pt_vct_errors <- function(x, y) {

  if (validate_xy(x, y)$value == FALSE) { stop(validate_xy(x, y)$message) }

  index_to_keep <- (!is.na(x) & !is.na(y))
  if (sum(index_to_keep) >= 3) {
    x <- x[index_to_keep]
    y <- y[index_to_keep]
    log10_y <- log10(y)
  } else {
    return(NA)
  }

  predicted_values <- pt_vct_predicted_values(x, y)
  log10_y - predicted_values
}

#' @export
#' @describeIn pt_vct_funs Calculate celeration
pt_vct_celeration <- function(x, y) {

  if (validate_xy(x, y)$value == FALSE) { stop(validate_xy(x, y)$message) }

  b1 <- pt_vct_b1(x, y)
  (10^b1)^7

}

#' @export
#' @describeIn pt_vct_funs Calculate accuracy ratio
pt_vct_accuracy_ratio <- function(x, y_cor, y_incor) {

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
#' @describeIn pt_vct_funs Calculate accuracy
pt_vct_accuracy <- function(x, y_cor, y_incor) {

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

  accuracy_ratio <- pt_vct_accuracy_ratio(x, y_cor, y_incor)

  b1 <- pt_vct_b1(x, accuracy_ratio)

  (10^(b1))^7

}

#' @export
#' @describeIn pt_vct_funs Calculate bounce up
pt_vct_bounce_up <- function(x, y) {

  if (validate_xy(x, y)$value == FALSE) { stop(validate_xy(x, y)$message) }

  errors <- pt_vct_errors(x, y)
  10^(max(errors))
}

#' @export
#' @describeIn pt_vct_funs Calculate bounce down
pt_vct_bounce_down <- function(x, y) {

  if (validate_xy(x, y)$value == FALSE) { stop(validate_xy(x, y)$message) }

  errors <- pt_vct_errors(x, y)
  10^(min(errors))
}

#' @export
#' @describeIn pt_vct_funs Calculate bounce total
pt_vct_bounce_total <- function(x, y) {

  if (validate_xy(x, y)$value == FALSE) { stop(validate_xy(x, y)$message) }

  pt_vct_bounce_up(x, y) * (1 / pt_vct_bounce_down(x, y))
}
