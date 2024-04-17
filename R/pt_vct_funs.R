#' Functions for calculating pt values for two vectors
#'
#' @param x Day of calendar
#' @param y Frequency
#' @param y_cor Correct frequency
#' @param y_incor Incorrect frequency
#'
#' @return A single value or a vector
#'
#' @examples
#' # NOT YET
#'
#' @export
#' @describeIn pt_vct_funs Calculate b1
pt_vct_b1 <- function(x, y) {
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

  b1 <- pt_vct_b1(x, y)
  (10^b1)^7

}

#' @export
#' @describeIn pt_vct_funs Calculate accuracy ratio
pt_vct_accuracy_ratio <- function(x, y_cor, y_incor) {
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
  errors <- pt_vct_errors(x, y)
  10^(max(errors))
}

#' @export
#' @describeIn pt_vct_funs Calculate bounce down
pt_vct_bounce_down <- function(x, y) {
  errors <- pt_vct_errors(x, y)
  10^(min(errors))
}

#' @export
#' @describeIn pt_vct_funs Calculate bounce total
pt_vct_bounce_total <- function(x, y) {
  pt_vct_bounce_up(x, y) * pt_vct_bounce_down(x, y)
}
