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
#' The residual errors \eqn{\epsilon} obtained with `res()` is calculated with
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
#' df <- ptdata01
#' celeration(df$jour, df$t_frequency)
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

  res <- res(x, y)
  10^(max(res))
}

#' @export
#' @describeIn summary_funs Calculate bounce down
bounce_down <- function(x, y) {

  if (validate_xy(x, y)$value == FALSE) { stop(validate_xy(x, y)$message) }

  res <- res(x, y)
  10^(min(res))
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
#' @examples
#' df <- ptdata01
#' accuracy_ratio(df$jour, df$t_frequency, df$nt_frequency)
#'
#' @describeIn vct_funs Calculate accuracy ratio
accuracy_ratio <- function(x, y_cor, y_incor) {

  if (validate_xy(x, y_cor)$value == FALSE) { stop(validate_xy(x, y_cor)$message) }
  if (validate_xy(x, y_incor)$value == FALSE) { stop(validate_xy(x, y_incor)$message) }

  index_to_keep <- (!is.na(x) | !is.na(y_cor) | !is.na(y_incor))
  #index_to_keep <- (!is.na(x) & !is.na(y_cor) & !is.na(y_incor)) # To be checked

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
#' @describeIn vct_funs Calculate residual errors
res <- function(x, y) {

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










# This section are functions in work - Not exported yet

record_floor <- function(time, type = "minute") {

  if (!(type %in% c("minute", "second", "hour"))) {
    cli::cli_abort("Type must be one of `minute`, `second` or `hour`.")
  }

  output <- dplyr::case_when(
    type == "minute" ~ 1 / time,
    type == "second" ~ 1 / (time / 60),
    type == "hour" ~ 1 / (time * 60)
  )

  index_to_check <- !is.na(output)

  if (any(output[index_to_check] < (1 / (24*60))) || any(output[index_to_check] > 1000)) {
    cli::cli_abort("Some output values in time floor are lower than 1 response per day or greater than 1000 response per minute. Check time input values.")
  }

  output
}


record_floor_to_time <- function(record_floor) {
  1 / record_floor
}




date_to_day <- function(date, date_zero) {
  if (is_sunday(date_zero) == FALSE) {
    current_wday <- lubridate::wday(date_zero, week_start = 1, label = TRUE, abbr = FALSE)
    stop(paste0("`date_zero` must be a sunday. This date, ", as.character(date_zero), ", is a: ", as.character(current_wday)))
  }
  as.integer(date - date_zero)
}


first_sunday <- function(date){
  seven_previous_dates <- min(date) - lubridate::days(0:6)
  seven_previous_dates[is_sunday(seven_previous_dates)]
}

is_sunday <- function(date) {
  lubridate::wday(date, week_start = 1) == 7
}



day_to_date <- function(day, date_zero) {
  date_zero + day
}

count_to_freq <- function(count, time) {
  count / time
}

freq_to_count <- function(freq, time) {
  freq * time
}

count_and_freq_to_time <- function(count, freq) {
  count / freq
}



recode_zero_freq <- function(y, time, method = "div2") {

  record_floor <- record_floor(time)

  if (!(method %in% c("div2", "remove"))) {
    cli::cli_abort("`method` must have one of the following: 'div2', 'remove'")
  }

  if (method == "div2") {
    output <- dplyr::case_when(
      y == 0 ~ record_floor / 2,
      .default = y
    )
  } else if (method == "remove") {
    output <- dplyr::case_when(
      y == 0 ~ NA_real_,
      .default = y
    )
  }

  output

}


