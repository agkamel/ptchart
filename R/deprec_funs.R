calculate_accuracy <- function(day, log10_acc_ratio_raw, raw = TRUE) {

  b1 <- calculate_b1(day, log10_acc_ratio_raw)
  a_raw <- antilog(b1)^7
  a_val <- convert_value(a_raw)

  if (raw == TRUE) {
    return(a_raw)
  } else {
    return(a_val)
  }
}



calculate_accuracy_ratio <- function(freq, freq_err, raw = TRUE) {

  ar_raw <- freq / freq_err
  ar_val <- convert_value(ar_raw)

  if (raw == TRUE) {
    return(ar_raw)
  } else {
    return(ar_val)
  }
}



calculate_b0 <- function(day, log10_freq, antilog = FALSE) {
  # stopifnot("`day` must be integer" = is.integer(day))
  # stopifnot("`log_10_freq` must be numeric" = is.numeric(log10_freq))
  stopifnot("`antilog` must be a logical `TRUE` or `FALSE`" = is.logical(antilog) & antilog %in% c(TRUE, FALSE))

  mean_day <- mean(day)
  mean_log10_freq <- mean(log10_freq)
  b1 <- calculate_b1(day, log10_freq)

  if (antilog == FALSE) {
    mean_log10_freq - (b1 * mean_day)
  } else {
    antilog(mean_log10_freq - (b1 * mean_day))
  }
}



calculate_b1 <- function(day, log10_freq, antilog = FALSE) {
  # stopifnot("`day` must be integer" = is.integer(day))
  # stopifnot("`log_10_freq` must be numeric" = is.numeric(log10_freq))
  stopifnot("`antilog` must be a logical `TRUE` or `FALSE`" = is.logical(antilog) & antilog %in% c(TRUE, FALSE))

  mean_day <- mean(day)
  mean_log10_freq <- mean(log10_freq)

  day_deviation_from_mean <- day - mean_day
  day_deviation_from_mean_squared <- day_deviation_from_mean^2

  log10_freq_deviation_from_mean <- log10_freq - mean_log10_freq

  if (antilog == FALSE) {
    sum(day_deviation_from_mean * log10_freq_deviation_from_mean) / sum(day_deviation_from_mean_squared)
  } else {
    antilog(sum(day_deviation_from_mean * log10_freq_deviation_from_mean) / sum(day_deviation_from_mean_squared))
  }

}



calculate_bounce <- function(day, log10_freq, type, raw = TRUE) {
  # stopifnot("`day` must be integer" = is.integer(day))
  # stopifnot("`log_10_freq` must be numeric" = is.numeric(log10_freq))

  errors <- calculate_errors(day, log10_freq)

  type <- match.arg(type, choices = c("up", "down"))
  value <- switch(type,
                  up = antilog(max(errors)),
                  down = antilog(min(errors))
  )

  if (raw == TRUE) {
    value
  } else {
    convert_value(value)
  }
}


calculate_bounce_up <- function(day, log10_freq, raw = TRUE) {
  calculate_bounce(day = day, log10_freq = log10_freq, type = "up", raw = raw)
}


calculate_bounce_down <- function(day, log10_freq, raw = TRUE) {
  calculate_bounce(day = day, log10_freq = log10_freq, type = "down", raw = raw)
}


calculate_bounce_total <- function(day, log10_freq) {
  bounce_up <- calculate_bounce_up(day, log10_freq, raw = FALSE)
  bounce_down <- calculate_bounce_down(day, log10_freq, raw = FALSE)
  bounce_up * bounce_down
}




calculate_celeration <- function(day, log10_freq, raw = TRUE) {
  # stopifnot("`day` must be integer" = is.integer(day))
  # stopifnot("`log_10_freq` must be numeric" = is.numeric(log10_freq))

  b1 <- calculate_b1(day, log10_freq)
  c_raw <- antilog(b1)^7
  c_val <- convert_value(c_raw)

  if (raw == TRUE) {
    return(c_raw)
  } else {
    return(c_val)
  }
}



calculate_count_ceil <- function(count_ceil) {
  if (!is.numeric(count_ceil)) {
    stop("`count_ceil` must be numeric.")
  }

  for (i in count_ceil) {
    if (is.na(i)) {
      next
    } else if (i <= 0) {
      stop("`count_ceil` must be greater that 0.")
    }
  }

  1 / count_ceil
}



calculate_count_floor <- function(count_floor) {
  if (!is.numeric(count_floor)) {
    stop("`count_floor` must be numeric.")
  }

  for (i in count_floor) {
    if (is.na(i)) {
      next
    } else if (i <= 0) {
      stop("`count_floor` must be greater that 0.")
    }
  }

  1 / count_floor
}



calculate_day <- function(date, date_zero) {
  as.integer(date - date_zero)
}



calculate_errors <- function(day, log10_freq, antilog = FALSE, squared = FALSE){
  # stopifnot("`day` must be integer" = is.integer(day))
  # stopifnot("`log_10_freq` must be numeric" = is.numeric(log10_freq))
  stopifnot("`antilog` must be a logical `TRUE` or `FALSE`" = is.logical(antilog) & antilog %in% c(TRUE, FALSE))

  predicted_values <- calculate_predicted_values(day, log10_freq)

  if (antilog == FALSE){
    errors <- log10_freq - predicted_values
  } else {
    errors <- antilog(log10_freq - predicted_values)
  }

  if (squared == FALSE){
    errors
  } else {
    errors^2
  }
}



calculate_freq <- function(count, time
                           #, rule = log10(2)
) {
  count / time

  #dplyr::case_when(
  #  count == 0 ~ freq_if_zero_count(time, rule = rule),
  #  TRUE       ~ count / time
  #)
}

calculate_count <- function(freq, time) {
  freq * time
}


recode_freq_when_zero <- function(freq, time, rule = log10(2)) {
  dplyr::case_when(
    freq < calculate_time_floor(time) ~ antilog(log10(calculate_time_floor(time)) - rule),
    TRUE                              ~ freq
  )
}



# log10(2) est utilisé selon la recommandation de l'article de Neely
freq_if_zero_count <- function(time, rule = log10(2)){
  antilog(log10(calculate_time_floor(time)) - rule)
}



calculate_predicted_values <- function(day, log10_freq, antilog = FALSE){
  # stopifnot("`day` must be integer" = is.integer(day))
  # stopifnot("`log_10_freq` must be numeric" = is.numeric(log10_freq))
  stopifnot("`antilog` must be a logical `TRUE` or `FALSE`" = is.logical(antilog) & antilog %in% c(TRUE, FALSE))

  b0 <- calculate_b0(day, log10_freq)
  b1 <- calculate_b1(day, log10_freq)

  if (antilog == FALSE) {
    b0 + b1 * day
  } else {
    antilog(b0 + b1 * day)
  }
}



calculate_time_floor <- function(time) {
  if (!is.numeric(time)) {
    stop("`time` must be numeric.")
  }

  for (i in time) {
    if (is.na(i)) {
      next
    } else if (i <= 0) {
      stop("`time` must be greater that 0.")
    }
  }

  1 / time
}



# ----------------------------------------------------------------



convert_value <- function(x) {

  if (!is.numeric(x)) {
    stop("`x` must be numeric")
  }

  for (i in x) {
    if (is.na(i)) {
      next
    } else if (i <= 0) {
      stop("Values in `x` must be greater than 0.")
    }
  }

  antilog(abs(log10(x)))
}



convert_value_with_ptsign <- function(x) {

  raw <- x
  val <- convert_value(x)

  if (raw == val) {
    return(paste0("\u00d7", val))
  } else {
    return(paste0("\u00f7", val))
  }
}
