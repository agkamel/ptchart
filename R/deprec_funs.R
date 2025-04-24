# Old ptstat() --------------------------------------------------------------------

#' Computing measures of behavioral change.
#'
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function was deprecated and renamed from `ptstat()` to `ptstat_old()`
#' because the code was hard to read and maintain.
#' Use the new `ptchart()` instead.
#'
#' `ptstat_old()` is the main function for computing measures of behavioral change.
#'
#'
#' @details
#' `ptstat_old()` returns a list that contains all calculations.
#' More specifically:
#' - `arg_table` is a data frame that contains the call values of the function.
#' - `pttable` is a data frame that contains all basic calculations like frequencies, logged frequencies, accuracy ratios, etc. This table can be extracted with `pttable()`.
#' - `pttables` is a list of data frames with same columns of `pttables` except they are splitted by phase.
#' - `desc` contains descriptives analysis.
#' - `terms` contains terms of a regression model.
#' - `a_table` contains accuracy values (or Accuracy Improvement Measure). This table can be extracted with `accuracy()`.
#' - `c_table` contains celeration values. This table can be extracted with `celeration()`.
#' - `b_table` contains bounce values. This table can be extracted with `bounce()`, `bounce_up()`, `bounce_down()` or `bounce_total()`.
#' - `j_table` contains jump values. This table can be extracted with `jump()`.
#' - `t_table` contains turn values. This table can be extracted with `turn()`.
#'
#'
#' @param data Dataframe.
#' @param day Numeric. No of the successive day of the calendar. Values must be integers and >= 0.
#' @param freq Numeric. Frequencies of observations aimed to be accelerated. Values must be >= 0.
#' @param freq_err Numeric. Frequencies of observations aimed to be decelerated. Values must be >= 0.
#' @param phase Charactor. Phase of intervention. If none are provided, default phase is `"A"` for all observations. Note: Providing factor vector to be implemented.
#' @param date Date. Dates of observations in the format "yyyy-mm-dd". If `date_zero` is not provided, the closest sunday before the first date will be used for `date_zero`.
#' @param date_zero Date of length one. A date that correspond to day 0 in the format "yyyy-mm-dd".
#' @param count Integer. Count of responses aimed to be accelerated. Values must be >= 0.
#' @param count_err Integer. Count of responses aimed to be decelerated. Values must be >= 0.
#' @param time Numeric. Number of minutes. Values must be > 0.
#' @param count_floor Double. NOTE. Name may be changed...
#' @param count_ceil Double. NOTE. Name may be changed...
#' @param verbose Logical. Show informational messages. Default to `TRUE`.
#'
#' @keywords internal
#' @return `ptstat_old()` returns an S3 object of class `ptstat`, a list of computation tables.
#' @rdname deprec_ptstat_old
#' @export
#'
#' @examples
#' ptstat_old(example_pt_data,
#'        day = "jour",
#'        count = "reponse",
#'        time = "minute",
#'        phase = "phase"
#' )
ptstat_old <- function(data,
                       day = NULL,
                       freq = NULL,
                       phase = NULL,
                       date = NULL,
                       date_zero = NULL,
                       count = NULL,
                       time = NULL,
                       count_err = NULL,
                       freq_err = NULL,
                       count_floor = NULL,
                       count_ceil = NULL,
                       verbose = TRUE
) {

  lifecycle::deprecate_warn("0.1.0.9000", "ptstat_old()", "ptstat()")

  # Validate input
  validate_arg_data(data)
  if (!is.null(day)) validate_arg_day(data, day)
  if (!is.null(freq)) validate_arg_freq(data, freq)
  if (!is.null(phase)) validate_arg_phase(data, phase)
  if (!is.null(date)) validate_arg_date(data, date)
  if (!is.null(date_zero)) validate_arg_date_zero(date_zero)
  if (!is.null(count)) validate_arg_count(data, count)
  if (!is.null(time)) validate_arg_time(data, time)
  if (!is.null(count_err)) validate_arg_count_err(data, count_err)
  if (!is.null(freq_err)) validate_arg_freq_err(data, freq_err)
  if (!is.null(count_floor)) stopifnot("`count_floor` must be numeric" = is.numeric(data[[count_floor]]))
  if (!is.null(count_ceil)) stopifnot("`count_ceil` must be numeric" = is.numeric(data[[count_ceil]]))
  stopifnot("`verbose` must be logical" = is.logical(verbose))

  # Table of supplied argument
  arg_table <- tibble::tibble(
    day = if (is.null(day)) NA else day,
    freq = if (is.null(freq)) NA else freq,
    phase = if (is.null(phase)) NA else phase,
    date = if (is.null(date)) NA else date,
    date_zero = if (is.null(date_zero)) NA else date_zero,
    count = if (is.null(count)) NA else count,
    time = if (is.null(time)) NA else time,
    count_err = if (is.null(count_err)) NA else count_err,
    freq_err = if (is.null(freq_err)) NA else freq_err,
    count_floor = if (is.null(count_floor)) NA else count_floor,
    count_ceil = if (is.null(count_ceil)) NA else count_ceil)


  # Conditions for supplying day, date and date_zero
  if (!is.null(date) & !is.null(date_zero)){
    data[["day"]] <- calculate_day(data[[date]], date_zero)
    day <- "day"
    if (verbose == TRUE) message("i `date` and `date_zero` were used to calculate `day`")
  } else if (!is.null(date) & is.null(date_zero)) {
    date_zero <- first_sunday(data[[date]])
    data[["day"]] <- calculate_day(data[[date]], date_zero)
    day <- "day"
    if (verbose == TRUE) message("i `date` was used to calculate `date_zero`")
    if (verbose == TRUE) message("i `date` and calculated `date_zero` were used to calculate `day`")
  } else if (!is.null(day)) {
    if (verbose == TRUE) message("i `day` was used to calculate `day`")
  } else {
    stop("! One of these combinaisons must be supplied:\n    `day`\n    `date`\n    `date` and `date_zero`")
  }


  # Conditions for supplying time
  if (is.null(time)) {
    stop("! `time` must be supplied.")
  }

  # Condition for supplying freq, count, freq_err and count_err
  if ( is.null(count_err) && is.null(count) && is.null(freq) && is.null(freq_err) ) {
    stop("! One of these combinaisons must be supplied:\n    `freq`\n    `count`\n    `freq_err`\n    `count_err`")
  }

  # Conditions for supplying freq, count and time
  if (!is.null(count)) {
    data[["freq"]] <- calculate_freq(data[[count]], data[[time]])
    data[["freq"]] <- recode_freq_when_zero(data[["freq"]], data[[time]])
    freq <- "freq"
    if (verbose == TRUE) message("i `count` and `time` were used to calculate `freq`")
  } else if (!is.null(freq)) {
    data[[freq]] <- recode_freq_when_zero(data[[freq]], data[[time]])
    data[["count"]] <- calculate_count(data[[freq]], data[[time]])
    count <- "count"
    if (verbose == TRUE) message("i `freq` and `time` were used to calculate `count`")
  } #else {
  #stop("! One of these combinaisons must be supplied:\n    `freq`\n    `count` and `time`")
  #}


  # Conditions for supplying freq_err, count_err and time
  if (!is.null(count_err)) {
    data[["freq_err"]] <- calculate_freq(data[[count_err]], data[[time]])
    data[["freq_err"]] <- recode_freq_when_zero(data[[count_err]], data[[time]])
    freq_err <- "freq_err"
    if (verbose == TRUE) message("i `count_err` and `time` were used to calculate `freq_err`")
  } else if (!is.null(freq_err)) {
    data[[freq_err]] <- recode_freq_when_zero(data[[freq_err]], data[[time]])
    data[["count_err"]] <- calculate_count(data[[freq_err]], data[[time]])
    count_err <- "count_err"
    if (verbose == TRUE) message("i `freq_err` and `time` were used to calculate `count_err`")
  } #else {
  #stop("! One of these combinaisons must be supplied:\n    `freq_err`\n    `count_err` and `time`")
  #}


  # Condition if not supplying phase
  if (is.null(phase)) {
    data[["phase"]] <- rep("A", nrow(data))
    phase <- "phase"
    if (verbose == TRUE) message("i `phase` not supplied. Default phase set to `A` for all observation")
  }


  # Splitting data by phase
  splitted_data <- split(data, data[[phase]])
  unique_phase <- unique(data[[phase]])
  phase_n <- length(unique_phase)

  # Initialize vectors
  pttables <- vector(mode = "list", length = length(unique_phase))
  names(pttables) <- unique_phase

  n_values <- vector(mode = "integer", length = phase_n)
  day_mean_values <- vector(mode = "double", length = phase_n)
  log10_freq_mean_values <- vector(mode = "double", length = phase_n)
  log10_freq_err_mean_values <- vector(mode = "double", length = phase_n)

  b1_values <- vector(mode = "double", length = phase_n)
  b0_values <- vector(mode = "double", length = phase_n)

  b1_values_err <- vector(mode = "double", length = phase_n)
  b0_values_err <- vector(mode = "double", length = phase_n)

  acc_raw <- vector(mode = "double", length = phase_n)
  acc_values <- vector(mode = "double", length = phase_n)

  cel_raw <- vector(mode = "double", length = phase_n)
  cel_values <- vector(mode = "double", length = phase_n)

  cel_raw_err <- vector(mode = "double", length = phase_n)
  cel_values_err <- vector(mode = "double", length = phase_n)

  bounce_up_raw <- vector(mode = "double", length = phase_n)
  bounce_up_values <- vector(mode = "double", length = phase_n)
  bounce_down_raw <- vector(mode = "double", length = phase_n)
  bounce_down_values <- vector(mode = "double", length = phase_n)
  bounce_total_values <- vector(mode = "double", length = phase_n)

  bounce_up_raw_err <- vector(mode = "double", length = phase_n)
  bounce_up_values_err <- vector(mode = "double", length = phase_n)
  bounce_down_raw_err <- vector(mode = "double", length = phase_n)
  bounce_down_values_err <- vector(mode = "double", length = phase_n)
  bounce_total_values_err <- vector(mode = "double", length = phase_n)

  # Calculate main values
  for (i in seq_along(splitted_data)) {

    # Variable by phase - These ones must be calculated
    iphase <- splitted_data[[i]][[phase]]
    iday <- splitted_data[[i]][[day]]
    ilog10_freq <- if (!is.null(freq)) log10(splitted_data[[i]][[freq]]) else rep(NA_real_, nrow(splitted_data[[i]]))
    ifreq <- if (!is.null(freq)) splitted_data[[i]][[freq]] else rep(NA_real_, nrow(splitted_data[[i]]))

    idate <- if (!is.null(date)) splitted_data[[i]][[date]] else rep(NA_real_, nrow(splitted_data[[i]]))
    icount <- if (!is.null(count)) splitted_data[[i]][[count]] else rep(NA_real_, nrow(splitted_data[[i]]))
    itime <- if (!is.null(time)) splitted_data[[i]][[time]] else rep(NA_integer_, nrow(splitted_data[[i]]))
    icount_err <- if (!is.null(count_err)) splitted_data[[i]][[count_err]] else rep(NA_integer_, nrow(splitted_data[[i]]))
    ifreq_err <- if (!is.null(freq_err)) splitted_data[[i]][[freq_err]] else rep(NA_real_, nrow(splitted_data[[i]]))
    ilog10_freq_err <- if (!is.null(freq_err)) log10(splitted_data[[i]][[freq_err]]) else rep(NA_real_, nrow(splitted_data[[i]]))
    icount_floor <- if (!is.null(count_floor)) splitted_data[[i]][[count_floor]] else rep(NA_real_, nrow(splitted_data[[i]]))
    icount_ceil <- if (!is.null(count_ceil)) splitted_data[[i]][[count_ceil]] else rep(NA_real_, nrow(splitted_data[[i]]))

    # pttables
    time_floor_values <- calculate_time_floor(itime)
    count_floor_values <- calculate_count_floor(icount_floor)
    count_ceil_values <- calculate_count_ceil(icount_ceil)

    accuracy_ratio_values_raw <- calculate_accuracy_ratio(ifreq, ifreq_err)
    log10_accuracy_ratio_values_raw <- log10(accuracy_ratio_values_raw)
    accuracy_ratio_values <- calculate_accuracy_ratio(ifreq, ifreq_err, raw = FALSE)

    predicted_values <- calculate_predicted_values(iday, ilog10_freq)
    errors <- calculate_errors(iday, ilog10_freq)

    predicted_values_err <- calculate_predicted_values(iday, ilog10_freq_err)
    errors_err <- calculate_errors(iday, ilog10_freq_err)

    # desc_table
    n_values[i] <- nrow(splitted_data[[i]])
    day_mean_values[i] <- mean(iday)
    log10_freq_mean_values[i] <- mean(ilog10_freq)
    log10_freq_err_mean_values[i] <- mean(ilog10_freq_err)

    # terms_table
    b1_values[i] <- calculate_b1(iday, ilog10_freq)
    b0_values[i] <- calculate_b0(iday, ilog10_freq)

    b1_values_err[i] <- calculate_b1(iday, ilog10_freq_err)
    b0_values_err[i] <- calculate_b0(iday, ilog10_freq_err)

    # a_table
    acc_raw[i] <- calculate_accuracy(iday, log10_accuracy_ratio_values_raw)
    acc_values[i] <- calculate_accuracy(iday, log10_accuracy_ratio_values_raw, raw = FALSE)

    # c_table
    cel_raw[i] <- calculate_celeration(iday, ilog10_freq)
    cel_values[i] <- calculate_celeration(iday, ilog10_freq, raw = FALSE)

    cel_raw_err[i] <- calculate_celeration(iday, ilog10_freq_err)
    cel_values_err[i] <- calculate_celeration(iday, ilog10_freq_err, raw = FALSE)

    # b_table
    bounce_up_raw[i] <- calculate_bounce_up(iday, ilog10_freq)
    bounce_up_values[i] <- calculate_bounce_up(iday, ilog10_freq, raw = FALSE)
    bounce_down_raw[i] <- calculate_bounce_down(iday, ilog10_freq)
    bounce_down_values[i] <- calculate_bounce_down(iday, ilog10_freq, raw = FALSE)
    bounce_total_values[i] <- calculate_bounce_total(iday, ilog10_freq)

    bounce_up_raw_err[i] <- calculate_bounce_up(iday, ilog10_freq_err)
    bounce_up_values_err[i] <- calculate_bounce_up(iday, ilog10_freq_err, raw = FALSE)
    bounce_down_raw_err[i] <- calculate_bounce_down(iday, ilog10_freq_err)
    bounce_down_values_err[i] <- calculate_bounce_down(iday, ilog10_freq_err, raw = FALSE)
    bounce_total_values_err[i] <- calculate_bounce_total(iday, ilog10_freq_err)

    # pttables by phase
    pttables[[i]] <- tibble::tibble(day = iday,
                                    date = idate,
                                    count = icount,
                                    count_err = icount_err,
                                    time = itime,
                                    freq = ifreq,
                                    freq_err = ifreq_err,
                                    log10_freq = ilog10_freq,
                                    phase = iphase,
                                    acc_ratio_raw = accuracy_ratio_values_raw,
                                    log10_acc_ratio_raw = log10_accuracy_ratio_values_raw,
                                    acc_ratio = accuracy_ratio_values,
                                    time_floor = time_floor_values,
                                    count_floor = count_floor_values,
                                    count_ceil = count_ceil_values,
                                    pred = predicted_values,
                                    errors = errors,
                                    pred_err = predicted_values_err,
                                    errors_err = errors_err)
  }

  # Make tables
  pttable <- tibble::tibble(do.call("rbind", pttables))

  desc_table <- tibble::tibble(phase = unique_phase,
                               n = n_values,
                               day_mean = day_mean_values,
                               log10_freq_mean = log10_freq_mean_values,
                               log10_freq_err_mean = log10_freq_err_mean_values)

  terms_table <- tibble::tibble(phase = unique_phase,
                                b0 = b0_values,
                                b1 = b1_values,
                                b0_err = b0_values_err,
                                b1_err = b1_values_err)

  a_table <- tibble::tibble(phase = unique_phase,
                            a_raw = acc_raw,
                            a = acc_values
  )

  c_table <- tibble::tibble(phase = unique_phase,
                            c_raw = cel_raw,
                            c = cel_values,
                            c_raw_err = cel_raw_err,
                            c_err = cel_values_err)

  b_table <- tibble::tibble(phase = unique_phase,
                            b_up_raw = bounce_up_raw,
                            b_up = bounce_up_values,
                            b_down_raw = bounce_down_raw,
                            b_down = bounce_down_values,
                            b_total = bounce_total_values,

                            b_up_raw_err = bounce_up_raw_err,
                            b_up_err = bounce_up_values_err,
                            b_down_raw_err = bounce_down_raw_err,
                            b_down_err = bounce_down_values_err,
                            b_total_err = bounce_total_values_err)










  # Initialize jump and turn vectors
  jump_raw <- vector(mode = "double", length = phase_n - 1)
  jump_values <- vector(mode = "double", length = phase_n - 1)
  jump_phase_from <- vector(mode = "character", phase_n - 1)
  jump_phase_to <- vector(mode = "character", phase_n - 1)

  turn_raw <- vector(mode = "double", length = phase_n - 1)
  turn_values <- vector(mode = "double", length = phase_n - 1)
  turn_phase_from <- vector(mode = "character", length = phase_n - 1)
  turn_phase_to <- vector(mode = "character", length = phase_n - 1)


  # Calculate jumps
  if (phase_n > 1) {
    for (i in seq_along(phase_n - 1)) {
      a <- pttables[[i]]
      b <- pttables[[i+1]]
      jump_raw[i] <- antilog(b$pred[1] - a$pred[length(a$pred)])
      jump_values[i] <- convert_value(antilog(b$pred[1] - a$pred[length(a$pred)]))

      jump_phase_from[i] <- unique_phase[i]
      jump_phase_to[i] <- unique_phase[i+1]
    }

    j_table <- tibble::tibble(from = jump_phase_from,
                              to = jump_phase_to,
                              j_raw = jump_raw,
                              j = jump_values)
  } else {
    j_table <- tibble::tibble(from = NA_character_,
                              to = NA_character_,
                              j_raw = NA_real_,
                              j = NA_real_)
  }


  # Calculate turn
  if (phase_n > 1) {
    for (i in seq_along(phase_n - 1)) {
      a_b1 <- b1_values[i]
      b_b1 <- b1_values[i+1]
      turn_raw[i] <- antilog((b_b1 * 7) - (a_b1 * 7))
      turn_values[i] <- convert_value(antilog((b_b1 * 7) - (a_b1 * 7)))

      turn_phase_from[i] <- unique_phase[i]
      turn_phase_to[i] <- unique_phase[i+1]
    }

    t_table <- tibble::tibble(from = turn_phase_from,
                              to = turn_phase_to,
                              t_raw = turn_raw,
                              t = turn_values)
  } else {
    t_table <- tibble::tibble(from = NA_character_,
                              to = NA_character_,
                              t_raw = NA_real_,
                              t = NA_real_)
  }








  # Return list
  ptstat_list <- list(arg_table = arg_table,
                      pttable = pttable,
                      pttables = pttables,
                      desc = desc_table,
                      terms = terms_table,
                      a_table = a_table,
                      c_table = c_table,
                      b_table = b_table,
                      j_table = j_table,
                      t_table = t_table)

  validate_ptstat(new_ptstat(ptstat_list))
}



# Internal function for old ptstat() ------------------------------------------



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



# Other functions -------------------------------------------------------------



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



#' Compute antilogarithm
#'
#' @title Antilogarithm
#' @description
#' `r lifecycle::badge("deprecated")`
#' `antilog()` returns the antilogarithm of a value.
#'
#' @param x Numeric. Value to which base is augmented.
#' @param base Numeric. Value to be augmented by x.
#'
#' @return Numeric.
#' @export
#'
#' @examples antilog(0.25)
antilog <- function(x, base = 10) {
  stopifnot("x must be numeric" = is.numeric(x))
  stopifnot("base must be numeric" = is.numeric(base))

  values <- base^x

  if (sum(is.nan(values)) >= 1) {
    stop("`antilog` returns some 'not a number' values (NaN)")
  } else if (sum(is.infinite(values)) >= 1) {
    stop("`antilog` returns some infinite (Inf) values")
  }

  values

}






# Old ptchart() ---------------------------------------------------------------



#' ptchart
#'
#' @param object An object of class `ptstat`.
#' @param zoom_x A vector of class `Date` and of length 2 for setting limits for the x axis.
#' @param zoom_y A numerical vector of length 2 for setting limits for the y axes.
#' @param title A character vector of length 1 for setting the title.
#'
#' @return A precision teaching chart.
#' @export
#'
#' @examples
#' #TODO
ptchart_old <- function(object,
                    zoom_x = NULL,
                    zoom_y = NULL,
                    title = "ptchart output"
) {

  lifecycle::deprecate_warn("0.1.0.9000", "ptchart_old()", "ptchart()")

  # To prevent note of "no visible binding for global variable 'x'" when building the package
  day <- freq <- freq_err <- phase <- time_floor <- count_ceil <- count_floor <- NULL


  stopifnot("`object` must be of class `ptstat`" = is_ptstat(object))

  scale_x_params <- make_scale_x_params_old(object)
  scale_y_params <- make_scale_y_params_old()

  output <- ggplot2::ggplot(
    data = extract_pttable(object)
  ) +


    ggplot2::scale_y_log10(
      name = "Nombre de comportement par minute",
      breaks = scale_y_params[["frequency"]][(scale_y_params[["is_y_breaks"]])],
      labels = scale_y_params[["frequency"]][(scale_y_params[["is_y_breaks"]])],
      minor_breaks = scale_y_params[["frequency"]],
      limits = c(1/(24*60), 1000),
      sec.axis = ggplot2::sec_axis(
        trans = ~.,
        breaks = c(.001, .002, .005, .01, .02, .05, .1, .2, .5, 1, 2, 3, 4, 6),
        labels = c("1000'", "500'", "200'", "100'", "50'", "20'", "10'", "5'",
                   "2'", "1'", "30\"", "20\"", "15\"", "10\""),
        name = ""
      )
    ) +

    ggplot2::scale_x_continuous(
      name = "Jour cons\u00e9cutif du calendrier",
      breaks = scale_x_params[["no_date"]][(scale_x_params[["breaks"]])],
      labels = scale_x_params[["labels"]][!is.na(scale_x_params[["labels"]])],
      minor_breaks = scale_x_params[["no_date"]],
      limits = c(scale_x_params[["no_date"]][[1]],
                 scale_x_params[["no_date"]][[length(scale_x_params[["no_date"]])]])
    ) +

    ggplot2::ggtitle(title) +

    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, size = 20),
      panel.grid.major = ggplot2::element_line(colour = "#00b1d9"),
      panel.grid.minor = ggplot2::element_line(colour = "#66d1e8"),
      text = ggplot2::element_text(family = "serif", size = 12)
      #aspect.ratio = 5.44 / 8, # 5 7/16 de pouce par 8 pouce
      #https://jweshleman.wordpress.com/2006/03/25/og-on-standard-celeration-charting-system-standards/
    ) +

    ggplot2::coord_fixed(
      expand = FALSE,
      #ratio = 1/1, # Voir note sur le ratio ci-bas, explication 1
      #ratio = 7/1, # Voir note sur le ratio ci-bas, explication 2
      #ratio = 7/log10(2), # 0.30103 voir note sur le ratio ci-bas, explication 3
      ratio = 7/(log10(2)/tan(34*pi/180)), # 0.30105 / 0.6745 # Voir note sur le ratio ci-bas, explication 4

      # ZOOM sur le graphique
      xlim = zoom_x,
      ylim = zoom_y
    ) +

    # Ligne séparant les phases
    ggplot2::geom_vline(
      xintercept = lubridate::ymd("2021-08-01")+0.5
    )

  if (is.na(object$arg_table$date)) {

    output +

      # Time floor
      ggplot2::geom_point(
        mapping = ggplot2::aes(x = day, y = time_floor),
        shape = "\u2013", size = 5, color = "gray30"
      ) +

      # Count floor
      ggplot2::geom_point(
        mapping = ggplot2::aes(x = day, y = count_floor),
        shape = "\u2012", size = 5, color = "gray20"
      ) +

      # Count ceil
      ggplot2::geom_point(
        mapping = ggplot2::aes(x = day, y = count_ceil),
        shape = "\u2013", size = 5, color = "gray10"
      ) +






      #Pente de régression
      ggplot2::geom_smooth(
        method = "lm",
        se = FALSE,
        mapping = ggplot2::aes(x = day, y = freq, group = phase)
      ) +

      ggplot2::geom_smooth(
        method = "lm",
        se = FALSE,
        mapping = ggplot2::aes(x = day, y = freq_err, group = phase), color = "red"
      ) +

      #Point de fréquence cible et non-cible
      ggplot2::geom_point(
        mapping = ggplot2::aes(x = day, y = freq),
        shape = 16#, size = 2
      ) +

      ggplot2::geom_point(
        mapping = ggplot2::aes(x = day, y = freq_err),
        shape = 4, size = 2.5,
      )



  } else {


    output +

      # Plancher d'enregistrement
      #ggplot2::geom_segment(x = ymd("2021-07-20")-0.5,
      #             xend = ymd("2021-07-20")+0.5,
      #             y = log10(1),
      #             yend = log10(1)
      #             ) +

      # Time floor
      ggplot2::geom_point(
        mapping = ggplot2::aes(x = date, y = time_floor),
        shape = "\u2013", size = 5, color = "gray30"
      ) +

      # Count floor
      ggplot2::geom_point(
        mapping = ggplot2::aes(x = date, y = count_floor),
        shape = "\u2012", size = 5, color = "gray20"
      ) +

      # Count ceil
      ggplot2::geom_point(
        mapping = ggplot2::aes(x = date, y = count_ceil),
        shape = "\u2013", size = 5, color = "gray10"
      ) +






      #Pente de régression
      ggplot2::geom_smooth(
        method = "lm",
        se = FALSE,
        mapping = ggplot2::aes(x = date, y = freq, group = phase)
      ) +

      ggplot2::geom_smooth(
        method = "lm",
        se = FALSE,
        mapping = ggplot2::aes(x = date, y = freq_err, group = phase), color = "red"
      ) +

      #Point de fréquence cible et non-cible
      ggplot2::geom_point(
        mapping = ggplot2::aes(x = date, y = freq),
        shape = 16#, size = 2
      ) +

      ggplot2::geom_point(
        mapping = ggplot2::aes(x = date, y = freq_err),
        shape = 4, size = 2.5,
      )# +




    # Annotation
    #  ggplot2::annotate("text",
    #                    x = lubridate::ymd("2021-07-24"),
    #                    y = 50, label = paste0("\u00d7", round(celeration(object)$c[1], 2)))




    #geom_abline(slope = object[["terms"]][["b1"]][[1]],
    #           intercept = object[["terms"]][["b0"]][[1]])

  }
}


# Functions for old ptchart() -------------------------------------------------

make_scale_y_params_old <- function() {
  # To prevent note of "no visible binding for global variable 'x'" when building the package
  base <- exponent <- sub_unit <- NULL

  tibble::tibble(
    base = 10,
    exponent = c(-4, -4, -4, -4, rep(c(-3, -2, -1, 0, 1, 2), each = 9), 3),
    base_to_exponent = base ^ exponent,
    sub_unit = c((1 / 1440) / (10 ^ -4), 7:9, rep(1:9, times = 6), 1),
    frequency = base ^ exponent * sub_unit,
    is_y_breaks = dplyr::case_when(sub_unit == 1 |
                                     sub_unit == 5 ~ TRUE,
                                   TRUE ~ FALSE)
  )
}


make_scale_x_params_old <- function(object) {

  #if(is.character(first_sunday)) {first_sunday <- ymd(first_sunday)}

  # Vérification
  #stopifnot(
  #  is.na(first_sunday) == FALSE,
  #  wday(first_sunday, week_start = 1) == 7
  #)

  if (is.na(object$arg_table$date)) {
    first_sunday <- as.Date(0)
  } else {
    first_sunday <- first_sunday(extract_pttable(object)[["date"]])
  }


  # Création de la base de données pour ggplot
  tibble::tibble(
    date = seq.Date(
      from = first_sunday,
      by = "day",
      length.out = 141 # Because the first sunday is 0, not 1
    ),
    no_date = seq(from = 0, to = 140),
    breaks = dplyr::case_when(
      no_date %% 7 == 0 ~ TRUE,
      TRUE ~ FALSE
    ),
    labels = dplyr::case_when(
      no_date %% 14 == 0 ~ as.character(no_date),
      no_date %% 7 == 0 ~ "",
      TRUE ~ NA_character_
    ),
    sec_axis_breaks = dplyr::case_when(
      no_date %% 28 == 0 ~ 1,
      TRUE ~ 0
    ),
    sec_axis_labels = dplyr::case_when(
      no_date %% 28 == 0 ~ no_date / 7,
      TRUE ~ NA_real_
    )
  )
}

first_date_old <- function(date){
  date[[1]]
}


last_date_old <- function(date){
  date[[length(date)]]
}







