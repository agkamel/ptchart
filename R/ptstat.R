#' Computing measures of behavioral change.
#'
#' `ptchart()` is the main function for computing measures of behavioral change.
#'
#' @details
#' `ptchart()` returns a list that contains all calculations.
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
#' @param x An S3 object of class ptstat.
#' @param ... Additional arguments.
#'
#' @return `ptstat()` returns an S3 object of class `ptstat`, a list of computation tables.
#' @export
#'
#' @examples
#' ptstat(example_pt_data,
#'        day = "jour",
#'        count = "reponse",
#'        time = "minute",
#'        phase = "phase"
#' )
ptstat <- function(data,
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


  # Conditions for sypplying time
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

new_ptstat <- function(x = list()) { # Constructeur S3
  stopifnot(is.list(x))
  structure(x,
            class = "ptstat"
  )
}

validate_ptstat <- function(x) { # Validateur S3
  values <- unclass(x)
  x
}

#' @export
#' @describeIn ptstat Print method for `ptstat()`.
print.ptstat <- function(x, ...) { # Method print.ptstat()
  cat("", sep = "\n")
  cat("PT Tables", sep = "\n")
  print(x$pttables, ...)
  return(invisible(x))
}



#' @description `is_ptstat()` tests if the object is an S3 object of class `"ptstat"` (`TRUE`) or not (`FALSE`).
#'
#' @param x An R object.
#'
#' @return `is_ptstat()` returns a boolean scalar.
#' @export
#' @rdname ptstat
#' @examples
#' # TODO
is_ptstat <- function(x) {
  return(inherits(x, "ptstat"))
}

