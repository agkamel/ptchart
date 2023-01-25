#' ptstat
#'
#' @param data A Dataframe.
#' @param day Integer.
#' @param freq Double.
#' @param freq_err Double.
#' @param phase Charactor or factor.
#' @param date Date.
#' @param date_zero Date of length one.
#' @param count Integer.
#' @param count_err Integer.
#' @param time Double. Number of minutes.
#' @param x An S3 object of class ptstat.
#' @param ... Additional arguments.
#'
#' @return An object of class S3 which is a list.
#' @export
#'
#' @examples
#' # NOT RUN
ptstat <- function(data, day = NULL, freq = NULL, phase = NULL, date = NULL, date_zero = NULL, count = NULL, time = NULL, count_err = NULL, freq_err = NULL) {

  # Stopifnot
  stopifnot("`data` must be a data.frame" = is.data.frame(data))

  if (!is.null(day)) stopifnot("`day` must be numeric" = is.numeric(data[[day]]))
  if (!is.null(freq)) stopifnot("`freq` must be numeric" = is.numeric(data[[freq]]))
  if (!is.null(phase)) stopifnot("`phase` must be character or a class `factor`" = is.character(data[[phase]])) | class(data[[phase]]) == "factor"
  if (!is.null(date)) stopifnot("`date` must be a class `Date`" = class(data[[date]]) == "Date")
  if (!is.null(date_zero)) {
    stopifnot("`date_zero` must be a class `Date`" = class(date_zero) == "Date")
    stopifnot("`date_zero` must be of length 1" = length(date_zero) == 1)
    }
  if (!is.null(count)) stopifnot("`count` must be numeric" = is.numeric(data[[count]]))
  if (!is.null(time)) stopifnot("`time` must be numeric" = is.numeric(data[[time]]))
  if (!is.null(count_err)) stopifnot("`count_err` must be numeric" = is.numeric(data[[count_err]]))
  if (!is.null(freq_err)) stopifnot("`freq_err` must be numeric" = is.numeric(data[[freq_err]]))


  # Conditions for supplying day, date and date_zero
  if (!is.null(date) & !is.null(date_zero)){
    data[["day"]] <- calculate_day(data[[date]], date_zero)
    day <- "day"
    message("i `date` and `date_zero` were used to calculate `day`")
  } else if (!is.null(date) & is.null(date_zero)) {
    date_zero <- first_sunday(data[[date]])
    data[["day"]] <- calculate_day(data[[date]], date_zero)
    day <- "day"
    message("i `date` was used to calculate `day`")
  } else if (!is.null(day)) {
    message("i `day` was used to calculate `day`")
  } else {
    stop("! One of these combinaisons must be supplied:\n    `day`\n    `date`\n    `date` and `date_zero`")
  }


  # Conditions for supplying freq, count and time
  if (!is.null(count) & !is.null(time)) {
    data[["freq"]] <- calculate_freq(data[[count]], data[[time]])
    freq <- "freq"
    message("i `count` and `time` were used to calculate `freq`")
  } else if (!is.null(freq)) {
    message("i `freq` was used to calculate `freq`")
  } else {
    stop("! One of these combinaisons must be supplied:\n    `freq`\n    `count` and `time`")
  }


  # Conditions for supplying freq_err, count_err and time
  if (!is.null(count_err) & !is.null(time)) {
    data[["freq_err"]] <- calculate_freq(data[[count_err]], data[[time]])
    freq <- "freq_err"
    message("i `count_err` and `time` were used to calculate `freq_err`")
  } else if (!is.null(freq_err)) {
    message("i `freq_err` was used to calculate `freq_err`")
  } else {
    stop("! One of these combinaisons must be supplied:\n    `freq_err`\n    `count_err` and `time`")
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

    # Variable by phase
    iday <- splitted_data[[i]][[day]]
    ilog10_freq <- log10(splitted_data[[i]][[freq]])
    ifreq <- splitted_data[[i]][[freq]]
    iphase <- splitted_data[[i]][[phase]]

    idate <- if (!is.null(date)) splitted_data[[i]][[date]] else rep(NA, nrow(splitted_data[[i]]))
    icount <- if (!is.null(count)) splitted_data[[i]][[count]] else rep(NA, nrow(splitted_data[[i]]))
    itime <- if (!is.null(time)) splitted_data[[i]][[time]] else rep(NA, nrow(splitted_data[[i]]))
    icount_err <- if (!is.null(count_err)) splitted_data[[i]][[count_err]] else rep(NA, nrow(splitted_data[[i]]))
    ifreq_err <- if (!is.null(freq_err)) splitted_data[[i]][[freq_err]] else rep(NA, nrow(splitted_data[[i]]))
    ilog10_freq_err <- if (!is.null(freq_err)) log10(splitted_data[[i]][[freq_err]]) else rep(NA, nrow(splitted_data[[i]]))

    # pttables
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


    pttables[[i]] <- tibble::tibble(day = iday,
                                    date = idate,
                                    count = icount,
                                    count_err = icount_err,
                                    time = itime,
                                    freq = ifreq,
                                    freq_err = ifreq_err,
                                    log10_freq = ilog10_freq,
                                    phase = iphase,
                                    pred = predicted_values,
                                    errors = errors,
                                    pred_err = predicted_values_err,
                                    errors_err = errors_err)
  }

  # Make tables
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

  # Calculate turn
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








  # Return list
  ptstat_list <- list(pttables = pttables,
                      desc = desc_table,
                      terms = terms_table,
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
#' @describeIn ptstat Print method for ptstat
print.ptstat <- function(x, ...) { # Method print.ptstat()
  cat("", sep = "\n")
  cat("PT Tables", sep = "\n")
  print(x$pttables, ...)
  return(invisible(x))
}
