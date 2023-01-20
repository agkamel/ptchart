#' ptstat
#'
#' @param data A Dataframe.
#' @param day Integer.
#' @param freq Double.
#' @param phase Charactor or factor.
#' @param date Date.
#' @param date_zero Date of length one.
#' @param x An S3 object of class ptstat.
#' @param ... Additional arguments.
#'
#' @return An object of class S3 which is a list.
#' @export
#'
#' @examples
#' # NOT RUN
ptstat <- function(data, day, freq, phase, date, date_zero) {

  # Condition for suppling day, date and date_zero
  if (missing(day)) {
    if (!missing(date) & !missing(date_zero)){
      data[["day"]] <- calculate_day(data[[date]], date_zero)
      day <- "day"
    } else if (!missing(date) & missing(date_zero)) {
      date_zero <- first_sunday(data[[date]])
      data[["day"]] <- calculate_day(data[[date]], date_zero)
      day <- "day"
    }
  }

  splitted_data <- split(data, data[[phase]])
  unique_phase <- unique(data[[phase]])
  phase_n <- length(unique_phase)

  # Initialize vectors
  pttables <- vector(mode = "list", length = length(unique_phase))
  names(pttables) <- unique_phase

  day_mean_values <- vector(mode = "double", length = phase_n)
  log10_freq_mean_values <- vector(mode = "double", length = phase_n)
  n_values <- vector(mode = "integer", length = phase_n)

  b1_values <- vector(mode = "double", length = phase_n)
  b0_values <- vector(mode = "double", length = phase_n)

  cel_raw <- vector(mode = "double", length = phase_n)
  cel_values <- vector(mode = "double", length = phase_n)

  bounce_up_raw <- vector(mode = "double", length = phase_n)
  bounce_up_values <- vector(mode = "double", length = phase_n)
  bounce_down_raw <- vector(mode = "double", length = phase_n)
  bounce_down_values <- vector(mode = "double", length = phase_n)
  bounce_total_values <- vector(mode = "double", length = phase_n)

  # Calculate main values
  for (i in seq_along(splitted_data)) {

    # Variable by phase
    iday <- splitted_data[[i]][[day]]
    ilog10_freq <- log10(splitted_data[[i]][[freq]])
    ifreq <- splitted_data[[i]][[freq]]
    iphase <- splitted_data[[i]][[phase]]

    # pttables
    predicted_values <- calculate_predicted_values(iday, ilog10_freq)
    errors <- calculate_errors(iday, ilog10_freq)

    # desc_table
    n_values[i] <- nrow(splitted_data[[i]])
    day_mean_values[i] <- mean(iday)
    log10_freq_mean_values[i] <- mean(ilog10_freq)

    # terms_table
    b1_values[i] <- calculate_b1(iday, ilog10_freq)
    b0_values[i] <- calculate_b0(iday, ilog10_freq)

    # c_table
    cel_raw[i] <- celeration(iday, ilog10_freq)
    cel_values[i] <- celeration(iday, ilog10_freq, raw = FALSE)

    # b_table
    bounce_up_raw[i] <- bounce_up(iday, ilog10_freq)
    bounce_up_values[i] <- bounce_up(iday, ilog10_freq, raw = FALSE)
    bounce_down_raw[i] <- bounce_down(iday, ilog10_freq)
    bounce_down_values[i] <- bounce_down(iday, ilog10_freq, raw = FALSE)
    bounce_total_values[i] <- bounce_total(iday, ilog10_freq)


    pttables[[i]] <- tibble::tibble(day = iday,
                                    freq = ifreq,
                                    log10_freq = ilog10_freq,
                                    phase = iphase,
                                    pred = predicted_values,
                                    errors = errors)
  }

  # Make tables
  desc_table <- tibble::tibble(phase = unique_phase,
                               n = n_values,
                               day_mean = day_mean_values,
                               log10_freq_mean = log10_freq_mean_values)

  terms_table <- tibble::tibble(phase = unique_phase,
                                b0 = b0_values,
                                b1 = b1_values)

  c_table <- tibble::tibble(phase = unique_phase,
                            c_raw = cel_raw,
                            c = cel_values)

  b_table <- tibble::tibble(phase = unique_phase,
                            b_up_raw = bounce_up_raw,
                            b_up = bounce_up_values,
                            b_down_raw = bounce_down_raw,
                            b_down = bounce_down_values,
                            b_total = bounce_total_values)










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
