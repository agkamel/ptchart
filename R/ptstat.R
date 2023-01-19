#' ptstat
#'
#' @param data A Dataframe.
#' @param day Integer.
#' @param freq Double.
#' @param phase Charactor or factor.
#' @param x An S3 object of class ptstat.
#'
#' @return An object of class S3 which is a list.
#' @export
#'
#' @examples
#' # NOT RUN
ptstat <- function(data, day, freq, phase) {

  splitted_data <- split(data, data[[phase]])

  unique_phase <- unique(data[[phase]])

  pttables <- vector(mode = "list", length = length(unique_phase))
  names(pttables) <- unique_phase

  ptmainvalues <- vector(mode = "list", length = length(unique_phase))
  names(ptmainvalues) <- unique_phase

  for (i in seq_along(splitted_data)) {
    iday <- splitted_data[[i]][[day]]
    ilog10_freq <- log10(splitted_data[[i]][[freq]])
    ifreq <- splitted_data[[i]][[freq]]
    iphase <- splitted_data[[i]][[phase]]

    b1 <- calculate_b1(iday, ilog10_freq)
    b0 <- calculate_b0(iday, ilog10_freq)
    day_mean <- mean(iday, na.rm = TRUE)
    log10_freq_mean <- mean(ilog10_freq, na.rm = TRUE)

    predicted_values <- calculate_predicted_values(iday, ilog10_freq)
    errors <- calculate_errors(iday, ilog10_freq)

    c <- celeration(iday, ilog10_freq, raw = FALSE)
    c_raw <- celeration(iday, ilog10_freq)
    b_up <- bounce_up(iday, ilog10_freq, raw = FALSE)
    b_up_raw <- bounce_up(iday, ilog10_freq)
    b_down <- bounce_down(iday, ilog10_freq, raw = FALSE)
    b_down_raw <- bounce_down(iday, ilog10_freq)
    b_total <- bounce_total(iday, ilog10_freq)


    pttables[[i]] <- tibble::tibble(day = iday,
                                    freq = ifreq,
                                    log10_freq = ilog10_freq,
                                    phase = iphase,
                                    pred = predicted_values,
                                    errors = errors)

    ptmainvalues[[i]] <- tibble::tibble(b0 = b0,
                                    b1 = b1,
                                    day_mean = day_mean,
                                    log10_freq_mean = log10_freq_mean,
                                    c_raw = c_raw,
                                    c = c,
                                    b_up_raw = b_up_raw,
                                    b_up = b_up,
                                    b_down_raw = b_down_raw,
                                    b_down = b_down,
                                    b_total = b_total)
  }

  # Calculate jumps
  jump_phase_names <- vector(mode = "character", length = length(unique_phase) - 1)
  jump_raw <- vector(mode = "double", length = length(unique_phase) - 1)
  jump_values <- vector(mode = "double", length = length(unique_phase) - 1)

  for (i in seq_along(length(unique_phase) - 1)) {
    a <- pttables[[i]]
    b <- pttables[[i+1]]
    jump_raw[i] <- antilog(b$pred[1] - a$pred[length(a$pred)])
    jump_values[i] <- convert_value(antilog(b$pred[1] - a$pred[length(a$pred)]))

    a_name <- names(pttables[i])
    b_name <- names(pttables[i+1])
    jump_phase_names[i] <- paste0(a_name, "_to_", b_name)
  }

  ptjumpvalues <- tibble::tibble(change = jump_phase_names,
                                 j_raw = jump_raw,
                                 j = jump_values)





  # Calculate turn
  turn_phase_names <- vector(mode = "character", length = length(unique_phase) - 1)
  turn_raw <- vector(mode = "double", length = length(unique_phase) - 1)
  turn_values <- vector(mode = "double", length = length(unique_phase) - 1)

  for (i in seq_along(length(unique_phase) - 1)) {
    a <- ptmainvalues[[i]]
    b <- ptmainvalues[[i+1]]
    turn_raw[i] <- antilog((b$b1 * 7) - (a$b1 * 7))
    turn_values[i] <- convert_value(antilog((b$b1 * 7) - (a$b1 * 7)))

    a_name <- names(ptmainvalues[i])
    b_name <- names(ptmainvalues[i+1])
    turn_phase_names[i] <- paste0(a_name, "_to_", b_name)
  }

  ptturnvalues <- tibble::tibble(change = turn_phase_names,
                                 t_raw = turn_raw,
                                 t = turn_values)








  # Return list
  ptstat_list <- list(pttables = pttables,
                      ptmainvalues = ptmainvalues,
                      ptjumpvalues = ptjumpvalues,
                      ptturnvalues = ptturnvalues)

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


#' @describeIn ptstat Print method for ptstat
print.ptstat <- function(x) { # Method print.ptstat()
  cat("", sep = "\n")
  cat("PT Values", sep = "\n")
  print(x$ptmainvalues)
  cat("", sep = "\n")
  cat("PT Tables", sep = "\n")
  print(x$pttables)
  return(invisible(x))
}
