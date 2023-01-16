new_ptstat <- function(x = list()) {
  stopifnot(is.list(x))
  structure(x,
            class = "ptstat"
  )
}


# Validateur S3 validate_ptstat() -----------------------------------------
validate_ptstat <- function(x) {
  values <- unclass(x)
  x
}


# Helper S3 ptstat() ------------------------------------------------------
ptstat <- function(data, day, freq, phase) {

  splitted_data <- split(data, data[[phase]])

  unique_phase <- unique(data[[phase]])

  pttables <- vector(mode = "list", length = length(unique_phase))
  names(pttables) <- unique_phase
  ptvalues <- vector(mode = "list", length = length(unique_phase))
  names(ptvalues) <- unique_phase

  for (i in seq_along(splitted_data)) {
    iday <- splitted_data[[i]][[day]]
    ilog10_freq <- log10(splitted_data[[i]][[freq]])
    ifreq <- splitted_data[[i]][[freq]]
    iphase <- splitted_data[[i]][[phase]]

    b1 <- calculate_b1(iday, ilog10_freq)
    b0 <- calculate_b0(iday, ilog10_freq)

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

    ptvalues[[i]] <- tibble::tibble(b0 = b0,
                                    b1 = b1,
                                    c_raw = c_raw,
                                    c = c,
                                    b_up_raw = b_up_raw,
                                    b_up = b_up,
                                    b_down_raw = b_down_raw,
                                    b_down = b_down,
                                    b_total = b_total)

  }


  ptstat_list <- list(pttables = pttables,
                      ptvalues = ptvalues)

  validate_ptstat(new_ptstat(ptstat_list))
}



# Method print.ptstat() ---------------------------------------------------
print.ptstat <- function(x) {
  cat("", sep = "\n")
  cat("PT Values", sep = "\n")
  print(x$ptvalues)
  cat("", sep = "\n")
  cat("PT Tables", sep = "\n")
  print(x$pttables)
  return(invisible(x))
}
