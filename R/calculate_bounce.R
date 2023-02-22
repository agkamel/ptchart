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
