#' @title
#' Bounce
#'
#' @description
#' Functions for calculating bounces.
#'
#' @details
#' Bounce, bounce up, bounce down, bounce total
#'
#' @param day Integer. Days of observation.
#' @param log10_freq Double. Base 10 logged frequencies.
#' @param type Character. One between `"up"` or `"down"` bounce.
#' @param raw Boolean. Is the returned value raw?
#'
#' @return A double.
#' @export
#'
#' @examples
#' # TODO
#' @describeIn bounce General bounce function
calculate_bounce <- function(day, log10_freq, type, raw = TRUE) {
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

#' @describeIn bounce Bounce up
calculate_bounce_up <- function(day, log10_freq, raw = TRUE) {
  calculate_bounce(day = day, log10_freq = log10_freq, type = "up", raw = raw)
}

#' @describeIn bounce Bounce down
calculate_bounce_down <- function(day, log10_freq, raw = TRUE) {
  calculate_bounce(day = day, log10_freq = log10_freq, type = "down", raw = raw)
}

#' @describeIn bounce Bounce total
calculate_bounce_total <- function(day, log10_freq) {
  bounce_up <- calculate_bounce_up(day, log10_freq, raw = FALSE)
  bounce_down <- calculate_bounce_down(day, log10_freq, raw = FALSE)
  bounce_up * bounce_down
}
