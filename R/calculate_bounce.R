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
bounce <- function(day, log10_freq, type, raw = TRUE) {
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
bounce_up <- function(day, log10_freq, raw = TRUE) {
  bounce(day = day, log10_freq = log10_freq, type = "up", raw = raw)
}

#' @describeIn bounce Bounce down
bounce_down <- function(day, log10_freq, raw = TRUE) {
  bounce(day = day, log10_freq = log10_freq, type = "down", raw = raw)
}

#' @describeIn bounce Bounce total
bounce_total <- function(day, log10_freq) {
  bounce_up <- bounce_up(day, log10_freq, raw = FALSE)
  bounce_down <- bounce_down(day, log10_freq, raw = FALSE)
  bounce_up * bounce_down
}
