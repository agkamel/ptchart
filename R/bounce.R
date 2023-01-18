#' Bounce
#'
#' @param day
#' @param log10_freq
#' @param raw
#'
#' @return A double.
#' @export
#'
#' @examples
bounce_up <- function(day, log10_freq, raw = TRUE) {
  errors <- calculate_errors(day, log10_freq)
  max_error <- antilog(max(errors))

  if (raw == TRUE) {
    max_error
  } else {
    convert_value(max_error)
  }
}

#' @rdname add
bounce_down <- function(day, log10_freq, raw = TRUE) {
  errors <- calculate_errors(day, log10_freq)
  min_error <- antilog(min(errors))

  if (raw == TRUE) {
    min_error
  } else {
    convert_value(min_error)
  }

}

#' @rdname add
bounce_total <- function(day, log10_freq) {
  bounce_up <- bounce_up(day, log10_freq, raw = FALSE)
  bounce_down <- bounce_down(day, log10_freq, raw = FALSE)

  bounce_up * bounce_down
}
