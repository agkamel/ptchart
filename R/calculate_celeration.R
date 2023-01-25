#' @title
#' Calculate celeration
#'
#' @description
#' Calculate celeration.
#'
#' @param day Integer.
#' @param log10_freq Double.
#' @param raw Boolean.
#'
#' @return Double.
#' @export
#'
#' @examples
#' #TODO
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
