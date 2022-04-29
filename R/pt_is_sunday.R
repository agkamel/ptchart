#' Verify if a date is a sunday
#'
#' @param date A date vector.
#'
#' @return A date vector
#' @importFrom lubridate wday
#' @export
#'
#' @examples # pt_is_sunday(date = as.Date(2022-04-29))
pt_is_sunday <- function(date) {

  # Check if input data are in the right format (if used outside of pt_format_data())
  stopifnot(
  "Argument `date` must be of class `Date`." = class(date) == "Date"
  )

  lubridate::wday(date, week_start = 1) == 7
}
