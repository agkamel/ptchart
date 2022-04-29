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
  lubridate::wday(date, week_start = 1) == 7
}
