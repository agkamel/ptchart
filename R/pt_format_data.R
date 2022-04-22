#' Format data for futher analysis
#'
#' @param data A dataframe.
#' @param date A date vector.
#' @param count An integer vector.
#' @param time A numeric vector.
#' @param phase A factor vector.
#' @param date_zero A date of length one. Date of the first sunday.
#'
#' @return A dataframe
#' @import dplyr
#' @importFrom forcats as_factor
#' @export
#'
#' @examples # format_data()
format_data <- function(data, date, count, time, phase, date_zero) {

  # Data type checking
  stopifnot(
    class(data %>% dplyr::select({{ date }}) %>% dplyr::pull()) == "Date",
    class(data %>% dplyr::select({{ count }}) %>% dplyr::pull()) == "numeric",
    class(data %>% dplyr::select({{ time }}) %>% dplyr::pull()) == "numeric",
    class({{ date_zero }}) == "Date"
  )

  # Format data
  data <- data %>%
    dplyr::transmute(
      date = {{ date }},
      day = as.integer({{ date }} - {{ date_zero }}),
      count = {{ count }},
      time = {{ time }},
      timefloor = pt_time_floor({{ time }}),
      frequency = pt_frequency({{ count }}, {{ time }}),
      log10freq = log10(frequency),
      phase = forcats::as_factor({{ phase }})
    )

  # Ordering by date
  data <- data %>% dplyr::arrange(date)

  return(data)
}
