#' Format data for further analysis
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
#' @examples # pt_format_data()
pt_format_data <- function(data, date, count, time, phase, date_zero) {

  # NOTE: To prevent message of "Undefined global functions or variables 'freq'"
  freq <- NULL

  # Check if input data are in the right format
  stopifnot(

    # data
    "data must be of class `data.frame`." = "data.frame" %in% class(data),

    # date
    "Argument `date` must be of class `Date`." = class(data %>% dplyr::select({{ date }}) %>% dplyr::pull()) == "Date",

    # count
    "Argument `count` must be numeric." = class(data %>% dplyr::select({{ count }}) %>% dplyr::pull()) == "numeric",
    "Argument `count` must be greater of equal than 0." = data %>% dplyr::select({{ count }}) %>% dplyr::pull() >= 0,
    "Argument `count` must be whole numbers." = data %>% dplyr::select({{ count }}) %>% dplyr::pull() %% 1 == 0,

    # time
    "Argument `time` must be numeric." = class(data %>% dplyr::select({{ time }}) %>% dplyr::pull()) == "numeric",
    "Argument `time` must be greater than 0." = data %>% dplyr::select({{ time }}) %>% dplyr::pull() > 0,

    # phase
    # TODO

    # date_zero
    "Argument `date_zero` must be of class `Date`." = class(date_zero) == "Date",
    "Argument `date_zero` must be of length 1." = length(date_zero) == 1,
    "Argument `date_zero` must be a sunday." = pt_is_sunday(date_zero) == TRUE
  )

  # Format data
  data <- data %>%
    dplyr::transmute(
      date = {{ date }},
      day = as.integer({{ date }} - date_zero),
      count = {{ count }},
      time = {{ time }},
      timefloor = pt_time_floor({{ time }}),
      freq = pt_freq({{ count }}, {{ time }}),
      log10freq = log10(freq),
      phase = forcats::as_factor({{ phase }})
    )

  # Ordering by date
  data <- data %>% dplyr::arrange(date)

  return(data)
}
