validate_arg_data <- function(data) {
  if (is.data.frame(data) == FALSE) {
    stop("`data` must be a data.frame")
  }
}

validate_arg_day <- function(data, day) {
  if (is.numeric(data[[day]]) == FALSE) {
    stop("`day` must be numeric")
  }

  if ((FALSE %in% (data[[day]] >= 0)) == TRUE) {
    stop("`day must be greater or equal than 0`")
  }

  if ((sum(data[[day]] %% 1) > 0) == TRUE) {
    stop("`day` must be a integer")
  }
}
