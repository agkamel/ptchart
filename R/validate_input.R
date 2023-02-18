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
    stop("`day` must be greater or equal than 0`")
  }

  if ((sum(data[[day]] %% 1) > 0) == TRUE) {
    stop("`day` must be a integer")
  }
}

validate_arg_freq <- function(data, freq) {
  if (is.numeric(data[[freq]]) == FALSE) {
    stop("`freq` must be numeric")
  }

  if ((FALSE %in% (data[[freq]] >= 0)) == TRUE) {
    stop("`freq` must be greater or equal than 0`")
  }

  if ( ((Inf %in% (data[[freq]])) == TRUE) | ((-Inf %in% (data[[freq]])) == TRUE) ) {
    stop("`freq` must not contain `Inf` or `-Inf` values")
  }
}

validate_arg_phase <- function(data, phase) {
  if ((is.character(data[[phase]]) | inherits(data[[phase]], "factor")) == FALSE) {
    stop("`phase` must be character or a class `factor`")
  }
}

validate_arg_date <- function(data, date) {
  if (inherits(data[[date]], "Date") == FALSE)
    stop("`date` must be a class `Date`")
}

validate_arg_date_zero <- function(date_zero) {
  if (inherits(date_zero, "Date") == FALSE) {
    stop("`date_zero` must be a class `Date`")
  }
  if (length(date_zero) != 1) {
    stop("`date_zero` must be of length 1")
  }
}

validate_arg_count <- function(data, count) {
  if (is.numeric(data[[count]]) == FALSE) {
    stop("`count` must be numeric")
  }
  if ((FALSE %in% (data[[count]] >= 0)) == TRUE) {
    stop("`count` must be greater or equal than 0`")
  }
  if ((sum(data[[count]] %% 1) > 0) == TRUE) {
    stop("`count` must be a integer")
  }
}
