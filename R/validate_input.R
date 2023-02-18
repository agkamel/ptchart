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
  if ((is.character(data[[phase]]) | (class(data[[phase]]) == "factor")) == FALSE) {
    stop("`phase` must be character or a class `factor`")
  }
}




