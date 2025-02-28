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

  if ((Inf %in% (data[[freq]])) == TRUE) {
    stop("`freq` must not contain `Inf` or `-Inf` values")
  }
}

validate_arg_freq_err <- function(data, freq_err) {
  if (is.numeric(data[[freq_err]]) == FALSE) {
    stop("`freq_err` must be numeric")
  }

  if ((FALSE %in% (data[[freq_err]] >= 0)) == TRUE) {
    stop("`freq_err` must be greater or equal than 0`")
  }

  if ((Inf %in% (data[[freq_err]])) == TRUE) {
    stop("`freq_err` must not contain `Inf` or `-Inf` values")
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

validate_arg_count_err <- function(data, count_err) {
  if (is.numeric(data[[count_err]]) == FALSE) {
    stop("`count_err` must be numeric")
  }
  if ((FALSE %in% (data[[count_err]] >= 0)) == TRUE) {
    stop("`count_err` must be greater or equal than 0`")
  }
  if ((sum(data[[count_err]] %% 1) > 0) == TRUE) {
    stop("`count_err` must be a integer")
  }
}



validate_arg_time <- function(data, time) {
  if (is.numeric(data[[time]]) == FALSE) {
    stop("`time` must be numeric")
  }

  if ((FALSE %in% (data[[time]] > 0)) == TRUE) {
    stop("`time` must be greater than 0`")
  }
}



validate_numeric_vctr <- function(x,
                                  lower_lim = -Inf,
                                  upper_lim = Inf,
                                  lower_excluded = TRUE,
                                  upper_excluded = TRUE
                                  ) {
  x_arg <- substitute(x)
  lower_lim_val <- as.character(lower_lim)
  upper_lim_val <- as.character(upper_lim)
  #lower_lim_exc_val <- as.character(lower_lim_exc)
  #upper_lim_exc_val <- as.character(upper_lim_exc)

  if (!is.atomic(x)) {
    return(list(value = FALSE,
                message = paste0("`", x_arg, "` must be an atomic vector.")))
  }

  if (!is.numeric(x)) {
    return(list(value = FALSE,
                message = paste0("`", x_arg, "` must be a numeric vector.")))
  }

  if (any(x < lower_lim) && lower_excluded) {
      return(list(value = FALSE,
                  message = paste0("All values in `", x_arg, "` must be greater or equal than ", lower_lim_val, ".")))
    }

  if (any(x <= lower_lim) && lower_excluded == FALSE) {
      return(list(value = FALSE,
                  message = paste0("All values in `", x_arg, "` must be strictly greater than ", lower_lim_val, ".")))
    }

  if (any(x > upper_lim) && upper_excluded) {
      return(list(value = FALSE,
                  message = paste0("All values in `", x_arg, "` must be lower or equal than ", upper_lim_val, ".")))
    }

  if (any(x >= upper_lim) && upper_excluded == FALSE) {
      return(list(value = FALSE,
                  message = paste0("All values in `", x_arg, "` must be strictly lower than ", upper_lim_val, ".")))
    }

  return(list(value = TRUE,
                  message = ""))
}


# TODO ?
#validate_logical_vctr
#validate_character_vctr


validate_xy <- function(x, y) {

  x_arg <- substitute(x)
  y_arg <- substitute(y)

  if (!is.atomic(x)) {
    return(list(value = FALSE,
                message = paste0("`", x_arg, "` must be an atomic vector.")))
  } else if (!is.atomic(y)) {
    return(list(value = FALSE,
                message = paste0("`", y_arg, "` must be an atomic vector.")))
  } else if (!is.numeric(x)) {
    return(list(value = FALSE,
                message = paste0("`", x_arg, "` must be a numeric vector.")))
  } else if (!is.numeric(y)) {
    return(list(value = FALSE,
                message = paste0("`", y_arg, "` must be a numeric vector.")))
  } else if (length(x) != length({{ y }})) {
    return(list(value = FALSE,
                message = paste0("`", x_arg, "` and `", y_arg, "` must have the same length.")))
  } else {
    return(list(value = TRUE,
                message = ""))
  }
}






# For testing validator
a_generic_function <- function(some_arg) {

  if (validate_numeric_vctr(some_arg)$value == FALSE) { stop(validate_numeric_vctr(some_arg)$message) }

  TRUE

}
#a_generic_function(c(1, 2,3,Inf))
