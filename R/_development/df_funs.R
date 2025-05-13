
#' Get base 10 logarithm
#'
#' @param x A numeric vector.
#'
#' @return A numeric Vector.
#' @rdname df_funs
#' @export
#'
#' @examples
#' #NOT YET
df_log10 <- function(x) {
  log10(x)
}


.remove_NAs <- function(.df, x, y) {
  .df_original <- .df |> dplyr::select(x = {{ x }}, y = {{ y }})
  .df_no_NAs <- .df_original |> dplyr::filter(!is.na(x), !is.na(y))

  if (nrow(.df_no_NAs) < 3) {
    warning("Number of non NA rows are smaller than 3. Returning NA.")
    return(NA)
  }

  .df_no_NAs
}


#' Get predicted values
#'
#' @param .df A dataframe.
#' @param x Day of calendar.
#' @param y Frequency.
#'
#' @return Predicted values
#' @rdname df_funs
#' @export
#'
#' @examples
#' # NOT YET
df_predicted_values <- function(.df, x, y) {

  .df_no_NAs <- .remove_NAs(.df, {{ x }}, {{ y }}) |> dplyr::mutate(log10_y = df_log10(y))

  b0 <- df_b0(.df, {{ x }}, {{ y }})
  b1 <- df_b1(.df, {{ x }}, {{ y }})

  b0 + b1 * .df_no_NAs$x
}


#' Get errors
#'
#' @param .df A dataframe.
#' @param x Day of calendar.
#' @param y Frequency.
#'
#' @return Errors
#' @rdname df_funs
#' @export
#'
#' @examples
#' # NOT YET
df_errors <- function(.df, x, y) {

  .df_no_NAs <- .remove_NAs(.df, {{ x }}, {{ y }}) |> dplyr::mutate(log10_y = df_log10(y))
  predicted_values <- df_predicted_values(.df, {{ x }}, {{ y }})
  .df_no_NAs$log10_y - predicted_values
}

#' Get b1
#'
#' @param .df A dataframe.
#' @param x Day of calendar.
#' @param y Frequency.
#'
#' @return b1
#' @rdname df_funs
#' @export
#'
#' @examples
#' #NOT YET
df_b1 <- function(.df, x, y) {

  # To prevent note of "no visible binding for global variable 'x'" when building the package
  log10_y <- x_deviation_from_mean <- NULL

  .df_no_NAs <- .remove_NAs(.df, {{ x }}, {{ y }}) |> dplyr::mutate(log10_y = df_log10(y))

  .df_no_NAs <- .df_no_NAs |> dplyr::mutate(
    x = x,
    y = y,
    x_deviation_from_mean = x - mean(x),
    x_deviation_from_mean_squared = x_deviation_from_mean ^ 2,
    log10_y_deviation_from_mean = log10_y - mean(log10_y)
  )

  sum(.df_no_NAs$x_deviation_from_mean * .df_no_NAs$log10_y_deviation_from_mean) / sum(.df_no_NAs$x_deviation_from_mean_squared)
}






#' Get b0
#'
#' @param .df A dataframe.
#' @param x Day of calendar.
#' @param y Frequency.
#'
#' @return b0
#' @rdname df_funs
#' @export
#'
#' @examples
#' # NOT YET
df_b0 <- function(.df, x, y) {

  # To prevent note of "no visible binding for global variable 'x'" when building the package
  log10_y <- NULL

  b1 <- df_b1(.df, {{ x }}, {{ y }})

  .df_no_NAs <- .remove_NAs(.df, {{ x }}, {{ y }}) |> dplyr::mutate(log10_y = df_log10(y))
  .df_means <- .df_no_NAs |> dplyr::summarise(mean_x = mean(x), mean_log10_y = mean(log10_y))

  .df_means$mean_log10_y - (b1 * .df_means$mean_x)
}


#' Get celeration
#'
#' @param .df A dataframe.
#' @param x Day of calendar.
#' @param y Frequency.
#'
#' @return A dataframe.
#' @rdname df_funs
#' @export
#'
#' @examples
#' # NOT YET
df_celeration <- function(.df, x, y) {
  b1 <- df_b1(.df, {{ x }}, {{ y }})
  (10^b1)^7
}

#  load_all()
#  df_b1(mini_test_data, x_aug, y_aug)
#  df_celeration(mini_test_data, x_aug, y_aug)
#  df_b0(mini_test_data, x_aug, y_aug)


df_antilog <- function(x, base = 10) {
  base^x
}

#' Get bounce up.
#'
#' @param .df A dataframe.
#' @param x Day of calendar.
#' @param y Frequency.
#'
#' @return Bounce up
#' @rdname df_funs
#' @export
#'
#' @examples
#' # NOT YET
df_bounce_up <- function(.df, x, y) {
  errors <- df_errors(.df, {{ x }}, {{ y }})
  df_antilog(max(errors))
}

#' Get bounce down.
#'
#' @param .df A dataframe.
#' @param x Day of calendar.
#' @param y Frequency.
#'
#' @return Bounce down
#' @rdname df_funs
#' @export
#'
#' @examples
#' # NOT YET
df_bounce_down <- function(.df, x, y) {
  errors <- df_errors(.df, {{ x }}, {{ y }})
  df_antilog(min(errors))
}

#' Get bounce total.
#'
#' @param .df A dataframe.
#' @param x Day of calendar.
#' @param y Frequency.
#'
#' @return Bounce total
#' @rdname df_funs
#' @export
#'
#' @examples
#' # NOT YET
df_bounce_total <- function(.df, x, y) {
  df_bounce_up(.df, {{ x }}, {{ y }}) * df_bounce_down(.df, {{ x }}, {{ y }})
}


#' Get accuracy ratio
#'
#' @param .df A dataframe.
#' @param y_cor Correct frequency.
#' @param y_incor Incorrect frequency.
#'
#' @return Accuracy ratio
#' @rdname df_funs
#' @export
#'
#' @examples
#' # NOT YET
df_accuracy_ratio <- function(.df, y_cor, y_incor) {

  # To prevent note of "no visible binding for global variable 'x'" when building the package
  x <- y <- NULL

  .df_no_NAs <- .remove_NAs(.df, {{ y_cor }}, {{ y_incor }}) |>
    dplyr::rename(y_cor = x, y_incor = y)

  .df_no_NAs$y_cor / .df_no_NAs$y_incor

}


#' Get accuracy
#'
#' @param .df A dataframe.
#' @param x Day of calendar.
#' @param y_cor Correct frequency.
#' @param y_incor Incorrect frequency.
#'
#' @return Accuracy
#' @rdname df_funs
#' @export
#'
#' @examples
#' # NOT YET
df_accuracy <- function(.df, x, y_cor, y_incor){

  .df_original <- .df |> dplyr::select(x = {{ x }}, y_cor = {{ y_cor }}, y_incor = {{ y_incor }})
  .df_no_NAs <- .df_original |> dplyr::filter(!is.na(x), !is.na(y_cor), !is.na(y_incor))

  accuracy_ratio <- df_accuracy_ratio(.df_no_NAs, y_cor, y_incor)

  .df_no_NAs <- .df_no_NAs |> dplyr::mutate(accuracy_ratio = accuracy_ratio)

  b1 <- df_b1(.df_no_NAs, x, accuracy_ratio)

  df_antilog(b1)^7

}



df_jump <- function(.df, x, y, phase) {

  .df_original <- .df |> dplyr::select(x = {{ x }}, y = {{ y }}, phase = {{ phase }})
  .df_no_NAs <- .df_original |> dplyr::filter(!is.na(x), !is.na(y), !is.na(phase))

  print(.df_original)
  print(.df_no_NAs)

  .phase_list <- split(.df_no_NAs,
  dplyr::select(.df_no_NAs, phase) |> dplyr::pull())

  print(.phase_list)

  jump_output <- vector(mode = "numeric", length = (length(.phase_list) - 1))

  for (i in 1:(length(.phase_list) - 1)) {

    jump_output[i] <- df_antilog(
      df_predicted_values(.phase_list[[i + 1]], x, y) |> dplyr::first() -
        df_predicted_values(.phase_list[[i]], x, y) |> dplyr::last()
    )
  }

  jump_output
}


