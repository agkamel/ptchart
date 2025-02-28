#' Computing measures of behavioral change.
#'
#' @description
#'
#'
#' `ptstat_old()` is the main function for computing measures of behavioral change.
#'
#' @param .df Dataframe.
#' @param day Integer.
#' @param freq Double.
#' @param phase Character.
#' @param date Date class.
#' @param date_zero Scalar date class.
#' @param count Integer.
#' @param time Double.
#' @param count_err Integer.
#' @param freq_err Double.
#' @param count_floor Double.
#' @param count_ceil Double.
#' @param verbose Logical scalar.
#' @param x An S3 object of class ptstat.
#' @param ... Additional arguments.
#'
#' @returns An S3 object of class `ptstat`, which is a list.
#' @export
#' @examples
#' ptstat(example_pt_data,
#'        day = jour, freq = frequence, phase = phase,
#'        freq_err = frequence_nc)
ptstat <- function(.df,
                   day = NULL,
                   freq = NULL,
                   phase = NULL,
                   date = NULL,
                   date_zero = NULL,
                   count = NULL,
                   time = NULL,
                   count_err = NULL,
                   freq_err = NULL,
                   count_floor = NULL,
                   count_ceil = NULL,
                   verbose = TRUE
                   ) {

  .count <- .count_ceil <- .count_err <- .count_floor <- .date <- .day <- .freq <- .freq_err <- .phase <- .time <- b0 <- b1 <- b_down <- b_total <- b_up <- cel <- first_day <- first_freq <- lag_freq <- lead_day <- new_freq <- turn <- jump <-  NULL

  # First scenario
  # date, count, time are provided

  if (any(names(.df) %in% c(".day", ".freq", ".phase", ".date", ".count",
                            ".time", ".count_err", ".freq_err", ".count_floor",
                            ".count_ceil"))) {
    stop("These are reserved colnames: .day, .freq, .phase,
         .date, .count, .time, .count_err, .freq_err, .count_floor and .count_ceil.",
         call. = FALSE)
  }

  main_df <- .df |>
    dplyr::mutate(.day = if (is.null({{ day }})) NA else {{ day }},
                  .freq = if (is.null({{ freq }})) NA else {{ freq }},
                  .phase = if (is.null({{ phase }})) NA else {{ phase }},
                  .date = if (is.null({{ date }})) NA else {{ date }},
                  .count = if (is.null({{ count }})) NA else {{ count }},
                  .time = if (is.null({{ time }})) NA else {{ time }},
                  .count_err = if (is.null({{ count_err }})) NA else {{ count_err }},
                  .freq_err = if (is.null({{ freq_err }})) NA else {{ freq_err }},
                  .count_floor = if (is.null({{ count_floor }})) NA else {{ count_floor }},
                  .count_ceil = if (is.null({{ count_ceil }})) NA else {{ count_ceil }})

  main_df <- main_df |>
    dplyr::select(dplyr::starts_with(".")) |>
    dplyr::rename(day = .day,
           freq = .freq,
           phase = .phase,
           date = .date,
           count = .count,
           time = .time,
           count_err = .count_err,
           freq_err = .freq_err,
           count_floor = .count_floor,
           count_ceil = .count_ceil)

  main_df <- main_df |>
    dplyr::mutate(acc_ratio = pt_vct_accuracy_ratio(day, freq, freq_err))

  print(main_df)


  single_phase_table <- main_df |>
    dplyr::group_by(phase) |>
    dplyr::summarise(
      b0 = pt_vct_b0(day, freq),
      b1 = pt_vct_b1(day, freq),
      cel0 = pt_vct_celeration_0(day, freq),
      cel = pt_vct_celeration(day, freq),
      b_up = pt_vct_bounce_up(day, freq),
      b_down = pt_vct_bounce_down(day, freq),
      b_total = pt_vct_bounce_total(day, freq),
      acc = pt_vct_accuracy(day, freq, freq_err),
      first_day = dplyr::first(day),
      first_freq = dplyr::first(freq)) |>
    dplyr::mutate(turn = dplyr::lag(cel) / cel)

  single_phase_table |>
    dplyr::mutate(
      # # Jump method 1
      # first_daym1 = first_day - 1,
      # lead_daym1 = dplyr::lead(first_day - 1),
      # freq_daym1 = 10^b0 * 10^(b1*lead_daym1),
      # lag_freq_daym1 = dplyr::lag(freq_daym1),
      # jump1 = 10^log10(first_freq) / 10^log10(lag_freq_daym1),

      # # Jump method 2
      lead_day = dplyr::lead(first_day),
      new_freq = 10^b0 * 10^(b1 * lead_day),
      lag_freq = dplyr::lag(new_freq),
      jump = 10^log10(first_freq) / 10^log10(lag_freq)
    ) |>
    dplyr::select(!c(lead_day, new_freq, lag_freq)) |>
    dplyr::mutate(
      cel = ptvalue::as_ptvalue(cel),
      b_up = ptvalue::as_ptvalue(b_up),
      b_down = ptvalue::as_ptvalue(b_down),
      b_total = ptvalue::as_ptvalue(b_total),
      turn = ptvalue::as_ptvalue(turn),
      jump = ptvalue::as_ptvalue(jump)
    )


  ptstat_list <- list(
    main_df = main_df,
    single_phase_table = single_phase_table
  )

  validate_ptstat(new_ptstat(ptstat_list))

}




new_ptstat <- function(x = list()) { # Constructeur S3
  stopifnot(is.list(x))
  structure(x,
            class = "ptstat"
  )
}

validate_ptstat <- function(x) { # Validateur S3
  values <- unclass(x)
  x
}

#' @export
#' @describeIn ptstat Print method for `ptstat()`.
print.ptstat <- function(x, ...) { # Method print.ptstat()
  cat("", sep = "\n")
  cat("PT Tables", sep = "\n")
  print(x$pttables, ...)
  return(invisible(x))
}



#' @description `is_ptstat()` tests if the object is an S3 object of class `"ptstat"` (`TRUE`) or not (`FALSE`).
#'
#' @param x An R object.
#'
#' @return `is_ptstat()` returns a boolean scalar.
#' @export
#' @rdname ptstat
#' @examples
#' # TODO
is_ptstat <- function(x) {
  return(inherits(x, "ptstat"))
}





