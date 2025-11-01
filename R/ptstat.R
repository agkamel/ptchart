#' Computing measures of behavioral change.
#'
#' @description
#' `ptstat()` is the main function for computing measures of behavioral change.
#'
#' @param .df Dataframe.
#' @param day Integer. No of the successive day of the calendar. Values must be integers and >= 0.
#' @param freq Double. Frequencies of observations aimed to be accelerated. Values must be >= 0.
#' @param freq_e Double. Frequencies of observations aimed to be decelerated. Values must be >= 0.
#' @param phase Character. Phase of intervention. If none are provided, default phase is `"A"` for all observations. Note: Providing factor vector to be implemented.
#' @param date Date. Dates of observations in the format "yyyy-mm-dd". If `date_zero` is not provided, the closest sunday before the first date will be used for `date_zero`.
#' @param date_zero Date scalar. Date of length one. A date that correspond to day 0 in the format "yyyy-mm-dd".
#' @param count Integer. Count of responses aimed to be accelerated. Values must be >= 0.
#' @param count_e Integer. Count of responses aimed to be decelerated. Values must be >= 0.
#' @param time Double. Number of minutes. Values must be > 0.
#' @param record_floor Double. NOTE. Name may be changed...
#' @param behavior_floor Double. NOTE. Name may be changed...
#' @param record_ceil Double. NOTE. Name may be changed...
#' @param verbose Logical scalar. Show informational messages. Default to `TRUE`.
#' @param x An S3 object of class ptstat.
#' @param ... Additional arguments.
#'
#' @returns A S3 object of class `ptstat`, a list.
#' @export
#' @examples
#' ptstat(example_pt_data,
#'        day = jour,
#'        freq = frequence,
#'        phase = phase,
#'        time = minute,
#'        freq_e = frequence_nc)
ptstat <- function(.df,
                   day = NULL,
                   freq = NULL,
                   freq_e = NULL,
                   phase = NULL,
                   date = NULL,
                   date_zero = NULL,
                   count = NULL,
                   count_e = NULL,
                   time = NULL,
                   record_floor = NULL,
                   behavior_floor = NULL,
                   record_ceil = NULL,
                   verbose = TRUE
                   ) {

  .count <- .record_ceil <- .count_e <- .behavior_floor <- .record_floor <- .date <- .day <- .freq <- .freq_e <- .phase <- .time <- b0 <- b1 <- b_down <- b_total <- b_up <- cel <- first_day <- first_freq <- lag_freq <- lead_day <- new_freq <- turn <- jump <-  NULL
  cel_e <- accu <- first_day_e <- b0_e <- b1_e <- lead_day_e <- new_freq_e <- first_freq_e <- lag_freq_e <- b_up_e <- b_down_e <- b_total_e <- turn_e <- jump_e <- last_col <- NULL
  # First scenario
  # date, count, time are provided

  if (any(names(.df) %in% c(".day", ".freq", ".phase", ".date", ".count",
                            ".time", ".count_e", ".freq_e", ".record_floor", ".behavior_floor",
                            ".record_ceil"))) {
    stop("These are reserved colnames: .day, .freq, .phase,
         .date, .count, .time, .count_e, .freq_e, .record_floor and .record_ceil.",
         call. = FALSE)
  }

  main_df <- .df |>
    dplyr::mutate(.day = if (is.null({{ day }})) NA_integer_ else {{ day }},
                  .freq = if (is.null({{ freq }})) NA_real_ else {{ freq }},
                  .phase = if (is.null({{ phase }})) NA else {{ phase }},
                  .date = if (is.null({{ date }})) NA_real_ else {{ date }},
                  .count = if (is.null({{ count }})) NA_integer_ else {{ count }},
                  .time = if (is.null({{ time }})) NA_real_ else {{ time }},
                  .count_e = if (is.null({{ count_e }})) NA_integer_ else {{ count_e }},
                  .freq_e = if (is.null({{ freq_e }})) NA_real_ else {{ freq_e }},
                  .record_floor = if (is.null({{ record_floor }})) NA else {{ record_floor }},
                  .behavior_floor = if (is.null({{ behavior_floor }})) NA else {{ behavior_floor }},
                  .record_ceil = if (is.null({{ record_ceil }})) NA else {{ record_ceil }})

  main_df <- main_df |>
    dplyr::select(dplyr::starts_with(".")) |>
    dplyr::rename(day = .day,
           freq = .freq,
           phase = .phase,
           date = .date,
           count = .count,
           time = .time,
           count_e = .count_e,
           freq_e = .freq_e,
           record_floor = .record_floor,
           behavior_floor = .behavior_floor,
           record_ceil = .record_ceil)


  # Scenario conditions --------------------------------------------------------
  ## Dates and day priority ----
  if (is_provided(main_df$date) && !is.null(date_zero)){
    main_df$day <- date_to_day(main_df$date, date_zero)
    if (verbose == TRUE) cli::cli_alert_info("`date` and `date_zero` are used to calculate `day`")

   } else if (is_provided(main_df$date) && is.null(date_zero)) {
    date_zero <- first_sunday(main_df$date)
    main_df$day <- date_to_day(main_df$date, date_zero)

    if (verbose == TRUE) cli::cli_alert_info("`date` is used to calculate `date_zero`.")
    if (verbose == TRUE) cli::cli_alert_info("`date` and calculated `date_zero` are used to calculate `day`.")

  } else if (is_provided(main_df$day)) {
    if (verbose == TRUE) cli::cli_alert_info("`day` is used to calculate `day`.")

 } else {
    cli::cli_abort("One of these combinaisons must be supplied: `day`, `date`, or `date` and `date_zero`.")
  }


  ## Count and time, and freq priority ----
  if (is_provided(main_df$count) && is_provided(main_df$time)) {
    if (is_provided(main_df$freq)) {
      cli::cli_warn("Provided `freq` is dropped since `count` and `time` are supplied.")
    }
    main_df$freq <- count_to_freq(main_df$count, main_df$time)
    if (verbose == TRUE) cli::cli_alert_info("`count` and `time` are used to calculate `freq`.")

  } else if (is_provided(main_df$freq) && is_provided(main_df$time)){
    main_df$freq <- main_df$freq
    main_df$count <- freq_to_count(main_df$freq, main_df$time)
    if (verbose == TRUE) cli::cli_alert_info("`freq` and `time` are used to calculate `count`.")

    # Warning if count is provided but time is missing when freq is also missing.
  } else if (is_provided(main_df$count) && is_missing(main_df$time) &&
             is_missing(main_df$freq)
  ) {
    cli::cli_alert_warning("`count` is provided, but not `time`. Returning {.code NA}s.")
  }


  ## Count_e and time, and freq_e priority ----
  if (is_provided(main_df$count_e) && is_provided(main_df$time)) {
    if (is_provided(main_df$freq_e)) {
      cli::cli_warn("Provided `freq_e` is dropped since `count_e` and `time` are supplied.")
    }
    main_df$freq_e <- count_to_freq(main_df$count_e, main_df$time)
    if (verbose == TRUE) cli::cli_alert_info("i `count_e` and `time` are used to calculate `freq_e`.")

  } else if (is_provided(main_df$freq_e) && is_provided(main_df$time)){
    main_df$freq_e <- main_df$freq_e
    main_df$count_e <- freq_to_count(main_df$freq_e, main_df$time)
    if (verbose == TRUE) cli::cli_alert_info("i `freq_e` and `time` are used to calculate `count_e`.")

    # Warning if count_e is provided but time is missing when freq_e is also missing.
  } else if (is_provided(main_df$count_e) && is_missing(main_df$time) &&
             is_missing(main_df$freq_e)) {
    cli::cli_alert_warning("`count_e` is provided, but not `time`. Returning {.code NA}s.")
  }



  ## Error if count, freq, count_e or freq_e are missing ----
  if (
    is_missing(main_df$count) && is_missing(main_df$freq) && is_missing(main_df$count_e) && is_missing(main_df$freq_e)
  ) {
    cli::cli_abort("One of these arguments must be supplied: `count`, `freq`, `count_e`, or `freq_e`.")
  }


  # Recoding algorithm for frequency with zero ---------------------------------

  ## freq ----
  if (is_provided(main_df$freq) && any(main_df$freq == 0)) {
    cli::cli_alert_warning("At least one zero is found in `freq`.")

    if (is_provided(main_df$time)) {
      cli::cli_alert_warning("Recoding zero to \u00f72 under record floor.")
    }

    if (is_missing(main_df$time)) {
      cli::cli_alert_warning("`time` is missing. Zeros are recoded to NAs.")
    }

    main_df <- main_df |>
      dplyr::mutate(freq = recode_zero_freq(freq, time))

  }


  ## freq_e ----
  if (is_provided(main_df$freq_e) && any(main_df$freq_e == 0)) {
    cli::cli_alert_warning("At least one zero is found in `freq_e`:")

    if (is_provided(main_df$time)) {
      cli::cli_ul("Recoding zero to \u00f72 under record floor.")
    }

    if (is_missing(main_df$time)) {
      cli::cli_ul("`time` is missing. Zeros are recoded to NAs.")
    }

    main_df <- main_df |>
      dplyr::mutate(freq_e = recode_zero_freq(freq_e, time))

  }



  # Main computations ----------------------------------------------------------

  main_df <- main_df |>
    dplyr::group_by(phase) |>
    dplyr::mutate(
      log_freq = log10(freq),
      log_freq_e = log10(freq_e),
      accu_ratio = accuracy_ratio(day, freq, freq_e),
      record_floor = record_floor(time),
      res_freq = res(day, freq),
      pred_freq = predicted_values(day, freq),
      res_freq_e = res(day, freq_e),
      pred_freq_e = predicted_values(day, freq_e)
      )

  # For testing only
  #outside_main_df <<- main_df

  pt_measures <- main_df |>
    dplyr::group_by(phase) |>
    dplyr::summarise(
      b0 = b0(day, freq),
      b1 = b1(day, freq),
      cel0 = celeration_0(day, freq),
      cel = celeration(day, freq),
      b_up = bounce_up(day, freq),
      b_down = bounce_down(day, freq),
      b_total = bounce_total(day, freq),
      first_day = dplyr::first(day),
      first_freq = dplyr::first(freq),

      b0_e = b0(day, freq_e),
      b1_e = b1(day, freq_e),
      cel0_e = celeration_0(day, freq_e),
      cel_e = celeration(day, freq_e),
      b_up_e = bounce_up(day, freq_e),
      b_down_e = bounce_down(day, freq_e),
      b_total_e = bounce_total(day, freq_e),
      first_day_e = dplyr::first(day),
      first_freq_e = dplyr::first(freq_e),

      accu = accuracy(day, freq, freq_e),



      ) |>
    dplyr::mutate(turn = dplyr::lag(cel) / cel,
                  turn_e = dplyr::lag(cel_e) / cel_e,
                  .before = accu)

  pt_measures <- pt_measures |>
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
      jump = 10^log10(first_freq) / 10^log10(lag_freq),

      lead_day_e = dplyr::lead(first_day_e),
      new_freq_e = 10^b0_e * 10^(b1_e * lead_day_e),
      lag_freq_e = dplyr::lag(new_freq_e),
      jump_e = 10^log10(first_freq_e) / 10^log10(lag_freq_e),
      .before = accu
    ) |>

    dplyr::select(!c(first_day, first_freq, lead_day, new_freq, lag_freq,
                     first_day_e, first_freq_e, lead_day_e,  new_freq_e, lag_freq_e)) |>

    dplyr::mutate(
      cel = ptvalue::as_ptvalue(cel),
      b_up = ptvalue::as_ptvalue(b_up),
      b_down = ptvalue::as_ptvalue(b_down),
      b_total = ptvalue::as_ptvalue(b_total),
      turn = ptvalue::as_ptvalue(turn),
      jump = ptvalue::as_ptvalue(jump),

      cel_e = ptvalue::as_ptvalue(cel_e),
      b_up_e = ptvalue::as_ptvalue(b_up_e),
      b_down_e = ptvalue::as_ptvalue(b_down_e),
      b_total_e = ptvalue::as_ptvalue(b_total_e),
      turn_e = ptvalue::as_ptvalue(turn_e),
      jump_e = ptvalue::as_ptvalue(jump_e)
    ) |>
    dplyr::relocate(phase, !dplyr::ends_with("_e"), dplyr::ends_with("_e")) |>
    dplyr::relocate(accu, .after = last_col())


  #basic_print <- pt_measures |>
    #tidyr::pivot_longer(cols = c(b0, b0_e), names_to = "b0")

  acceleration <- pt_measures |>
    dplyr::select(phase, !dplyr::ends_with("_e"), accu)

  #print(acceleration)

  deceleration <- pt_measures |>
    dplyr::select(phase, dplyr::ends_with("_e"), accu)

  #print(deceleration)



  ptstat_list <- list(
    main_df = main_df,
    pt_measures = pt_measures,
    acceleration = acceleration,
    deceleration = deceleration
  )

  #print(ptstat_list)

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
  #cat("PT Tables", sep = "\n")
  print(x$pt_measures, ...)
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



is_missing <- function(x) {
  all(is.na(x))
  }

is_provided <- function(x) {
  !all(is.na(x))
}

