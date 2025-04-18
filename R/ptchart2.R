#' ptchart2
#'
#' @param object An object of class `ptstat`.
#' @param zoom_x A vector of class `Date` and of length 2 for setting limits for the x axis.
#' @param zoom_y A numerical vector of length 2 for setting limits for the y axes.
#'
#' @param title A character vector of length 1 for setting the title.
#' @param xlab Character vector of length 1. Label of x-axis (default: `"Day"`).
#' @param ylab Character vector of length 1. Label of y-axis (default: `"Rate"`).
#'
#' @param show_record_floor Logical vector of length 1. Is recording floor showed (default: `TRUE`) or not (`FALSE`)?
#' @param show_behavior_floor Logical vector of length 1. Is behavior floor showed (default: `TRUE`) or not (`FALSE`)?
#' @param show_record_ceil Logical vector of length 1. Is recording ceiling showed (default: `TRUE`) or not (`FALSE`)?
#'
#' @param show_acc_line Logical vector of length 1. Is acceleration line showed (default: `TRUE`) or not (`FALSE`)?
#' @param show_dec_line Logical vector of length 1. Is decerelation line showed (default: `TRUE`) or not (`FALSE`)?
#' @param show_accuracy_line Logical vector of length 1. Is accuracy line showed (default: `TRUE`) or not (`FALSE`)?
#'
#' @param show_acc_bounce_lines Logical vector of length 1. Are bounce lines around acceleration showed (default: `TRUE`) or not (`FALSE`)?
#' @param show_dec_bounce_lines Logical vector of length 1. Are bounce lines around deceleration showed (default: `TRUE`) or not (`FALSE`)?
#'
#' @param show_acc_point Logical vector of length 1. Are acceleration data points showed (default: `TRUE`) or not (`FALSE`)?
#' @param show_dec_point Logical vector of length 1. Are deceleration data points showed (default: `TRUE`) or not (`FALSE`)?
#' @param show_accuracy_point Logical vector of length 1. Are accuracy data points showed (default: `TRUE`) or not (`FALSE`)?
#'
#' @param color_acc_line Character vector of length 1. Color HEX values for acceleration line (default: `"#7CAE00"`).
#' @param color_dec_line Character vector of length 1. Color HEX values for deceleration line (default: `"#F8766D"`).
#' @param color_accuracy_line Character vector of length 1. Color HEX values for accuracy line (default: `"#00BFC4"`).
#'
#' @return A precision teaching chart.
#' @export
#'
#' @examples
#' #TODO
ptchart2 <- function(object,
                    zoom_x = NULL,
                    zoom_y = NULL,
                    title = "ptchart output",
                    xlab = "Day",
                    ylab = "Rate",

                    show_record_floor = TRUE,
                    show_behavior_floor = TRUE,
                    show_record_ceil = TRUE,
                    show_acc_line = TRUE,
                    show_dec_line = TRUE,
                    show_accuracy_line = TRUE,
                    show_acc_bounce_lines = FALSE,
                    show_dec_bounce_lines = FALSE,

                    show_acc_point = TRUE,
                    show_dec_point = TRUE,
                    show_accuracy_point = TRUE,

                    color_acc_line = "#7CAE00",
                    color_dec_line = "#F8766D",
                    color_accuracy_line = "#00BFC4"
) {

  # To prevent note of "no visible binding for global variable 'x'" when building the package
  day <- accu_ratio <- res_freq <- res_freq_err <- freq <- freq_err <- phase <- record_floor <- record_ceil <- behavior_floor <- NULL

  stopifnot("`object` must be of class `ptstat`" = is_ptstat(object))

  main_df <- object[["main_df"]]

  scale_x_params <- make_scale_x_params2(main_df)
  scale_y_params <- make_scale_y_params2()

  gg_scale_y_log10 <- ggplot2::scale_y_log10(
      name = ylab,
      breaks = scale_y_params[["frequency"]][(scale_y_params[["is_y_breaks"]])],
      labels = scale_y_params[["frequency"]][(scale_y_params[["is_y_breaks"]])],
      minor_breaks = scale_y_params[["frequency"]],
      limits = c(1/(24*60), 1000),
      sec.axis = ggplot2::sec_axis(
        trans = ~.,
        breaks = c(.001, .002, .005, .01, .02, .05, .1, .2, .5, 1, 2, 3, 4, 6),
        labels = c("1000'", "500'", "200'", "100'", "50'", "20'", "10'", "5'",
                   "2'", "1'", "30\"", "20\"", "15\"", "10\""),
        name = ""
      )
    )


  gg_scale_x_continuous <- ggplot2::scale_x_continuous(
    name = xlab,
    breaks = scale_x_params[["no_date"]][(scale_x_params[["breaks"]])],
    labels = scale_x_params[["labels"]][!is.na(scale_x_params[["labels"]])],
    minor_breaks = scale_x_params[["no_date"]],
    limits = c(scale_x_params[["no_date"]][[1]],
               scale_x_params[["no_date"]][[length(scale_x_params[["no_date"]])]])
  )

  gg_title <- ggplot2::ggtitle(title)

  gg_theme <- ggplot2::theme(
    plot.title = ggplot2::element_text(hjust = 0.5, size = 20),
    panel.grid.major = ggplot2::element_line(colour = "#00b1d9"),
    panel.grid.minor = ggplot2::element_line(colour = "#66d1e8"),
    text = ggplot2::element_text(family = "serif", size = 12)
    #aspect.ratio = 5.44 / 8, # 5 7/16 de pouce par 8 pouce
    #https://jweshleman.wordpress.com/2006/03/25/og-on-standard-celeration-charting-system-standards/
  )

  gg_coord_fixed <- ggplot2::coord_fixed(
    expand = FALSE,
    #ratio = 1/1, # Voir note sur le ratio ci-bas, explication 1
    #ratio = 7/1, # Voir note sur le ratio ci-bas, explication 2
    #ratio = 7/log10(2), # 0.30103 voir note sur le ratio ci-bas, explication 3
    ratio = 7/(log10(2)/tan(34*pi/180)), # 0.30105 / 0.6745 # Voir note sur le ratio ci-bas, explication 4

    # ZOOM sur le graphique
    xlim = zoom_x,
    ylim = zoom_y
  )

  # Ligne séparant les phases
  gg_geom_vline <-  ggplot2::geom_vline(
      xintercept = lubridate::ymd("2021-08-01")+0.5
    )

  output <- ggplot2::ggplot(
    data = main_df
  ) +
    gg_scale_y_log10 +
    gg_scale_x_continuous +
    gg_title +
    gg_theme +
    gg_coord_fixed +
    gg_geom_vline










  if (is_missing(main_df$date)) {


    if (show_record_floor) {

      output <- output +

        # Recording floor
        ggplot2::geom_point(
          mapping = ggplot2::aes(x = day, y = record_floor),
          shape = "\u2013",
          size = 5,
          color = "gray30"
        )

    }


    if (show_behavior_floor) {

      output <- output +

      # Behavior floor
      ggplot2::geom_point(
        mapping = ggplot2::aes(x = day, y = behavior_floor),
        shape = "\u2012", size = 5, color = "gray20"
      )
    }


    if (show_record_ceil) {

      output <- output +

      # Record ceil
      ggplot2::geom_point(
        mapping = ggplot2::aes(x = day, y = record_ceil),
        shape = "\u2013", size = 5, color = "gray10"
      )

    }


    if (show_acc_line) {

      output <- output +

      #Pente de régression
      ggplot2::geom_smooth(
        method = "lm",
        se = FALSE,
        mapping = ggplot2::aes(x = day, y = freq, group = phase),
        color = color_acc_line
      )


    }

    if (show_dec_line) {

      output <- output +

      ggplot2::geom_smooth(
        method = "lm",
        se = FALSE,
        mapping = ggplot2::aes(x = day, y = freq_err, group = phase),
        color = color_dec_line
      )
    }


    if (show_accuracy_line) {

      output <- output +

        #Pente de régression
        ggplot2::geom_smooth(
          method = "lm",
          se = FALSE,
          mapping = ggplot2::aes(x = day, y = accu_ratio, group = phase),
          color = color_accuracy_line
        )


    }


    if (show_acc_bounce_lines) {

      output <- output +

        #Pente de régression
        ggplot2::geom_smooth(
          method = "lm",
          se = FALSE,
          mapping = ggplot2::aes(x = day, y = freq * 10^max(res_freq), group = phase),
          color = color_acc_line,
          linetype = 2
        ) +
        ggplot2::geom_smooth(
          method = "lm",
          se = FALSE,
          mapping = ggplot2::aes(x = day, y = freq * 10^min(res_freq), group = phase),
          color = color_acc_line,
          linetype = 2
        )


    }

    if (show_dec_bounce_lines) {

      output <- output +

        #Pente de régression
        ggplot2::geom_smooth(
          method = "lm",
          se = FALSE,
          mapping = ggplot2::aes(x = day, y = freq_err * 10^max(res_freq_err), group = phase),
          color = color_dec_line,
          linetype = 2
        ) +
        ggplot2::geom_smooth(
          method = "lm",
          se = FALSE,
          mapping = ggplot2::aes(x = day, y = freq_err * 10^min(res_freq_err), group = phase),
          color = color_dec_line,
          linetype = 2
        )


    }




    if (show_acc_point) {

      output <- output +

      #Point de fréquence cible et non-cible
      ggplot2::geom_point(
        mapping = ggplot2::aes(x = day, y = freq),
        shape = 16#, size = 2
      )

    }


    if (show_dec_point) {

      output <- output +

      ggplot2::geom_point(
        mapping = ggplot2::aes(x = day, y = freq_err),
        shape = 4, size = 2.5,
      )
    }

    if (show_accuracy_point) {

      output <- output +

        ggplot2::geom_point(
          mapping = ggplot2::aes(x = day, y = accu_ratio),
          shape = 2, size = 2.5,
        )
    }

  } else {



      # Plancher d'enregistrement
      #ggplot2::geom_segment(x = ymd("2021-07-20")-0.5,
      #             xend = ymd("2021-07-20")+0.5,
      #             y = log10(1),
      #             yend = log10(1)
      #             ) +


    if (show_record_floor) {
      output <- output +

        # Record floor
        ggplot2::geom_point(
          mapping = ggplot2::aes(x = date, y = record_floor),
          shape = "\u2013",
          size = 5,
          color = "gray30"
        )
    }

    if (show_behavior_floor) {

      output <- output +

      # Behavior floor
      ggplot2::geom_point(
        mapping = ggplot2::aes(x = date, y = behavior_floor),
        shape = "\u2012", size = 5, color = "gray20"
      )
    }

    if (show_record_ceil) {

      output <- output +

      # Record ceil
      ggplot2::geom_point(
        mapping = ggplot2::aes(x = date, y = record_ceil),
        shape = "\u2013", size = 5, color = "gray10"
      )

    }


    if (show_acc_line) {

      output <- output +
      #Pente de régression
      ggplot2::geom_smooth(
        method = "lm",
        se = FALSE,
        mapping = ggplot2::aes(x = date, y = freq, group = phase),
        color = color_acc_line
      )
    }

    if (show_dec_line) {
      output <- output +

      ggplot2::geom_smooth(
        method = "lm",
        se = FALSE,
        mapping = ggplot2::aes(x = date, y = freq_err, group = phase),
        color = color_dec_line
      )
    }


    if (show_accuracy_line) {

      output <- output +

        #Pente de régression
        ggplot2::geom_smooth(
          method = "lm",
          se = FALSE,
          mapping = ggplot2::aes(x = date, y = accu_ratio, group = phase),
          color = color_accuracy_line
        )


    }


    if (show_acc_point) {

      output <- output +
      #Point de fréquence cible et non-cible
      ggplot2::geom_point(
        mapping = ggplot2::aes(x = date, y = freq),
        shape = 16#, size = 2
      )
    }

    if (show_dec_point) {

      output <- output +

      ggplot2::geom_point(
        mapping = ggplot2::aes(x = date, y = freq_err),
        shape = 4, size = 2.5,
      )
    }

    if (show_accuracy_point) {

      output <- output +

        ggplot2::geom_point(
          mapping = ggplot2::aes(x = date, y = accu_ratio),
          shape = 2, size = 2.5,
        )
    }




    # Annotation
    #  ggplot2::annotate("text",
    #                    x = lubridate::ymd("2021-07-24"),
    #                    y = 50, label = paste0("\u00d7", round(celeration(object)$c[1], 2)))




    #geom_abline(slope = object[["terms"]][["b1"]][[1]],
    #           intercept = object[["terms"]][["b0"]][[1]])

  }


  output
}


make_scale_y_params2 <- function() {
  # To prevent note of "no visible binding for global variable 'x'" when building the package
  base <- exponent <- sub_unit <- NULL

  tibble::tibble(
    base = 10,
    exponent = c(-4, -4, -4, -4, rep(c(-3, -2, -1, 0, 1, 2), each = 9), 3),
    base_to_exponent = base ^ exponent,
    sub_unit = c((1 / 1440) / (10 ^ -4), 7:9, rep(1:9, times = 6), 1),
    frequency = base ^ exponent * sub_unit,
    is_y_breaks = dplyr::case_when(sub_unit == 1 |
                                     sub_unit == 5 ~ TRUE,
                                   TRUE ~ FALSE)
  )
}


make_scale_x_params2 <- function(main_df) {

  #if(is.character(first_sunday)) {first_sunday <- ymd(first_sunday)}

  # Vérification
  #stopifnot(
  #  is.na(first_sunday) == FALSE,
  #  wday(first_sunday, week_start = 1) == 7
  #)

  if (is_missing(main_df$date)) {
    first_sunday <- as.Date(0)
  } else {
    first_sunday <- first_sunday(main_df$date)
  }


  # Création de la base de données pour ggplot
  tibble::tibble(
    date = seq.Date(
      from = first_sunday,
      by = "day",
      length.out = 141 # Because the first sunday is 0, not 1
    ),
    no_date = seq(from = 0, to = 140),
    breaks = dplyr::case_when(
      no_date %% 7 == 0 ~ TRUE,
      TRUE ~ FALSE
    ),
    labels = dplyr::case_when(
      no_date %% 14 == 0 ~ as.character(no_date),
      no_date %% 7 == 0 ~ "",
      TRUE ~ NA_character_
    ),
    sec_axis_breaks = dplyr::case_when(
      no_date %% 28 == 0 ~ 1,
      TRUE ~ 0
    ),
    sec_axis_labels = dplyr::case_when(
      no_date %% 28 == 0 ~ no_date / 7,
      TRUE ~ NA_real_
    )
  )
}

first_date2 <- function(date){
  date[[1]]
}


last_date2 <- function(date){
  date[[length(date)]]
}

