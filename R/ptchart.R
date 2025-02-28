#' ptchart
#'
#' @param object An object of class `ptstat`.
#' @param zoom_x A vector of class `Date` and of length 2 for setting limits for the x axis.
#' @param zoom_y A numerical vector of length 2 for setting limits for the y axes.
#' @param title A character vector of length 1 for setting the title.
#'
#' @return A precision teaching chart.
#' @export
#'
#' @examples
#' #TODO
ptchart <- function(object,
                    zoom_x = NULL,
                    zoom_y = NULL,
                    title = "ptchart output"
                    ) {

  # To prevent note of "no visible binding for global variable 'x'" when building the package
  day <- freq <- freq_err <- phase <- time_floor <- count_ceil <- count_floor <- NULL


  stopifnot("`object` must be of class `ptstat`" = is_ptstat(object))

  scale_x_params <- make_scale_x_params(object)
  scale_y_params <- make_scale_y_params()

  output <- ggplot2::ggplot(
    data = extract_pttable(object)
    ) +


    ggplot2::scale_y_log10(
      name = "Nombre de comportement par minute",
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
      ) +

    ggplot2::scale_x_continuous(
      name = "Jour cons\u00e9cutif du calendrier",
      breaks = scale_x_params[["no_date"]][(scale_x_params[["breaks"]])],
      labels = scale_x_params[["labels"]][!is.na(scale_x_params[["labels"]])],
      minor_breaks = scale_x_params[["no_date"]],
      limits = c(scale_x_params[["no_date"]][[1]],
                 scale_x_params[["no_date"]][[length(scale_x_params[["no_date"]])]])
      ) +

    ggplot2::ggtitle(title) +

    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, size = 20),
      panel.grid.major = ggplot2::element_line(colour = "#00b1d9"),
      panel.grid.minor = ggplot2::element_line(colour = "#66d1e8"),
      text = ggplot2::element_text(family = "serif", size = 12)
      #aspect.ratio = 5.44 / 8, # 5 7/16 de pouce par 8 pouce
      #https://jweshleman.wordpress.com/2006/03/25/og-on-standard-celeration-charting-system-standards/
      ) +

    ggplot2::coord_fixed(
      expand = FALSE,
      #ratio = 1/1, # Voir note sur le ratio ci-bas, explication 1
      #ratio = 7/1, # Voir note sur le ratio ci-bas, explication 2
      #ratio = 7/log10(2), # 0.30103 voir note sur le ratio ci-bas, explication 3
      ratio = 7/(log10(2)/tan(34*pi/180)), # 0.30105 / 0.6745 # Voir note sur le ratio ci-bas, explication 4

      # ZOOM sur le graphique
      xlim = zoom_x,
      ylim = zoom_y
      ) +

    # Ligne séparant les phases
    ggplot2::geom_vline(
      xintercept = lubridate::ymd("2021-08-01")+0.5
      )

  if (is.na(object$arg_table$date)) {

    output +

    # Time floor
    ggplot2::geom_point(
      mapping = ggplot2::aes(x = day, y = time_floor),
      shape = "\u2013", size = 5, color = "gray30"
    ) +

      # Count floor
      ggplot2::geom_point(
        mapping = ggplot2::aes(x = day, y = count_floor),
        shape = "\u2012", size = 5, color = "gray20"
      ) +

      # Count ceil
      ggplot2::geom_point(
        mapping = ggplot2::aes(x = day, y = count_ceil),
        shape = "\u2013", size = 5, color = "gray10"
      ) +






      #Pente de régression
      ggplot2::geom_smooth(
        method = "lm",
        se = FALSE,
        mapping = ggplot2::aes(x = day, y = freq, group = phase)
      ) +

      ggplot2::geom_smooth(
        method = "lm",
        se = FALSE,
        mapping = ggplot2::aes(x = day, y = freq_err, group = phase), color = "red"
      ) +

      #Point de fréquence cible et non-cible
      ggplot2::geom_point(
        mapping = ggplot2::aes(x = day, y = freq),
        shape = 16#, size = 2
      ) +

      ggplot2::geom_point(
        mapping = ggplot2::aes(x = day, y = freq_err),
        shape = 4, size = 2.5,
      )



  } else {


  output +

    # Plancher d'enregistrement
    #ggplot2::geom_segment(x = ymd("2021-07-20")-0.5,
    #             xend = ymd("2021-07-20")+0.5,
    #             y = log10(1),
    #             yend = log10(1)
    #             ) +

    # Time floor
    ggplot2::geom_point(
      mapping = ggplot2::aes(x = date, y = time_floor),
      shape = "\u2013", size = 5, color = "gray30"
    ) +

    # Count floor
    ggplot2::geom_point(
      mapping = ggplot2::aes(x = date, y = count_floor),
      shape = "\u2012", size = 5, color = "gray20"
    ) +

    # Count ceil
    ggplot2::geom_point(
      mapping = ggplot2::aes(x = date, y = count_ceil),
      shape = "\u2013", size = 5, color = "gray10"
    ) +






  #Pente de régression
    ggplot2::geom_smooth(
    method = "lm",
    se = FALSE,
    mapping = ggplot2::aes(x = date, y = freq, group = phase)
    ) +

    ggplot2::geom_smooth(
      method = "lm",
      se = FALSE,
      mapping = ggplot2::aes(x = date, y = freq_err, group = phase), color = "red"
    ) +

  #Point de fréquence cible et non-cible
    ggplot2::geom_point(
      mapping = ggplot2::aes(x = date, y = freq),
      shape = 16#, size = 2
    ) +

    ggplot2::geom_point(
      mapping = ggplot2::aes(x = date, y = freq_err),
      shape = 4, size = 2.5,
    )# +




  # Annotation
  #  ggplot2::annotate("text",
  #                    x = lubridate::ymd("2021-07-24"),
  #                    y = 50, label = paste0("\u00d7", round(celeration(object)$c[1], 2)))




  #geom_abline(slope = object[["terms"]][["b1"]][[1]],
  #           intercept = object[["terms"]][["b0"]][[1]])

  }
}


make_scale_y_params <- function() {
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


make_scale_x_params <- function(object) {

  #if(is.character(first_sunday)) {first_sunday <- ymd(first_sunday)}

  # Vérification
  #stopifnot(
  #  is.na(first_sunday) == FALSE,
  #  wday(first_sunday, week_start = 1) == 7
  #)

  if (is.na(object$arg_table$date)) {
    first_sunday <- as.Date(0)
  } else {
    first_sunday <- first_sunday(extract_pttable(object)[["date"]])
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

first_date <- function(date){
  date[[1]]
}


last_date <- function(date){
  date[[length(date)]]
}



