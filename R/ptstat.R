# Fonction pour calculer la régression linéaire ####
#' Calculate measures of behavioral changing
#'
#' This a description that will be added here.
#'
#' @param data A data.frame
#' @param date Date of observations
#' @param count Count of observations
#' @param time Time of observations
#' @param date_zero Date of the first sunday
#' @param log_freq Logical. Are frequencies logged on base 10 ?
#'
#' @return A list
#' @export
#'
#' @examples #ptstat(date, date, count, time, date_zero, phase)
#' @import dplyr
ptstat <- function(data,
                   date,
                   count,
                   time,
                   date_zero,
                   log_freq = TRUE#,
                   # phase = NULL
) {
  SCR <- SCReg <- b0 <- b1 <- bounce_down <- bounce_up <- day <- day_deviation <- day_deviation_squared <- day_mean <- error <- freq_deviation <- freq_deviation_squared <- freq_mean <- freq_predicted <- frequency <- log10freq <- n <- s2 <- sum_day_dev_squared <- sum_day_dev_times_freq_dev <- sum_freq_dev_squared <- temp_freq <- NULL
  data_raw <-
    data %>%
    dplyr::transmute(
      date = {{ date }},
      day = as.integer({{ date }} - as.Date({{ date_zero }})),
      count = {{ count }},
      time = {{ time }},
      frequency = {{ count }}/{{ time }},
      log10freq = log10(frequency),
      #phase = if (is.null({{ phase }}) == FALSE) { ({{ phase }}) } else {"a"},
      temp_freq = if(log_freq == TRUE) {log10freq} else if (log_freq == FALSE) {frequency}
    )


  data_raw <- data_raw %>%
    #group_by(phase) %>%
    dplyr::mutate(
      # Regression step
      day_deviation = day - base::mean(day, na.rm = TRUE),
      day_deviation_squared = day_deviation^2,
      freq_deviation = temp_freq - base::mean(temp_freq, na.rm = TRUE),
      freq_deviation_squared = freq_deviation^2,
      day_dev_times_freq_dev = day_deviation * freq_deviation
    )

  data_descriptive <- data_raw %>%
    #group_by(phase) %>%
    dplyr::summarise(
      # Analyses descriptives
      n = dplyr::n(),
      day_mean = base::mean(day, na.rm = TRUE),
      freq_mean = base::mean(temp_freq, na.rm = TRUE),

      # Regression step
      sum_day_dev_times_freq_dev = base::sum(day_deviation * freq_deviation),
      sum_day_dev_squared = base::sum(day_deviation_squared),
      sum_freq_dev_squared = base::sum(freq_deviation_squared),

      # coefficient droite de régression pour célération
      b1 = sum_day_dev_times_freq_dev / sum_day_dev_squared,
      b0 = freq_mean - (b1 * day_mean),
      n = dplyr::n()
    )

  data_raw <- data_raw %>%
    #group_by(phase) %>%
    dplyr::mutate(
      # Valeurs prédites et erreurs
      freq_predicted = data_descriptive$b0 + data_descriptive$b1*day,
      error = temp_freq - freq_predicted,
      error_squared = error^2,

      # al = antilog
      # antilog des valeurs prédites
      al_freq_predicted = antilog(freq_predicted)
    )

  data_descriptive <- data_descriptive %>%
    dplyr::mutate(
      # Regression step
      SCR = base::sum(data_raw$error_squared),
      s2 = SCR / (n - 2),
      s = base::sqrt(s2),
      SCReg = b1 * sum_day_dev_times_freq_dev,

      # R carré
      r2 = SCReg / sum_freq_dev_squared,

      # Antilog coefficient de célération
      al_b1 = antilog(b1),
      al_b0 = antilog(b0),
      celeration_raw = antilog(b1)^7,
      celeration = if (antilog(b1)^7 >= 1) {antilog(b1)^7} else {(1/antilog(b1))^7},
      cel_type = if (antilog(b1)^7 >= 1) {"Acceleration"} else {"Deceleration"}
    )

  data_bounce <- data_descriptive %>%
    dplyr::transmute(
      bounce_up_b0 = b0 + base::max(data_raw$error),
      bounce_down_b0 = b0 - -base::min(data_raw$error),
      bounce_up_al_b0 = antilog(b0 + base::max(data_raw$error)),
      bounce_down_al_b0 = antilog(b0 - -base::min(data_raw$error)),
      bounce_up = antilog(base::max(data_raw$error)),
      bounce_down = 1/antilog(base::min(data_raw$error)),
      bounce_total = bounce_up * bounce_down
    )

  list(data_raw = data_raw,
       data_descriptive = data_descriptive,
       data_bounce = data_bounce)
}

# Base de données test ####
# Accélération
#my_data_test1 <- tibble(
#  my_date = seq.Date(from = as.Date("2021-07-19"), by = "day", length.out = 13),
#  my_response = c(4, 6, 5, 7, 8, 7, 9, 6, 10, 12, 9, 11, 12),
#  my_time =  c(1, 1, 1, 1, 1, 1, 1, 1,  1,  1, 1, 1,  1),
#  my_phase = c("a", "a", "a", "a", "a", "a", "a", "a",  "a",  "a", "a", "a",  "a")
#)

# Célération presque nulle
#my_data_test2 <- tibble(
#  my_date = seq.Date(from = as.Date("2021-08-02"), by = "day", length.out = 13),
#  my_response = c(10, 12, 11, 8, 8, 11, 12, 14, 11, 10, 12, 10, 12),
#  my_time =  c(1, 1, 1, 1, 1, 1, 1, 1,  1,  1, 1, 1,  1),
#  my_phase = c("b", "b", "b", "b", "b", "b", "b", "b",  "b",  "b", "b", "b",  "b")
#)

# Décélération
#my_data_test3 <- tibble(
#  my_date = seq.Date(from = as.Date("2021-08-16"), by = "day", length.out = 13),
#  my_response = c(12, 11,  9, 12, 9,  7,  8,  6,  7,  7,  5,  6,  4),
#  my_time =  c(1, 1, 1, 1, 1, 1, 1, 1,  1,  1, 1, 1,  1),
#  my_phase = c("c", "c", "c", "c", "c", "c", "c", "c",  "c",  "c", "c", "c",  "c")
#)

#my_data_test <- bind_rows(my_data_test1, my_data_test2)


# Test
#ptstat(my_data_test3, my_date, my_response, my_time, "2021-07-18", log_freq = TRUE)
#ptstat(my_data_test2, my_date, my_response, my_time, "2021-07-18", log_freq = TRUE)[[2]] %>% View(title = "b")



