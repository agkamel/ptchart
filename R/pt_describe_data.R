#' Describe summary statistics
#'
#' @param data_formated A data frame.
#' @param day A integer vector.
#' @param log10freq A numeric vector.
#'
#' @return A dataframe
#' @import dplyr
#' @export
#'
#' @examples # pt_descriptive_data()
pt_describe_data <- function(data_formated, day, log10freq) {
  data_formated %>%
    summarise(
      n = n(),
      day_mean = mean(day),
      day_sd = sd(day),
      log10freq_mean = mean(log10freq),
      log10freq_sd = sd(log10freq)
    )
}
