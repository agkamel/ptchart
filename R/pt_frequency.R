#' @title Compute frequency
#'
#' @param count Integer. The number of response.
#' @param time Numeric. The number of minute.
#' @param rule Rule to apply when the number of response is zero. The Neely rule is applied by default.
#'
#' @return A numeric vector.
#'
#' @examples #pt_frequency()
#' @import dplyr
#' @export
pt_frequency <- function(count, time, rule = log10(2)) {
  dplyr::case_when(
    count == 0 ~ .frequency_if_zero_count(time, rule = rule),
    TRUE       ~ count / time
  )
}

.frequency_if_zero_count <- function(time, rule = log10(2)){
  # Fonction helper .frequency_if_zero_count ####
  #    Selon la suggestion de l'article de Neely
  antilog(log10(pt_time_floor(time)) - rule)
}
