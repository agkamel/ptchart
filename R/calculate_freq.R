calculate_freq <- function(count, time, rule = log10(2)) {
  dplyr::case_when(
    count == 0 ~ freq_if_zero_count(time, rule = rule),
    TRUE       ~ count / time
  )
}


# log10(2) est utilisé selon la recommandation de l'article de Neely
freq_if_zero_count <- function(time, rule = log10(2)){
  antilog(log10(calculate_time_floor(time)) - rule)
}


calculate_time_floor <- function(time) {
  1 / time
}
