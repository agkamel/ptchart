calculate_freq <- function(count, time
                           #, rule = log10(2)
                           ) {
  count / time

  #dplyr::case_when(
  #  count == 0 ~ freq_if_zero_count(time, rule = rule),
  #  TRUE       ~ count / time
  #)
}

calculate_count <- function(freq, time) {
  freq * time
}


recode_freq_when_zero <- function(freq, time, rule = log10(2)) {
  dplyr::case_when(
    freq < calculate_time_floor(time) ~ antilog(log10(calculate_time_floor(time)) - rule),
    TRUE                              ~ freq
  )
}





# log10(2) est utilisé selon la recommandation de l'article de Neely
freq_if_zero_count <- function(time, rule = log10(2)){
  antilog(log10(calculate_time_floor(time)) - rule)
}


