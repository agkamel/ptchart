# Packages required ####
library(dplyr)
library(lubridate)

# Function
format_data <- function(data, date, count, time, date_zero) {
  data_raw %>%
    transmute(
      date = {{ date }},
      day = as.integer({{ date }} - as.Date({{ date_zero }})),
      count = {{ count }},
      time = {{ time }},
      frequency = {{ count }}/{{ time }},
      log10freq = log10(frequency)
    )
  
  return(format_data)
}
