# Packages required ####
library(dplyr)
library(lubridate)

# Dependencies:
source("package_en_developpement/R/pt_frequency.R")
source("package_en_developpement/R/pt_time_floor.R")

format_data <- function(data, date, count, time, phase, date_zero) {

  # Data type checking
  stopifnot(
            class(data %>% select({{ date }}) %>% pull()) == "Date",
            class(data %>% select({{ count }}) %>% pull()) == "numeric",
            class(data %>% select({{ time }}) %>% pull()) == "numeric",
            class({{ date_zero }}) == "Date"
            )

  # Format data
  data <- data %>%
    transmute(
      date = {{ date }},
      day = as.integer({{ date }} - {{ date_zero }}),
      count = {{ count }},
      time = {{ time }},
      timefloor = pt_time_floor({{ time }}),
      frequency = pt_frequency({{ count }}, {{ time }}),
      log10freq = log10(frequency),
      phase = forcats::as_factor({{ phase }})
    )

  # Ordering by date
  data <- data %>% arrange(date)

  return(data)
}
