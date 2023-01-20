calculate_day <- function(date, date_zero) {
  as.integer(date - date_zero)
}

is_sunday <- function(date) {
  lubridate::wday(date, week_start = 1) == 7
}

find_last_sunday <- function(date){
  seven_last_dates <- min(date) - lubridate::days(0:6)
  seven_last_dates[is_sunday(seven_last_dates)]
}

