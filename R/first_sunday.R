first_sunday <- function(date){
  seven_previous_dates <- min(date) - lubridate::days(0:6)
  seven_previous_dates[is_sunday(seven_previous_dates)]
}

is_sunday <- function(date) {
  lubridate::wday(date, week_start = 1) == 7
}
