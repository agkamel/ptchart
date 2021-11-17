data_example <- readr::read_rds("r/example_data.rds")
first_sunday <- readr::read_rds("r/example_first_sunday.rds")

library(dplyr)
data_phase_a <- data_example %>% filter(phase == "A")


# Fonction date_to_day()
date_to_day <- function(date, date_zero){
  as.integer(date - as.Date(date_zero))
}

date_to_day(data_example$date, as.Date(first_sunday))


celeration <- function(data, 
                       date, 
                       count,
                       time,
                       date_zero = NULL
                       ){
  
  # Fonction 1
  data <- data %>% select(date = date, count = count, time = time)
  
  #data %>% mutate(day = date_to_day(date, date_zero))
  
}
celeration(data_phase_a, date = jour, count = reponse, time = minute, date_zero = first_sunday)



