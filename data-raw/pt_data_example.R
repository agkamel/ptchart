# Code R pour générer des données aléatoires pour `example_pt_data.rda`

# Packages ####
library(dplyr)
library(lubridate)
#library(readr)

# Paramètres initaux ####

# Premier dimanche - Jour zéro
first_sunday <- as.Date("2021-07-18")
example_pt_date_zero <- first_sunday
# Sortie des données
#data_output <- "data-raw/example_pt_data.rda"
#first_sunday_output <- "data-raw/pt_date_zero_example.rda"

# Paramètres - phase A ####

#     Fréquence cible
celeration_a <- 1.4
b1_a <- log10(celeration_a^(1/7))
b0_a <- log10(4)
set.seed(1)
erreur_a <- rnorm(14, mean = 0, sd = .06)[-c(6,7,13,14)] # Sans les fin de semaines
minute_a <- 1

#     Fréquence non-cible
celeration_a_nc <- 0.40 # Cel: /2.5
b1_a_nc <- log10(celeration_a_nc^(1/7))
b0_a_nc <- log10(2)
set.seed(1)
erreur_a_nc <- rnorm(14, mean = 0, sd = .08)[-c(6,7,13,14)]


# Paramètres - phase B ####

#     Fréquence cible
celeration_b <- 0.5  # Cel: /2
b1_b <- log10(celeration_b^(1/7))
b0_b <- log10(80)    # 80 parce que l'ordonnée à l'origine est bcp plus loin 20*2*2
                     #  que pour la phase A
set.seed(1)
erreur_b <- rnorm(14, mean = 0, sd = .04)[-c(6,7,13,14)] # Sans les fin de semaines
minute_b <- 1

#     Fréquence non-cible
celeration_b_nc <- 1.4
b1_b_nc <- log10(celeration_b_nc^(1/7))
b0_b_nc <- log10(2)
erreur_b_nc <- rnorm(14, mean = 0, sd = .08)[-c(6,7,13,14)] # Sans les fin de semaines

# Création des données - phase A ####
phase_a <- tibble(
  # Date: Sans les fin de semaines
  date = c(
    seq.Date(from = first_sunday + 1, to = first_sunday + 5, by = "days"),
    seq.Date(from = first_sunday + 8, to = first_sunday + 12, by = "days")),
  # Jour: Axe des x
  jour = as.integer(date - first_sunday),
  # Fréquence cible:
  frequence = round(10^(b0_a + b1_a*jour + erreur_a)),
  reponse = frequence*minute_a,
  minute = minute_a,
  # Fréquence non-cible:
  frequence_nc = round(10^(b0_a_nc + b1_a_nc*jour + erreur_a_nc)),
  reponse_nc = frequence_nc*minute_a,
  # Phase:
  phase = "A"
)

# Création des données - phase B ####
phase_b <- tibble(
  # Date: Sans les fin de semaines
  date = c(
    seq.Date(from = first_sunday + 15, to = first_sunday + 19, by = "days"),
    seq.Date(from = first_sunday + 22, to = first_sunday + 26, by = "days")),
  # Jour: Axe des x
  jour = as.integer(date - first_sunday),
  # Fréquence cible:
  frequence = round(10^(b0_b + b1_b*jour + erreur_b)),
  reponse = frequence*minute_b,
  minute = minute_b,
  # Fréquence non-cible:
  frequence_nc = round(10^(b0_b_nc + b1_b_nc*jour + erreur_b_nc)),
  reponse_nc = frequence_nc*minute_b,
  # Phase:
  phase = "B"
)

# Création de la base de données complète - phase A et B
example_pt_data <-
  bind_rows(phase_a, phase_b) %>%
  mutate(i = seq(1:(nrow(phase_a) + nrow(phase_b)))) %>%
  select(i, date, jour, minute, reponse, frequence, reponse_nc, frequence_nc, phase)

# Exportation des données ####
#write_rds(my_data, data_output)
#write_rds(first_sunday, first_sunday_output)

# Nettoyage de l'environnement de travail ####
#   Note: Les packages sont cependant toujours chargées
#remove(list = ls())

usethis::use_data(example_pt_data, overwrite = TRUE)
usethis::use_data(example_pt_date_zero, overwrite = TRUE)
