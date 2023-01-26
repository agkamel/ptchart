# Fonction pour générer le graphique de célération standardisé

# Packages ####
library(readr)
library(ggplot2)
library(lubridate)
library(dplyr)

# Paramètres ####
#data_input <- "r/example_data.rds"

# Importing dataset ####
#my_data <- read_rds(data_input)

# Fonction make_scale_x ####
make_scale_x <- function(first_sunday) {

  if(is.character(first_sunday)) {first_sunday <- ymd(first_sunday)}

  # Vérification
  stopifnot(
    is.na(first_sunday) == FALSE,
    wday(first_sunday, week_start = 1) == 7
  )
  # Création de la base de données pour ggplot
  tibble(
    date = seq.Date(
      from = first_sunday,
      by = "day",
      length.out = 141 # Because the first sunday is 0, not 1
    ),
    no_date = seq(from = 0, to = 140),
    breaks = case_when(
      no_date %% 7 == 0 ~ 1,
      TRUE ~ 0
    ),
    labels = case_when(
      no_date %% 14 == 0 ~ as.character(no_date),
      no_date %% 7 == 0 ~ "",
      TRUE ~ NA_character_
    ),
    sec_axis_breaks = case_when(
      no_date %% 28 == 0 ~ 1,
      TRUE ~ 0
    ),
    sec_axis_labels = case_when(
      no_date %% 28 == 0 ~ no_date / 7,
      TRUE ~ NA_real_
    )
  )
}

# Testing make_scale_x() ####
#make_scale_x("2021-08-01")

# Fonction make_scale_y() ####
make_scale_y <- function(){
  tibble(
    base = 10,
    exponent = c(-4, -4, -4, -4, rep(c(-3, -2, -1, 0, 1, 2), each = 9), 3),
    base_to_exponent = base^exponent,
    sub_unit = c((1/1440)/(10^-4), 7:9, rep(1:9, times = 6), 1),
    frequency = base^exponent * sub_unit,
    y_breaks = case_when(
      sub_unit == 1 | sub_unit == 5 ~ TRUE,
      TRUE ~ FALSE
    )
  )

}

# Testing make_scale_y() ####
#make_scale_y()

# Fonction ptchart() ####
ptchart <- function(data,
                    date,
                    frequency,
                    first_sunday,
                    zoom_x = NULL,
                    zoom_y = NULL,
                    name_x = "Frequency",
                    name_y = "Day") {

  x_scale <- make_scale_x(first_sunday)

  y_scale <- make_scale_y()

  # Test for zoom_x
  stopifnot(is.null(zoom_x) == TRUE | is.Date(zoom_x) & length(zoom_x) == 2)

  # Test for zoom_y
  stopifnot(is.null(zoom_y) == TRUE | is.vector(zoom_y, mode = "numeric") & length(zoom_y) == 2)

  ggplot(data = data, mapping = aes(x = {{ date }}, y = {{ frequency }} )) +
    geom_point() +
    scale_y_log10(
      name = name_x,
      breaks = {y_scale %>% filter(y_breaks == TRUE) %>% select(frequency) %>% pull()},
      labels = {y_scale %>% filter(y_breaks == TRUE) %>% select(frequency) %>% pull()},
      minor_breaks = {y_scale %>% select(frequency)},
      limits = c(1/(24*60), 1000),
      sec.axis = sec_axis(
        trans = ~.,
        breaks = c(.001, .002, .005, .01, .02, .05, .1, .2, .5, 1, 2, 3, 4, 6),
        labels = c("1000'", "500'", "200'", "100'", "50'", "20'", "10'", "5'", "2'", "1'", "30\"", "20\"", "15\"", "10\""),
        name = ""
      )
    ) +
    scale_x_continuous(
      name = name_y,
      breaks = {x_scale %>% filter(breaks == 1) %>% select(date) %>% pull()},
      labels = {x_scale %>% filter(is.na(labels) == FALSE) %>% select(labels) %>% pull()},
      minor_breaks = {x_scale$date},
      limits = c(head(x_scale$date, 1),
                 tail(x_scale$date, 1))
    ) +
    theme(
      panel.grid.major = element_line(colour = "#00b1d9"),
      panel.grid.minor = element_line(colour = "#66d1e8"),
      text = element_text(family = "serif", size = 12)
      #aspect.ratio = 5.44 / 8, # 5 7/16 de pouce par 8 pouce #https://jweshleman.wordpress.com/2006/03/25/og-on-standard-celeration-charting-system-standards/
    ) +
    coord_fixed(expand = FALSE,
                #ratio = 1/1, # Voir note sur le ratio ci-bas, explication 1
                #ratio = 7/1, # Voir note sur le ratio ci-bas, explication 2
                #ratio = 7/log10(2), # 0.30103 voir note sur le ratio ci-bas, explication 3
                ratio = 7/(log10(2)/tan(34*pi/180)), # 0.30105 / 0.6745 # Voir note sur le ratio ci-bas, explication 4

                # ZOOM sur le graphique
                xlim = zoom_x,
                ylim = zoom_y,
    )
  # Ligne séparant les phases
  #geom_vline(xintercept = ymd("2021-08-01")+0.5) +

  # Plancher d'enregistrement
  #geom_segment(x = ymd("2021-07-20")-0.5,
  #             xend = ymd("2021-07-20")+0.5,
  #             y = log10(1),
  #             yend = log10(1)
  #             ) +

  # Pente de régression
  #geom_smooth(method = "lm", se = FALSE, mapping = aes(group = phase)) +

  #geom_line()
}


# Testing de la fonction ptchart() ####
#ptchart(my_data, date, frequence, "2021-07-18",
#        zoom_x = c(ymd("2021-07-18"), ymd("2021-07-18") + days(28)),
#        zoom_y = c(1,20))


#ggsave(filename = "r/output_ptcharts.pdf",
#       scale = 1)











# Note sur le ratio ####

# EXPLICATION 1

# Dans la fonction coord_fixed(), l'argument ratio prend une valeur de y / x.
# Selon la documentation R, Le ratio représente le nombre d'unités sur l'axe des
# y qui est équivalent à 1 unité sur l'axe des x. Les ratios plus grand que 1 font
# que les unités sur l'axe des y sont plus long que les unités sur l'axe des x,
# et vice versa (plus petit que 1).

# Donc un ratio de 7 / 1 = y / x, signifie qu'une unité sur l'axe des y est 7 fois
# plus grand qu'une unité sur l'axe des x.

# Il faut cependant ce rappeler que notre axe des y est logarithmique sur
# sur base de 10 (scale_y_log10), c'est à dire que les données (ici les fréquences)
# sont converties en unité de distance sur l'axe des y.

# Par exemple, pour une fréquence de 1, 2, 5, et 10, un logarithme est d'abord
# appliqué pour calculer la distance correcte. Notons les résultats suivants:
# log10(1) = 0
# log10(2) = 0.30103
# log10(5) = 0.69897
# log10(10) = 1

# On peut tout de suite remarquer que l'unité sur notre axe des y est la distance
# entre 1 et 10 et que cette distance est égale à la distance entre 10 et 100:
# log10(10) = 1
# log10(100) = 2
# ..puisque que log10(100) - log10(10) = 2 - 1 = 1
# ..puisque que log10(10)  - log10(1)  = 1 - 0 = 1

# Ceci signifie que pour un ratio de y / x = 1 / 1, nous verrons sur le graphique
# une distance égale entre une unité de y (les fréquences de 1 à 10,
# ou une distance e log10(10) ) et une unité de l'axe des x (une journée).

# Lorsque ce ratio de 1/1 est appliqué on peut remarquer que l'angle entre les
# unités des axes est de 45 degree. (voir figure 1). Ceci se comprend
# facilement puisque deux côtés égaux forment un carré et l'angle d'un carré
# est de 45 degree.

# EXPLICATION 2

# Maintenant, supposons que nous voulons obtenir un ratio de 7/1 = y/x,
# c'est-à-dire que la distance d'une unité sur l'axe des y est 7 fois plus
# longue qu'une unité sur l'axe des x. Le résultat obtenue est qu'une unité
# de l'axe des y est égale en terme de distance à 7 unités sur l'axe des x.
# Sur le  graphique, on retrouve donc la distance de 1 à 10 (log10(10)) comme
# étant équivalente à 7 jours (voir figure 2). Puisque maintenant 1 unité de y
# est 7 fois plus long que une unité de x, on peut cette fois-ci retrouver
# notre angle de 45 degree de 1 unité de y (à log10(10)) et à 7 unités
# de x (7 jours).

# EXPLICATION 3

# Rappelons nous cependant que notre distance de référence sur le graphique
# n,est pas la distance entre 1 et 10 (donc log10(10) = 1), mais bien la distance entre
# 1 et 2, puisque l'on souhaite que pour un même angle donnée, une fréquence qui double correspond
# à 7 jours sur l'axe des x (et donc que la fréquence double par semaine)

# La distance entre 1 et 2 n'est cependant pas une unité de distance, mais bien 0.30103 unité
# de distance, puisque log10(2) = 0.30103. Notons que la distance entre 1 et 2 et la même
# que la distance entre 10 et 20 puisque:
# log10(2) -  log10(1)  = 0.30103 - 0 = 0.30103
# log10(20) - log10(10) = 1.30103 - 1 = 0.30103
#
# Notre ratio y/x est maintenant 7/log10(2) = 7/0.30103, c'est-à-dire que la distance de 0.30103 unité de y
# (de 1 à 2 fréquences ou log10(2)) est égale à la distance de 7 unités de x (7 jours). Sur le graphique,
# on peut voir maintenant que ces distances sont égaux et on note encore une fois notre angle de 45 degree qui commence
# de la fréquence 2 (ou log10(2)) et va jusqu'à 7 jours, tel que spécifié dans le ratio. (voir figure 3).

# EXPLICATION 4

# Enfin, il manque un dernier élément pour compléter notre raisonnement sur le ratio. Jusqu'ici, nous avons
# ignoré le fait que l'angle dans le ratio est toujours de 45 degre.
# Bien que la fréquence double à chaque semaine, la standardisation n'est pas selon un angle de 45 degree,
# mais bien selon un angle de 34 degree!

# Dans un triangle rectangle, si nous savons que la cathète opposé à l'angle et que la cathète adjacant
# à l'angle sont égaux, nous avons que tan(theta) = x / x = 1, et donc par l'application
# de tan^-1 à 1, nous obtenons theta = 45. La formule est tan(theta) = opp / adj.

# Nous savons que tout nombre élément des réels divisé par lui-même égale à 1.

# Maintenant, supposons que notre cathète adjacente est de 1 (une unité de x) et nous savons que l'angle
# est déjà donnée, c'est-à-dire dire 34 degre. Quel sera la valeur de la cathète opposé à l'angle ?
# En application la formule tan(theta) = opp / adj    =    tan(34) = opp / 1    =    .6745

# Alors que pour un angle de 45 degre, le rapport des cathètes opposé sur adj est de 1/1 = 1,
# le rapport des cathètes opposé sur adjacent pour un angle de 34 degre est de .6745/1 = .6745.

# Comme nous avions déjà notre ratio 7 / 0.30103 = y / x, nous savons que nos deux distances sont égales
# soit log10(2) = 0.30103 unité de y est égale à 7 unités de x (7 jours). Le résultat de ce ratio
# étant 7 / log10(2) = 23.2535. Donc dire que 1 distance de 1 unité de y et égale à une distance
# de 23.2535 unité de x est équivalent à dire que la distance de 0.30103 unité de y est égale à une
# distance de 7 unité de y.

# En divisant nos 0.30103 par 0.6745, nous obtenons un résultat de 0.4463 (ce qui correspond
# à un ~ log10(2.794)). C,est donc à log10(2.794) ce se rend l'angle de 45 degres qui rejoint
# le 7e jour des x, ce qui fait en sorte que log10(2) se trouve maintenant à 34 degree! (voir figure 4.1 et 4.2)
