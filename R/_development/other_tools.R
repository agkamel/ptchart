angle_to_celeration <- function(x) {
  NA
}


angle_to_celeration <- function(angle) {

  tan(34*pi/180) * log10(2) * 7

}


d_tan <- function(angle) {
  tan(angle * pi / 180)
}

d_atan <- function(ratio) {
  atan(ratio) * 180 / pi
}

# En réalité
d_tan(8.13)
d_atan(0.1428553)
1/7


# 34 degre souhaité
d_tan(34)    # 0.6745085
d_tan(34)*7
# Opp: 4.72156

ratio_34_2 <- log10(4.72156) / log10(2)

##
d_tan(34) * 4.72156

d_atan(ratio_34_2)



angle_to_celeration()

log10(1)
log10(2)
log10(5)
log10(10)

celeration_to_angle <- function(cel) {
  #7/(log10(2)/tan(34*pi/180))

  cel

  7 * tan(34*pi/180) / log10(2)




  seven_days <- 7


  log_cel_value <- log10(2)




  theta <- tan(34*pi/180)

}




tan(log10(2))^-1
sin(log10(2))















#as.Date("2025-03-01") + c(0, 1, 2, NA, 4)


# ## Il serait probablement très utile d'ajouter un argument `phase`,
# # pour chacune de ces fonctions.
#
#
# turn <- function(x, y, phase) {
#
#   tibble::tibble(x, y, phase) |>
#     dplyr::group_by(phase) |>
#     dplyr::summarise(cel = celeration(x, y)) |>
#     dplyr::mutate(turn = dplyr::lead(cel) / cel) |>
#     dplyr::pull()
#
# }
#
#
#
#
#
# turn(ptdata01$day,
#             ptdata01$t_frequency,
#             ptdata01$phase)
#
# ptdata01 |>
#   dplyr::group_by(phase) |>
#   dplyr::mutate(turn = turn(day, t_frequency, phase))
#
#
# temp_df <- tibble::tibble(x = ptdata01$day,
#                           y = ptdata01$t_frequency,
#                           phase = ptdata01$phase
#                           )
# #temp_df2 <-
# temp_df |>
#   dplyr::group_by(phase) |>
#   dplyr::summarise(
#     cel = celeration(x, y),
#     last = dplyr::last(predicted_values(x, y)),
#     first = dplyr::first(predicted_values(x, y))
#   ) |>
#   dplyr::mutate(to = dplyr::lead(phase), .after = phase) |>
#   dplyr::mutate(lead_last = dplyr::lead(last),
#                 lead_cel = dplyr::lead(cel)) |>
#   dplyr::mutate(jump = 10^log10(lead_last) / 10^log10(first),
#                 turn = lead_cel / cel)
#
#
# predict_freq <- function(day, b0, b1) {
#   b0 + day * b1
# }
#
# predict_freq(5, 1, 0.02)
#
# (1.5^(1/7))^
#
# 7 * 1.059634
#
# 5^2
#
# 25^(1/2)
#
# predicted_values(ptdata01$day, ptdata01$t_frequency)












