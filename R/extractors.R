celeration <- function(object) {
  ptobject <- object[["c_table"]]
  ptobject
}

bounce <- function(object) {
  ptobject <- object[["b_table"]]
  ptobject
}

bounce_up <- function(object) {
  ptobject <- bounce(object)
  ptobject[c("phase", "b_up_raw", "b_up", "b_up_raw_err", "b_up_err")]
}

bounce_down <- function(object) {
  ptobject <- bounce(object)
  ptobject[c("phase", "b_down_raw", "b_down", "b_down_raw_err", "b_down_err")]
}

bounce_total <- function(object) {
  ptobject <- bounce(object)
  ptobject[c("phase","b_total", "b_total_err")]
}

jump <- function(object) {
  ptobject <- object[["j_table"]]
  ptobject
}

turn <- function(object) {
  ptobject <- object[["t_table"]]
  ptobject
}

describe <- function(object) {
  ptobject <- object[["desc"]]
  ptobject
}

terms <- function(object) {
  ptobject <- object[["terms"]]
  ptobject
}

