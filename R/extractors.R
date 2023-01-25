celeration <- function(object) {
  stopifnot("! `object` must be of class `ptstat`" = is_ptstat(object))
  ptobject <- object[["c_table"]]
  ptobject
}

bounce <- function(object) {
  stopifnot("! `object` must be of class `ptstat`" = is_ptstat(object))
  ptobject <- object[["b_table"]]
  ptobject
}

bounce_up <- function(object) {
  stopifnot("! `object` must be of class `ptstat`" = is_ptstat(object))
  ptobject <- bounce(object)
  ptobject[c("phase", "b_up_raw", "b_up", "b_up_raw_err", "b_up_err")]
}

bounce_down <- function(object) {
  stopifnot("! `object` must be of class `ptstat`" = is_ptstat(object))
  ptobject <- bounce(object)
  ptobject[c("phase", "b_down_raw", "b_down", "b_down_raw_err", "b_down_err")]
}

bounce_total <- function(object) {
  stopifnot("! `object` must be of class `ptstat`" = is_ptstat(object))
  ptobject <- bounce(object)
  ptobject[c("phase","b_total", "b_total_err")]
}

jump <- function(object) {
  stopifnot("! `object` must be of class `ptstat`" = is_ptstat(object))
  ptobject <- object[["j_table"]]
  ptobject
}

turn <- function(object) {
  stopifnot("! `object` must be of class `ptstat`" = is_ptstat(object))
  ptobject <- object[["t_table"]]
  ptobject
}

describe <- function(object) {
  stopifnot("! `object` must be of class `ptstat`" = is_ptstat(object))
  ptobject <- object[["desc"]]
  ptobject
}

terms <- function(object) {
  stopifnot("! `object` must be of class `ptstat`" = is_ptstat(object))
  ptobject <- object[["terms"]]
  ptobject
}

