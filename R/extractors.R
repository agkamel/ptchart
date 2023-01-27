celeration <- function(object) {
  stopifnot("! `object` must be of class `ptstat`" = is_ptstat(object))
  object[["c_table"]]
}

bounce <- function(object) {
  stopifnot("! `object` must be of class `ptstat`" = is_ptstat(object))
  object[["b_table"]]
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
  object[["j_table"]]
}

turn <- function(object) {
  stopifnot("! `object` must be of class `ptstat`" = is_ptstat(object))
  object[["t_table"]]
}

describe <- function(object) {
  stopifnot("! `object` must be of class `ptstat`" = is_ptstat(object))
  object[["desc"]]
}

terms <- function(object) {
  stopifnot("! `object` must be of class `ptstat`" = is_ptstat(object))
  object[["terms"]]
}

pttable <- function(object) {
  stopifnot("! `object` must be of class `ptstat`" = is_ptstat(object))
  object[["pttable"]]
}
