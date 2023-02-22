#' Extractor functions for ptstat object
#'
#' @param object An object of class `ptstat`
#'
#' @return A dataframe
#' @export
#'
#' @examples
#' #TODO

#' @describeIn extractors Extract accuracy table
accuracy <- function(object) {
  stopifnot("! `object` must be of class `ptstat`" = is_ptstat(object))
  object[["a_table"]]
}

#' @describeIn extractors Extract celeration table
celeration <- function(object) {
  stopifnot("! `object` must be of class `ptstat`" = is_ptstat(object))
  object[["c_table"]]
}

#' @describeIn extractors Extract bounce table
bounce <- function(object) {
  stopifnot("! `object` must be of class `ptstat`" = is_ptstat(object))
  object[["b_table"]]
}

#' @describeIn extractors Extract only bounce up
bounce_up <- function(object) {
  stopifnot("! `object` must be of class `ptstat`" = is_ptstat(object))
  ptobject <- bounce(object)
  ptobject[c("phase", "b_up_raw", "b_up", "b_up_raw_err", "b_up_err")]
}

#' @describeIn extractors Extract only bounce down
bounce_down <- function(object) {
  stopifnot("! `object` must be of class `ptstat`" = is_ptstat(object))
  ptobject <- bounce(object)
  ptobject[c("phase", "b_down_raw", "b_down", "b_down_raw_err", "b_down_err")]
}

#' @describeIn extractors Extract only bounce total
bounce_total <- function(object) {
  stopifnot("! `object` must be of class `ptstat`" = is_ptstat(object))
  ptobject <- bounce(object)
  ptobject[c("phase","b_total", "b_total_err")]
}

#' @describeIn extractors Extract jump table
jump <- function(object) {
  stopifnot("! `object` must be of class `ptstat`" = is_ptstat(object))
  object[["j_table"]]
}

#' @describeIn extractors Extract turn table
turn <- function(object) {
  stopifnot("! `object` must be of class `ptstat`" = is_ptstat(object))
  object[["t_table"]]
}

#' @describeIn extractors Extract descriptive statistics table
describe <- function(object) {
  stopifnot("! `object` must be of class `ptstat`" = is_ptstat(object))
  object[["desc"]]
}

#' @describeIn extractors Extract regression terms table
terms <- function(object) {
  stopifnot("! `object` must be of class `ptstat`" = is_ptstat(object))
  object[["terms"]]
}

#' @describeIn extractors Extract pttable table
pttable <- function(object) {
  stopifnot("! `object` must be of class `ptstat`" = is_ptstat(object))
  object[["pttable"]]
}


