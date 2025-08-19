#' @title Extractor functions for ptstat object
#'
#' @param object An object of class `ptstat`
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This a description of these extrators.
#'
#' @return A dataframe
#'
#' @examples
#' # Deprecated

#' @export
#' @describeIn deprec_extractors Extract accuracy table
extract_accuracy <- function(object) {
  stopifnot("! `object` must be of class `ptstat`" = is_ptstat(object))
  object[["a_table"]]
}

#' @export
#' @describeIn deprec_extractors Extract celeration table
extract_celeration <- function(object) {
  stopifnot("! `object` must be of class `ptstat`" = is_ptstat(object))
  object[["c_table"]]
}

#' @export
#' @describeIn deprec_extractors Extract bounce table
extract_bounce <- function(object) {
  stopifnot("! `object` must be of class `ptstat`" = is_ptstat(object))
  object[["b_table"]]
}

#' @export
#' @describeIn deprec_extractors Extract only bounce up
extract_bounce_up <- function(object) {
  stopifnot("! `object` must be of class `ptstat`" = is_ptstat(object))
  ptobject <- extract_bounce(object)
  ptobject[c("phase", "b_up_raw", "b_up", "b_up_raw_err", "b_up_err")]
}

#' @export
#' @describeIn deprec_extractors Extract only bounce down
extract_bounce_down <- function(object) {
  stopifnot("! `object` must be of class `ptstat`" = is_ptstat(object))
  ptobject <- extract_bounce(object)
  ptobject[c("phase", "b_down_raw", "b_down", "b_down_raw_err", "b_down_err")]
}

#' @export
#' @describeIn deprec_extractors Extract only bounce total
extract_bounce_total <- function(object) {
  stopifnot("! `object` must be of class `ptstat`" = is_ptstat(object))
  ptobject <- extract_bounce(object)
  ptobject[c("phase","b_total", "b_total_err")]
}

#' @export
#' @describeIn deprec_extractors Extract jump table
extract_jump <- function(object) {
  stopifnot("! `object` must be of class `ptstat`" = is_ptstat(object))
  object[["j_table"]]
}

#' @export
#' @describeIn deprec_extractors Extract turn table
extract_turn <- function(object) {
  stopifnot("! `object` must be of class `ptstat`" = is_ptstat(object))
  object[["t_table"]]
}

#' @export
#' @describeIn deprec_extractors Extract descriptive statistics table
extract_describe <- function(object) {
  stopifnot("! `object` must be of class `ptstat`" = is_ptstat(object))
  object[["desc"]]
}

#' @export
#' @describeIn deprec_extractors Extract regression terms table
extract_terms <- function(object) {
  stopifnot("! `object` must be of class `ptstat`" = is_ptstat(object))
  object[["terms"]]
}

#' @export
#' @describeIn deprec_extractors Extract pttable table
extract_pttable <- function(object) {
  stopifnot("! `object` must be of class `ptstat`" = is_ptstat(object))
  object[["pttable"]]
}


