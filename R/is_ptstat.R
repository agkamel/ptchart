#' @title
#' Check ptstat class
#'
#' @description Is the object an S3 object of class "ptstat" ?
#'
#' @param x A S3 object.
#'
#' @return Boolean.
#' @export
#'
#' @examples
#' # TODO
is_ptstat <- function(x) {
  return(inherits(x, "ptstat"))
}
