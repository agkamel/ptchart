#' Title
#'
#' @param string
#' @param pattern
#' @param n
#'
#' @return
#' @export
#'
#' @examples
str_split_one <- function(string, pattern, n = Inf) {
  stopifnot(is.character(string), length(string) <= 1)
  if (length(string) == 1) {
    stringr::str_split(string = string, pattern = pattern, n = n)[[1]]
  } else {
    character()
  }
}
