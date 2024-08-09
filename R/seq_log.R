#' Generate values of a logarithm scale
#'
#' @param cycle Integer vector. Default to 0. Also known as the characteristic.
#' @param base Integer scalar. Default to 10.
#'
#' @return A logarithm scale, a numeric vector.
#' @export
#'
#' @examples
#' seq_log()
#' seq_log(0:2)
#' seq_log(base = 5)
seq_log <- function(cycle = 0, base = 10) {

  stopifnot("Arg `cycle` must be an numeric vector." = (is.atomic(cycle) == TRUE) & (is.numeric(cycle) == TRUE))
  stopifnot("Arg `base` must be an numeric vector of length one." = (is.atomic(base) == TRUE) & (length(base) == 1) & (is.numeric(base) == TRUE))

  log_sequence <- log(1:(base - 1), base = base)
  last_log <- log(base, base = base)

  length_cycle <- length(cycle)

  output <- vector(mode = "list",
                   length = length_cycle)

  for (i in seq_along(cycle)) {
    if (max(cycle) != cycle[i]) {
      output[[i]] <- base^(log_sequence + cycle[i])

    } else {
      output[[i]] <- c(
        base^(log_sequence + cycle[i]),
        base^(last_log + cycle[i])
        )
    }
  }

  output |>
    unlist()

}


#' Generate values of a centered logarithm scale
#'
#' @param cycle Positive integer vector. Default to 0. Also known as the characteristic.
#' @param base Integer scalar. Default to 10.
#'
#' @return A centered logarithm scale, a numeric vector.
#' @export
#'
#' @examples
#' seq_log_centered()
#' seq_log_centered(1:2)
seq_log_centered <- function(cycle = 0, base = 10) {

  stopifnot("Arg `cycle` must be greater or equal than 0." = all(cycle >= 0))

  upper_way <- seq_log(cycle = cycle, base = base)
  lower_way <- (1 / upper_way)[2:length(upper_way)]


  c(
    lower_way |> sort(decreasing = FALSE),
    upper_way
  )

}


