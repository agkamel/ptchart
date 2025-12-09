#' Dataset example
#'
#' A fictional dataset for illustrating ptchart package functions.
#'
#' @format ## `ptdata01`
#' A data frame with 20 rows and 9 columns:
#' \describe{
#'   \item{i}{Observation index id.}
#'   \item{date}{Date of observation in yyyy-mm-dd format.}
#'   \item{jour}{Day of the observation.}
#'   \item{minute}{Number of minutes.}
#'   \item{t_response}{Count of target responses.}
#'   \item{t_frequency}{Frequency of target responses.}
#'   \item{nt_response}{Count of non-target responses.}
#'   \item{nt_frequency}{Frequency of non-target responses.}
#'   \item{phase}{Phase of intervention.}
#' }
"ptdata01"


#' Date of day zero.
#'
#' Date of day zero. To be used with `ptdata01`.
#'
#' @format ## `ptdate0`
#' A vector of class `Date` with only one date: 2021-07-18.
"ptdate0"
