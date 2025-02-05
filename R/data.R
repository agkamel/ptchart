#' Dataset example
#'
#' A fictional dataset for illustrating ptchart package functions.
#'
#' @format ## `example_pt_data`
#' A data frame with 20 rows and 9 columns:
#' \describe{
#'   \item{i}{Observation index id.}
#'   \item{date}{Date of observation in yyyy-mm-dd format.}
#'   \item{jour}{Day of the observation.}
#'   \item{minute}{Number of minutes.}
#'   \item{reponse}{Count of target responses.}
#'   \item{frequence}{Frequency of target responses.}
#'   \item{reponse_nc}{Count of non-target responses.}
#'   \item{frequence_nc}{Count of non-target responses.}
#'   \item{phase}{Phase of intervention.}
#' }
"example_pt_data"


#' Date of day zero.
#'
#' Date of day zero. To be used with `example_pt_data`.
#'
#' @format ## `example_pt_date_zero`
#' A vector of class `Date` with only one date: 2021-07-18.
"example_pt_date_zero"
