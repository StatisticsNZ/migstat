#' Synthetic data on border crossings
#'
#' Synthetic data on border crossings by six individuals. The observation
#' period runs from 1 January 2009 to 31 December 2011.
#'
#' @format A data frame with 24 rows and 6 variables:
#' \describe{
#'  \item{crossing_id}{Unique ID for each crossing.}
#'  \item{name}{Name of the individual.}
#'  \item{passport_no}{Passport number of the individual.}
#'  \item{datetime_crossing}{The date and time of the border crossing.}
#'  \item{direction}{\code{"Arrival"} or \code{"Departure"}.}
#'  \item{res_status_default}{The residence status of the person before the crossing,
#'  according to some classification system other than outcomes-based classification.}
#' }
"border_crossings"
