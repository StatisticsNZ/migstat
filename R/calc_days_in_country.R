
#' Calculate the number of days in the country
#'
#' Calculate, for each border crossing, the number of days
#' in the country between the crossing and the end
#' of the observation or test period, whichever comes first.
#'
#' \code{date_crossing} must have the same length as \code{personId}, and
#' the dates must be ordered, from earliest to latest, within
#' each unique value of \code{personId}.
#'
#' \code{date_crossing} can contain dates after \code{date_obs_end}.
#' The return value for all such dates is 0.
#' 
#' If a person has two arrivals in a row, then 
#' \code{days_in_country} assumes that some a departure
#' was missed, and that this departure occurred half way
#' through the interval between the two arrivals. Similarly,
#' if a person has two departures in a row, then 
#' \code{days_in_country} assumes that an arrival was missed,
#' and that this arrival occurred half way through the
#' interval between the two departures.
#' 
#' @section Warning:
#' 
#' If the same person has multiple crossings on the same day,
#' it is the user's responsibility to ensure that the
#' crossings are sorted from earliest to latest.
#'
#' @param personId A vector of person/passenger IDs.
#' @param date_crossing A vector of dates specifying the dates of border
#' crossings. Can be of class \code{Date} or \code{character}, and must have
#' format yyyy-mm-dd. Cannot contain missing values.
#' @param is_arrival A logical vector, the same length as \code{personId}
#' and \code{date_crossing}, with each element specifying whether the
#' corresponding border crossing is an arrival.
#' For each person ID, the values of \code{is_arrival} must alternate between
#' \code{TRUE} and \code{FALSE}, starting with either
#' (depending on the direction of the first border crossing by the passenger).
#' @param date_obs_end The last date for which data are available.
#' @param dur_test The length of the test period.
#' Can be an integer giving the number of days, the result
#' of a call to function \code{\link[base]{difftime}}, or an object of
#' class \code{\link[lubridate:Duration-class]{Duration}}.
#' @param parallel Logical. Whether to use parallel processing, to
#' speed up calculations.  Defaults to \code{TRUE}.
#' @param n_core The number of cores to use, if \code{parallel} is
#' \code{TRUE}. Defaults to \code{2}. Higher values will
#' typically result in faster calculations on computers
#' with more than two cores.
#' @param check Logical. Whether to perform sanity checks on
#' arguments. Defaults to \code{TRUE}.
#'
#' @return An integer vector the same length as \code{personId}.
#'
#' @examples
#' personId <- c(1, 1, 1,
#'                2, 2, 2,
#'                3, 3)
#' date_crossing <- c("2010-01-05", "2010-03-12", "2010-05-30",
#'                    "2009-01-18", "2010-02-24", "2010-03-01",
#'                    "2010-01-20", "2010-02-09")
#' is_arrival <- c(FALSE, TRUE, FALSE,
#'                 TRUE, FALSE, TRUE,
#'                 TRUE, FALSE)
#' date_obs_end <- "2010-06-30"
#' dur_test <- 365
#' calc_days_in_country(personId = personId,
#'                      date_crossing = date_crossing,
#'                      is_arrival = is_arrival,
#'                      date_obs_end = date_obs_end,
#'                      dur_test = dur_test)
#' @export
calc_days_in_country <- function(personId, date_crossing, is_arrival,
                            date_obs_end, dur_test,
                            parallel = TRUE, n_core = 2,
                            check = TRUE) {
    check_person_id(personId)
    date_crossing <- check_and_tidy_date_crossing(date_crossing)
    check_is_arrival(is_arrival)
    check_parallel(parallel)
    date_obs_end <- check_and_tidy_date_obs_end(date_obs_end)
    dur_test <- check_and_tidy_dur(dur = dur_test,
                                   name = "dur_test")
    if (check) {
        check_same_length(e1 = personId,
                          e2 = date_crossing,
                          name1 = "personId",
                          name2 = "date_crossing")
        check_same_length(e1 = personId,
                          e2 = is_arrival,
                          name1 = "personId",
                          name2 = "is_arrival")
        check_date_crossing_and_person_id(date_crossing = date_crossing,
                                          personId = personId)
    }
    is_arrival <- as.integer(is_arrival)
    if (parallel) {
        check_n_core(n_core)
        date_crossing <- split_vector_by_n_core_and_person_id(date_crossing,
                                                              n_core = n_core,
                                                              personId = personId)
        is_arrival <- split_vector_by_n_core_and_person_id(is_arrival,
                                                           n_core = n_core,
                                                           personId = personId)
        personId <- split_vector_by_n_core_and_person_id(personId,
                                                          n_core = n_core,
                                                          personId = personId)
        n_crossing_person <- lapply(personId, function(x) rle(x)$lengths)
        cl <- parallel::makeCluster(n_core)
        ans <- parallel::clusterMap(cl = cl,
                                    fun = days_in_country_internal,
                                    date_crossing = date_crossing,
                                    is_arrival = is_arrival,
                                    n_crossing_person = n_crossing_person,
                                    date_obs_end = date_obs_end,
                                    dur_test = dur_test,
                                    SIMPLIFY = FALSE,
                                    USE.NAMES = FALSE)
        parallel::stopCluster(cl)
        ans <- unlist(ans)
    }
    else {
        n_crossing_person <- rle(personId)$lengths
        ans <- days_in_country_internal(date_crossing = date_crossing,
                                        is_arrival = is_arrival,
                                        n_crossing_person = n_crossing_person,
                                        date_obs_end = date_obs_end,
                                        dur_test = dur_test)
    }
    ans
}





