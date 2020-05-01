
#' Calculate residence status after border crossing
#'
#' Calculate, for each border crossing, the passenger's
#' residence status after the crossing.  If insufficient
#' time has passed since the border crossing, it may not
#' yet be possible to calculate residence status. In this
#' case, the residence status is set to \code{NA}.
#' 
#' The records should be in the same order that they are
#' for \code{\link{calc_days_in_country}}.
#'
#' @inheritParams calc_days_in_country
#' @param days_in_country Number of days in the country
#' from the time of the border crossing to the end
#' of the observation or test periods. Usually calculated
#' with \code{\link{calc_days_in_country}}.
#' @param days_obs Number of days from the time of the border
#' crossing to the end of the observation period.
#' Usually calculated with \code{\link{calc_days_obs}}.
#' @param res_status_initial A vector of \code{0}s and \code{1}s,
#' the same length as \code{personId}, giving each person's
#' residence status before their first border crossing.
#' If a person makes \code{n} border crossings, then that
#' person's residence status will be repeated \code{n} times
#' in \code{res_status_initial}.
#' @param dur_threshold The length of time a person must spend
#' in or out of the country, within the test period,
#' in order to change their residence status.
#' Can be an integer giving the number of days, the result
#' of a call to function \code{\link[base]{difftime}}, or an object of
#' class \code{\link[lubridate:Duration-class]{Duration}}.
#'
#' @return An integer vector the same length as \code{personId}.
#'
#' @examples
#' personId <- c(1, 1, 2, 2, 2, 3)
#' is_arrival <- c(TRUE, FALSE, FALSE, TRUE, FALSE, FALSE)
#' days_in_country <- c(10, 0, 390, 390, 365, 0)
#' days_obs <- c(200, 190, 500, 480, 475, 30)
#' res_status_initial <- c(0, 0, 1, 1, 1, 1)
#' dur_test <- 480
#' dur_threshold <- 365
#' calc_res_status_after(personId = personId,
#'                       is_arrival = is_arrival,
#'                       days_in_country = days_in_country,
#'                       days_obs = days_obs,
#'                       res_status_initial = res_status_initial,
#'                       dur_test = dur_test,
#'                       dur_threshold = dur_threshold)
#' @export
calc_res_status_after <- function(personId, is_arrival, days_in_country,
                                  days_obs, res_status_initial,
                                  dur_test, dur_threshold,
                                  parallel = TRUE, n_core = 2,
                                  check = TRUE) {
    if (identical(length(personId), 0L))
        return(integer())
    check_person_id(personId)
    check_is_arrival(is_arrival)
    days_in_country <- check_and_tidy_days_in_country(days_in_country)
    days_obs <- check_and_tidy_days_obs(days_obs)
    dur_test <- check_and_tidy_dur(dur = dur_test,
                                   name = "dur_test")
    dur_threshold <- check_and_tidy_dur(dur = dur_threshold,
                                        name = "dur_threshold")
    res_status_initial <- check_and_tidy_res_status_initial(res_status_initial)
    if (check) {
        check_same_length(e1 = personId,
                          e2 = is_arrival,
                          name1 = "personId",
                          name2 = "is_arrival")
        check_same_length(e1 = personId,
                          e2 = days_obs,
                          name1 = "personId",
                          name2 = "days_obs")
        check_same_length(e1 = personId,
                          e2 = days_in_country,
                          name1 = "personId",
                          name2 = "days_in_country")
        check_same_length(e1 = personId,
                          e2 = res_status_initial,
                          name1 = "personId",
                          name2 = "res_status_initial")
        check_res_status_initial_and_person_id(res_status_initial = res_status_initial,
                                               personId = personId)
        check_dur_test_and_dur_threshold(dur_test = dur_test,
                                         dur_threshold = dur_threshold)
        check_days_in_country_and_days_obs(days_in_country = days_in_country,
                                           days_obs = days_obs)
        check_days_in_country_and_dur_test(days_in_country = days_in_country,
                                           dur_test = dur_test)
    }
    is_arrival <- as.integer(is_arrival)
    if (parallel) {
        check_n_core(n_core)
        res_status_initial <- split_vector_by_n_core_and_person_id(res_status_initial,
                                                                  n_core = n_core,
                                                                  personId = personId)
        is_arrival <- split_vector_by_n_core_and_person_id(is_arrival,
                                                           n_core = n_core,
                                                           personId = personId)
        days_in_country <- split_vector_by_n_core_and_person_id(days_in_country,
                                                                n_core = n_core,
                                                                personId = personId)
        days_obs <- split_vector_by_n_core_and_person_id(days_obs,
                                                         n_core = n_core,
                                                         personId = personId)
        personId <- split_vector_by_n_core_and_person_id(personId,
                                                          n_core = n_core,
                                                          personId = personId)
        n_crossing_person <- lapply(personId, function(x) rle(x)$lengths)
        cl <- parallel::makeCluster(n_core)
        ans <- parallel::clusterMap(cl = cl,
                                    fun = res_status_after_internal,
                                    res_status_initial = res_status_initial,
                                    is_arrival = is_arrival,
                                    days_in_country = days_in_country,
                                    days_obs = days_obs,
                                    n_crossing_person = n_crossing_person,
                                    dur_test = dur_test,
                                    dur_threshold = dur_threshold,
                                    SIMPLIFY = FALSE,
                                    USE.NAMES = FALSE)
        parallel::stopCluster(cl)
        ans <- unlist(ans)
    }
    else {
        n_crossing_person <- rle(personId)$lengths
        ans <- res_status_after_internal(res_status_initial = res_status_initial,
                                         is_arrival = is_arrival,
                                         days_in_country = days_in_country,
                                         days_obs = days_obs,
                                         n_crossing_person = n_crossing_person,
                                         dur_test = dur_test,
                                         dur_threshold = dur_threshold)
    }
    ans[ans == -1L] <- NA
    ans
}



