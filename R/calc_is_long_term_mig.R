
#' Determine whether border crossing is long-term migration.
#'
#' Determine, for each border crossing, whether the crossing involves
#' a change in residence status, and therefore counts as a long-term
#' migration.  When there is insufficient information to decide
#' either way, the return value is set to missing.
#' 
#' The records should be in the same order that they are
#' for \code{\link{calc_days_in_country}}.
#'
#' @inheritParams calc_days_in_country
#' @param res_status_before A vector of \code{0}s, \code{1}s, and \code{NA}s,
#' the same length as \code{personId}, giving residence status before
#' the border crossing.
#' @param res_status_after A vector of \code{0}s, \code{1}s, and \code{NA}s,
#' the same length as \code{personId}, giving residence status after
#' the border crossing.
#' @param days_in_country Number of days in the country
#' from the time of the border crossing to the end
#' of the observation or test periods. Usually calculated
#' with \code{\link{calc_days_in_country}}.
#' @param days_obs Number of days from the time of the border
#' crossing to the end of the observation period.
#' Usually calculated with \code{\link{calc_days_obs}}.
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
#' personId <- c(1, 1, 1,
#'                2, 2,
#'                3, 3)
#' is_arrival <- c(FALSE, TRUE, FALSE,
#'                 FALSE, FALSE,
#'                 TRUE, FALSE)
#' res_status_before <- c(1, 0, 0,
#'                        1, NA,
#'                        0, 0)
#' res_status_after <- c(0, 0, 0,
#'                       NA, NA,
#'                       0, 0)
#' days_in_country <- c(20, 20, 0,
#'                      30, 30,
#'                      7, 0)
#' days_obs <- c(200, 150, 130,
#'               310, 30,
#'               70, 63)
#' dur_test <- 365
#' dur_threshold <- 300
#' calc_is_long_term_mig(personId = personId,
#'                       is_arrival = is_arrival,
#'                       res_status_before = res_status_before,
#'                       res_status_after = res_status_after,
#'                       days_in_country = days_in_country,
#'                       days_obs = days_obs,
#'                       dur_test = dur_test,
#'                       dur_threshold = dur_threshold)
#' @export
calc_is_long_term_mig <- function(personId, is_arrival,
                                  res_status_before, res_status_after,
                                  days_in_country, days_obs,
                                  dur_test, dur_threshold,
                                  parallel = TRUE, n_core = 2,
                                  check = TRUE) {
    check_person_id(personId)
    check_is_arrival(is_arrival)
    res_status_before <- check_and_tidy_res_status(res_status = res_status_before,
                                                   name = "res_status_before")
    res_status_after <- check_and_tidy_res_status(res_status = res_status_after,
                                                  name = "res_status_after")
    days_in_country <- check_and_tidy_days_in_country(days_in_country)
    days_obs <- check_and_tidy_days_obs(days_obs)
    dur_test <- check_and_tidy_dur(dur = dur_test,
                                   name = "dur_test")
    dur_threshold <- check_and_tidy_dur(dur = dur_threshold,
                                        name = "dur_threshold")
    if (check) {
        check_same_length(e1 = personId,
                          e2 = res_status_before,
                          name1 = "personId",
                          name2 = "res_status_before")
        check_same_length(e1 = personId,
                          e2 = is_arrival,
                          name1 = "personId",
                          name2 = "is_arrival")
        check_same_length(e1 = personId,
                          e2 = res_status_after,
                          name1 = "personId",
                          name2 = "res_status_after")
        check_same_length(e1 = personId,
                          e2 = days_in_country,
                          name1 = "personId",
                          name2 = "days_in_country")
        check_res_status_after_and_person_id(res_status_after = res_status_after,
                                             personId = personId)
        check_res_status_before_and_res_status_after(res_status_before = res_status_before,
                                                     res_status_after = res_status_after)
        check_is_arrival_and_res_status_before_and_res_status_after(is_arrival = is_arrival,
                                                                    res_status_before = res_status_before,
                                                                    res_status_after = res_status_after)
        check_res_status_and_is_arrival_and_days_in_country(res_status_before = res_status_before,
                                                            res_status_after = res_status_after,
                                                            is_arrival = is_arrival,
                                                            days_in_country = days_in_country,
                                                            dur_threshold = dur_threshold,
                                                            dur_test = dur_test)
        check_days_in_country_and_days_obs(days_in_country = days_in_country,
                                           days_obs = days_obs)
        check_days_in_country_and_dur_test(days_in_country = days_in_country,
                                           dur_test = dur_test)
    }
    is_arrival <- as.integer(is_arrival)
    res_status_before[is.na(res_status_before)] <- -1L
    res_status_after[is.na(res_status_after)] <- -1L
    if (parallel) {
        check_n_core(n_core)
        is_arrival <- split_vector_by_n_core_and_person_id(is_arrival,
                                                           n_core = n_core,
                                                           personId = personId)
        res_status_before <- split_vector_by_n_core_and_person_id(res_status_before,
                                                                  n_core = n_core,
                                                                  personId = personId)
        res_status_after <- split_vector_by_n_core_and_person_id(res_status_after,
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
                                    fun = is_long_term_mig_internal,
                                    is_arrival = is_arrival,
                                    res_status_before = res_status_before,
                                    res_status_after = res_status_after,
                                    days_in_country = days_in_country,
                                    days_obs = days_obs,
                                    dur_test = dur_test,
                                    dur_threshold = dur_threshold,
                                    n_crossing_person = n_crossing_person,
                                    SIMPLIFY = FALSE,
                                    USE.NAMES = FALSE)
        parallel::stopCluster(cl)
        ans <- unlist(ans)
    }
    else {
        n_crossing_person <- rle(personId)$lengths
        ans <- is_long_term_mig_internal(is_arrival = is_arrival,
                                         res_status_before = res_status_before,
                                         res_status_after = res_status_after,
                                         days_in_country = days_in_country,
                                         days_obs = days_obs,
                                         dur_test = dur_test,
                                         dur_threshold = dur_threshold,
                                         n_crossing_person = n_crossing_person)
    }
    ans[ans == -1L] <- NA_integer_
    ans
}
