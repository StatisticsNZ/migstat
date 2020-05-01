
#' Calculate the number of days until the end of the
#' observation or test period
#'
#' Calculate, for each border crossing, the number of days
#' until the end of the observation period, or the
#' test period, whichever comes first.
#'
#' \code{date_crossing} can contain dates after \code{date_obs_end}.
#' The return value for all such dates is 0.
#' 
#' The records should be in the same order that they are
#' for \code{\link{calc_days_in_country}}.
#'
#' @inheritParams calc_days_in_country
#'
#' @return An integer vector the same length as \code{date_crossing}.
#'
#' @examples
#' date_crossing <- c("2009-10-12", "2010-01-05", "2009-03-01",
#'                    "2010-01-20", "2010-12-09")
#' date_obs_end <- "2010-06-30"
#' dur_test <- 480
#' calc_days_obs(date_crossing = date_crossing,
#'               date_obs_end = date_obs_end,
#'               dur_test = dur_test)
#' @export
calc_days_obs <- function(date_crossing, date_obs_end, dur_test) {
    date_crossing <- check_and_tidy_date_crossing(date_crossing)
    date_obs_end <- check_and_tidy_date_obs_end(date_obs_end)
    dur_test <- check_and_tidy_dur(dur = dur_test,
                                   name = "dur_test")
    date_crossing <- as.integer(date_crossing)
    date_obs_end <- as.integer(date_obs_end)
    pmin(pmax(date_obs_end - date_crossing, 0L), dur_test)
}
