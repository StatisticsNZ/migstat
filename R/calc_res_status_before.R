
#' Calculate residence status before border crossing
#'
#' Calculate, for each border crossing, the passenger's
#' residence status before the crossing.  If insufficient
#' time has passed since the border crossing, it may not
#' yet be possible to calculate residence status. In this
#' case, the residence status is set to \code{NA}.
#' 
#' The records should be in the same order that they are
#' for \code{\link{calc_days_in_country}}.
#'
#' @inheritParams calc_days_in_country
#' @param res_status_initial A vector of \code{0}s and \code{1}s,
#' the same length as \code{personId}, giving each person's
#' residence status before their first border crossing.
#' If a person makes \code{n} border crossings, then that
#' person's residence status will be repeated \code{n} times
#' in \code{res_status_initial}.
#' @param res_status_after A vector of \code{0}s, \code{1}s, and \code{NA}s,
#' the same length as \code{personId}, giving residence status after
#' the border crossing.
#'
#' @return An integer vector the same length as \code{personId}.
#'
#' @examples
#' personId <- c(1, 1,
#'                2, 2, 2,
#'                3)
#' res_status_initial <- c(0, 0,
#'                              1, 1, 1,
#'                              1)
#' res_status_after <- c(1, 1,
#'                       1, 0, 0,
#'                       1)
#' calc_res_status_before(personId = personId,
#'                        res_status_initial = res_status_initial,
#'                        res_status_after = res_status_initial)
#' @export
calc_res_status_before <- function(personId, res_status_initial,
                                   res_status_after, check = TRUE) {
    check_person_id(personId)
    res_status_initial <- check_and_tidy_res_status_initial(res_status_initial)
    res_status_after <- check_and_tidy_res_status(res_status = res_status_after,
                                                  name = "res_status_after")
    if (check) {
        check_same_length(e1 = personId,
                          e2 = res_status_initial,
                          name1 = "personId",
                          name2 = "res_status_initial")
        check_same_length(e1 = personId,
                          e2 = res_status_after,
                          name1 = "personId",
                          name2 = "res_status_after")
        check_res_status_initial_and_person_id(res_status_initial = res_status_initial,
                                                    personId = personId)
        check_res_status_after_and_person_id(res_status_after = res_status_after,
                                             personId = personId)
        check_res_status_initial_and_res_status_after(res_status_initial = res_status_initial,
                                                           res_status_after = res_status_after)
    }
    l <- split(x = data.frame(res_status_initial, res_status_after),
               f = personId)
    res_status_before_inner <- function(x) {
        n <- nrow(x)
        ans <- c(x$res_status_initial[1L],
                 x$res_status_after[-n])
        as.integer(ans)
    }
    ans <- lapply(l, res_status_before_inner)
    unlist(ans, use.names = FALSE)
}




