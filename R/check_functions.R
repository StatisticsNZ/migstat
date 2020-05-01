

## Assume that single-argument check functions and 'check_same_length'
## are called before multiple-argument check functions

check_and_tidy_date_crossing <- function(date_crossing) {
    if (any(is.na(date_crossing)))
        stop(gettextf("'%s' has missing values",
                      "date_crossing"))
    if (is.character(date_crossing)) {
        date_crossing_original <- date_crossing
        date_crossing <- tryCatch(as.Date(date_crossing),
                                  error = function(e) e)
        if (methods::is(date_crossing, "error"))
            stop(gettextf("problem coercing '%s' to class \"%s\" : %s",
                          "date_crossing", "Date", date_crossing$message))
        is_invalid_date <- is.na(date_crossing)
        if (any(is_invalid_date)) {
            first_invalid_date <- date_crossing_original[is_invalid_date][1L]
            stop(gettextf("'%s' contains invalid date(s) : %s ...",
                          "date_crossing", first_invalid_date))
        }
    }
    else {
        if (!methods::is(date_crossing, "Date"))
            stop(gettextf("'%s' has class \"%s\"",
                          "date_crossing", class(date_crossing)))
    }
    date_crossing
}


## HAS_TESTS
check_and_tidy_date_first_last <- function(date, date_crossing, name) {
    multiplier_extra <- 0.1
    name <- match.arg(name, choices = c("date_first", "date_last"))
    n <- length(date_crossing)
    is_date_first <- identical(name, "date_first")
    if (is.null(date)) {
        if (identical(n, 1L))
            extra <- lubridate::ddays(30L)
        else {
            length_interval <- date_crossing[n] - date_crossing[1L]
            extra <- multiplier_extra * length_interval / lubridate::ddays(1L)
        }
        if (is_date_first)
            date <- date_crossing[1L] - extra
        else
            date <- date_crossing[n] + extra
    }
    else {
        if (!identical(length(date), 1L))
            stop(gettextf("'%s' does not have length %d",
                          name, 1L))
        if (is.na(date))
            stop(gettextf("'%s' is missing",
                          name))
        if (!methods::is(date, "Date")) {
            date <- tryCatch(lubridate::ymd(date),
                             error = function(e) e,
                             warning = function(w) w)
            if (methods::is(date, "error") || methods::is(date, "warning"))
                stop(gettextf("'%s' does not have valid year-month-date format : %s",
                              name, date$message))
        }
        if (is_date_first) {
            if (date > date_crossing[1L])
                stop(gettextf("'%s' is later than first element of '%s'",
                              name, "date_crossing"))
        }
        else {
            if (date < date_crossing[n])
                stop(gettextf("'%s' is earlier than last element of '%s'",
                              name, "date_crossing"))
        }
    }
    date
}

check_and_tidy_date_obs_end <- function(date_obs_end) {
    if (!identical(length(date_obs_end), 1L))
        stop(gettextf("'%s' does not have length %d",
                      "date_obs_end", 1L))
    if (is.na(date_obs_end))
        stop(gettextf("'%s' is missing",
                      "date_obs_end"))
    if (is.character(date_obs_end)) {
        date_obs_end <- tryCatch(as.Date(date_obs_end),
                                 error = function(e) e)
        if (methods::is(date_obs_end, "error"))
            stop(gettextf("problem coercing '%s' to class \"%s\" : %s",
                          "date_obs_end", "Date", date_obs_end$message))
    }
    else {
        if (!methods::is(date_obs_end, "Date"))
            stop(gettextf("'%s' has class \"%s\"",
                          "date_obs_end", class(date_obs_end)))
    }
    date_obs_end
}

check_and_tidy_days_in_country <- function(days_in_country) {
    if (any(is.na(days_in_country)))
        stop(gettextf("'%s' has missing values",
                      "days_in_country"))
    if (!is.numeric(days_in_country))
        stop(gettextf("'%s' is non-numeric",
                      "days_in_country"))
    days_in_country_int <- as.integer(days_in_country)
    if (any(days_in_country_int != days_in_country))
        stop(gettextf("'%s' has non-integer values",
                      "days_in_country"))
    days_in_country <- days_in_country_int
    if (any(days_in_country < 0L))
        stop(gettextf("'%s' has negative values",
                      "days_in_country"))
    days_in_country
}

check_and_tidy_days_obs <- function(days_obs) {
    if (any(is.na(days_obs)))
        stop(gettextf("'%s' has missing values",
                      "days_obs"))
    if (!is.numeric(days_obs))
        stop(gettextf("'%s' is non-numeric",
                      "days_obs"))
    days_obs_int <- as.integer(days_obs)
    if (any(days_obs_int != days_obs))
        stop(gettextf("'%s' has non-integer values",
                      "days_obs"))
    days_obs <- days_obs_int
    if (any(days_obs < 0L))
        stop(gettextf("'%s' has negative values",
                      "days_obs"))
    days_obs
}


check_and_tidy_dur <- function(dur, name = c("dur_test", "dur_threshold")) {
    name <- match.arg(name)
    if (!identical(length(dur), 1L))
        stop(gettextf("'%s' does not have length %d",
                      name, 1L))
    if (is.na(dur))
        stop(gettextf("'%s' is missing",
                      name))
    if (methods::is(dur, "difftime"))
        dur <- as.integer(dur)
    else if (lubridate::is.duration(dur))
        dur <- as.integer(dur / lubridate::ddays(1))
    else if (is.numeric(dur)) {
        dur_int <- as.integer(dur)
        if (!isTRUE(all.equal(dur_int, dur)))
            stop(gettextf("'%s' is not an integer",
                          name))
        dur <- dur_int
    }
    else
        stop(gettextf("'%s' has class \"%s\"",
                      name, class(dur)))
    dur
}

check_and_tidy_res_status <- function(res_status, name = "res_status_after") {
    if (!is.numeric(res_status))
        stop(gettextf("'%s' is non-numeric",
                      name))
    if (!all(res_status[!is.na(res_status)] %in% c(0L, 1L)))
        stop(gettextf("'%s' has values other than 0, 1, NA",
                      name))
    as.integer(res_status)
}

check_and_tidy_res_status_initial <- function(res_status_initial) {
    if (any(is.na(res_status_initial)))
        stop(gettextf("'%s' has missing values",
                      "res_status_initial"))
    if (!is.numeric(res_status_initial))
        stop(gettextf("'%s' is non-numeric",
                      "res_status_initial"))
    if (!all(res_status_initial %in% c(0L, 1L)))
        stop(gettextf("'%s' has values other than 0, 1",
                      "res_status_initial"))
    as.integer(res_status_initial)
}



check_date_crossing_and_person_id <- function(date_crossing, personId) {
    if (is.character(date_crossing))
        date_crossing <- as.Date(date_crossing)
    date_crossing <- as.integer(date_crossing)
    l <- split(x = date_crossing,
               f = personId)
    is_nondecreasing <- function(x) {
        all(diff(x) >= 0L)
    }
    is_valid <- sapply(l, is_nondecreasing)
    if (!all(is_valid))
        stop(gettextf("elements of '%s' for '%s' equal to '%s' not ordered correctly",
                      "date_crossing", "personId", unique(personId)[!is_valid][1L]))
    NULL
}


check_days_in_country_and_days_obs <- function(days_in_country, days_obs) {
    gt_days_obs <- days_in_country > days_obs
    if (any(gt_days_obs))
        stop(gettextf("element of '%s' [%s] greater than corresponding element of '%s' [%s]",
                      "days_in_country",
                      days_in_country[gt_days_obs][1L],
                      "days_obs",
                      days_obs[gt_days_obs][1L]))
    NULL
}


check_days_in_country_and_dur_test <- function(days_in_country, dur_test) {
    gt_dur_test <- days_in_country > dur_test
    if (any(gt_dur_test))
        stop(gettextf("element of '%s' [%s] greater than '%s' [%s]",
                      "days_in_country",
                      days_in_country[gt_dur_test][1L],
                      "dur_test",
                      dur_test))
    NULL
}

check_dur_test_and_dur_threshold <- function(dur_test, dur_threshold) {
    if (dur_threshold > dur_test)
        stop(gettextf("'%s' is greater than '%s'",
                      "dur_threshold", "dur_test"))
    if (dur_threshold <= dur_test / 2)
        stop(gettextf("'%s' less than or equal to half of '%s'",
                      "dur_threshold", "dur_test"))
    NULL
}


check_is_arrival <- function(is_arrival) {
    if (is.numeric(is_arrival)) {
        if (!all(is_arrival %in% c(0L, 1L)))
            stop(gettextf("'%s' is numeric, but has values not equal to 0, 1",
                          "is_arrival"))
    }
    else {
        if (!is.logical(is_arrival))
            stop(gettextf("'%s' has class \"%s\"",
                          "is_arrival", class(is_arrival)))
    }
    if (any(is.na(is_arrival)))
        stop(gettextf("'%s' has missing values",
                      "is_arrival"))
    NULL
}

check_is_arrival_and_res_status_before_and_res_status_after <- function(is_arrival, res_status_before,
                                                                        res_status_after) {
    is_arrival <- as.integer(is_arrival)
    ## arriving cannot change residence status from 1 to 0
    is_invalid_arrival <- ((is_arrival == 1L)
        & !is.na(res_status_before)
        & (res_status_before == 1L)
        & !is.na(res_status_after)
        & (res_status_after == 0L))
    if (any(is_invalid_arrival))
        stop(gettextf("border crossing is arrival, but '%s' equals %d and '%s' equals %d",
                      "res_status_before", 1L, "res_status_after", 0L))
    ## departing cannot change residence status from 0 to 1
    is_invalid_departure <- ((is_arrival == 0L)
        & !is.na(res_status_before)
        & (res_status_before == 0L)
        & !is.na(res_status_after)
        & (res_status_after == 1L))
    if (any(is_invalid_departure))
        stop(gettextf("border crossing is departure, but '%s' equals %d and '%s' equals %d",
                      "res_status_before", 0L, "res_status_after", 1L))
    NULL
}

## HAS_TESTS
check_n_core <- function(n_core) {
    check_positive_number(n_core, name = "n_core")
    if (n_core != round(n_core))
        stop(gettextf("'%s' is not an integer",
                      "n_core"))
    NULL
}

## HAS_TESTS
check_parallel <- function(parallel) {
    if (!identical(length(parallel), 1L))
        stop(gettextf("'%s' does not have length %d",
                      "parallel", 1L))
    if (!is.logical(parallel))
        stop(gettextf("'%s' does not have type \"%s\"",
                      "parallel", "logical"))
    NULL
}

check_person_id <- function(personId) {
    if (any(is.na(personId)))
        stop(gettextf("'%s' has missing values",
                      "personId"))
    values <- rle(personId)$values
    if (any(duplicated(values)))
        stop(gettextf("'%s' not ordered correctly",
                      "personId"))
    NULL
}

check_res_status_str_date_crossing <- function(res_status_before_str,
                                               res_status_after_str,
                                               date_crossing) {
    if (is.null(res_status_before_str) && is.null(res_status_after_str))
        NULL ## nothing to check
    else if (!is.null(res_status_before_str) && is.null(res_status_after_str))
        stop(gettextf("'%s' is %s but '%s' is not %s",
                      "res_status_after_str", "NULL", "res_status_before_str", "NULL"))
    else if (is.null(res_status_before_str) && !is.null(res_status_after_str))
        stop(gettextf("'%s' is %s but '%s' is not %s",
                      "res_status_before_str", "NULL", "res_status_after_str", "NULL"))
    else {
        if (!identical(length(res_status_before_str), length(res_status_after_str)))
            stop(gettextf("'%s' and '%s' have different lengths",
                          "res_status_before_str", "res_status_after_str"))
        if (!identical(length(res_status_before_str), length(date_crossing)))
            stop(gettextf("'%s' and '%s' have different lengths",
                          "res_status_before_str", "date_crossing"))
    }
    NULL
}



check_res_status_and_is_arrival_and_days_in_country <- function(res_status_before, res_status_after,
                                                                is_arrival, days_in_country,
                                                                dur_test, dur_threshold) {
    is_invalid_0 <- (!is.na(res_status_after) &
                     (res_status_after == 0L) &
                     (!(!is_arrival & (res_status_before == 0L))) &
                     (days_in_country >= dur_threshold))
    if (any(is_invalid_0))
        stop(gettextf("'%s' is %d but '%s' [%d] greater than '%s' [%d]",
                      "res_status_after",
                      0L,
                      "days_in_country",
                      days_in_country[is_invalid_0][1L],
                      "dur_threshold",
                      dur_threshold))
    is_invalid_1 <- (!is.na(res_status_after) &
                     (res_status_after == 1L) &
                     (!(is_arrival & (res_status_before == 1L))) &
                     (days_in_country < (dur_test - dur_threshold)))
    if (any(is_invalid_1))
        stop(gettextf("'%s' is %d but '%s' [%d] less than '%s' minus '%s' [%d]",
                      "res_status_after",
                      1L,
                      "days_in_country",
                      days_in_country[is_invalid_1][1L],
                      "dur_test",
                      "dur_threshold",
                      dur_test - dur_threshold))
    NULL
}




## HAS_TESTS
check_res_status_after_and_person_id <- function(res_status_after, personId) {
    l <- split(x = res_status_after,
               f = personId)
    consistent_na <- function(x)
        all(diff(is.na(x)) >= 0L)
    is_valid <- sapply(l, consistent_na)
    if (!all(is_valid))
        stop(gettextf("values of '%s' for '%s' equal to \"%s\" switch from missing back to non-missing",
                      "res_status_after", "personId", unique(personId)[!is_valid][1L]))
    NULL
}


check_res_status_before_and_person_id <- function(res_status_before, personId) {
    l <- split(x = res_status_before,
               f = personId)
    first_not_missing <- function(x)
        !is.na(x[[1L]])
    is_valid <- sapply(l, first_not_missing)
    if (!all(is_valid))
        stop(gettextf("first value of '%s' for '%s' equal to \"%s\" is missing",
                      "res_status_before", "personId", unique(personId)[!is_valid][1L]))
    NULL
}


check_res_status_before_and_res_status_after <- function(res_status_before, res_status_after) {
    switch_from_na <- is.na(res_status_before) & !is.na(res_status_after)
    if (any(switch_from_na))
        stop(gettextf("value of '%s' non-missing even though value for '%s' missing",
                      "res_status_after", "res_status_before"))
    NULL
}


check_res_status_initial_and_person_id <- function(res_status_initial, personId) {
    l <- split(x = res_status_initial,
               f = personId)
    repeats_correctly <- function(x) {
        all(x[-1L] == x[1L]) # works with length 1
    }
    is_valid <- sapply(l, repeats_correctly)
    if (!all(is_valid))
        stop(gettextf("values of '%s' not all identical for '%s' equal to \"%s\"",
                      "res_status_initial", "personId", unique(personId)[!is_valid][1L]))
    NULL
}

## HAS_TESTS
check_res_status_initial_and_res_status_after <- function(res_status_initial, res_status_after) {
    switch_from_na <- is.na(res_status_initial) & !is.na(res_status_after)
    if (any(switch_from_na))
        stop(gettextf("value of '%s' non-missing even though value for '%s' missing",
                      "res_status_after", "res_status_initial"))
    NULL
}



check_same_length <- function(e1, e2, name1, name2) {
    if (!identical(length(e1), length(e2)))
        stop(gettextf("'%s' and '%s' have different lengths",
                      name1, name2))
    NULL
}




check_positive_number <- function(number, name) {
    if (!identical(length(number), 1L))
        stop(gettextf("'%s' does not have length %d",
                      name, 1L))
    if (!is.numeric(number))
        stop(gettextf("'%s' is non-numeric",
                      name))
    if (is.na(number))
        stop(gettextf("'%s' is missing",
                      name))
    if (number <= 0)
        stop(gettextf("'%s' is non-positive",
                      name))
    NULL
}






















