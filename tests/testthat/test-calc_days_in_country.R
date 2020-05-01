################################################################################
context("calc_days_in_country")
################################################################################

## number of iterations for tests
long_tests <- FALSE

if(long_tests) {
  num_iter <- 500
  num_persons <- 20
  num_crossings <- 100
} else {
    num_iter <- 10
    num_persons <- 5
    num_crossings <- 20
}


## three person IDs:
## 1) date_obs_end > last date_crossing
test_that("calc_days_in_country works correctly when date_obs_end > last date_crossing", {
  personId <- c(2, 2, 2, 1, 1, 1, 3, 3, 3)  # personId needs to be grouped
  date_crossing <- as.Date(rep(c("2000-01-05", "2000-02-01", "2000-03-10"),
                               times = 3))
  is_arrival <- rep(c(FALSE, TRUE, FALSE), times = 3)
  date_obs_end <- as.Date("2000-12-31")
  dur_test <- as.Date("2001-04-01") - as.Date("2000-01-01")  # 456 days

  ## parallel, n_core = 1
  ans_obtained <- calc_days_in_country(personId = personId,
                                       date_crossing = date_crossing,
                                       is_arrival = is_arrival,
                                       date_obs_end = date_obs_end,
                                       dur_test = dur_test,
                                       n_core = 1)
  ans_expected <- as.integer(rep(c(date_crossing[3] - date_crossing[2],
                                   date_crossing[3] - date_crossing[2],
                                   0),
                                 times = 3))
  expect_identical(ans_obtained, ans_expected)

  ## parallel, n_core = 2
  ans_obtained <- calc_days_in_country(personId = personId,
                                       date_crossing = date_crossing,
                                       is_arrival = is_arrival,
                                       date_obs_end = date_obs_end,
                                       dur_test = dur_test,
                                       n_core = 2)
  ans_expected <- as.integer(rep(c(date_crossing[3] - date_crossing[2],
                                   date_crossing[3] - date_crossing[2],
                                   0),
                                 times = 3))
  expect_identical(ans_obtained, ans_expected)

  ## non-parallel
  ans_obtained <- calc_days_in_country(personId = personId,
                                       date_crossing = date_crossing,
                                       is_arrival = is_arrival,
                                       date_obs_end = date_obs_end,
                                       dur_test = dur_test,
                                       parallel = FALSE)
  ans_expected <- as.integer(rep(c(date_crossing[3] - date_crossing[2],
                                   date_crossing[3] - date_crossing[2],
                                   0),
                                 times = 3))
  expect_identical(ans_obtained, ans_expected)
})


## 2) date_obs_end < first date_crossing
test_that("calc_days_in_country works correctly when date_obs_end < first date_crossing", {
  personId <- c(2, 2, 2, 1, 1, 1, 3, 3, 3)  # personId needs to be grouped
  date_crossing <- as.Date(rep(c("2000-01-05", "2000-02-01", "2000-03-10"),
                               times = 3))
  date_obs_end <- as.Date("1999-12-01")  # set < earliest date_crossing
  dur_test <- as.Date("2001-04-01") - as.Date("2000-01-01")  # 456 days

  ## departure-arrival-departure
  is_arrival <- rep(c(FALSE, TRUE, FALSE), times = 3)
  ans_obtained <- calc_days_in_country(personId = personId,
                                       date_crossing = date_crossing,
                                       is_arrival = is_arrival,
                                       date_obs_end = date_obs_end,
                                       dur_test = dur_test)
  ans_expected <- as.integer(rep(c(0, 0, 0), times = 3))
  expect_identical(ans_obtained, ans_expected)

  ## arrival-departure-arrival
  is_arrival <- rep(c(TRUE, FALSE, TRUE), times = 3)
  ans_obtained <- calc_days_in_country(personId = personId,
                                       date_crossing = date_crossing,
                                       is_arrival = is_arrival,
                                       date_obs_end = date_obs_end,
                                       dur_test = dur_test)
  ans_expected <- as.integer(rep(c(0, 0, 0), times = 3))
  expect_identical(ans_obtained, ans_expected)
})


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## dealing with other valid input patterns
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## for crossings with dates after date_obs_end, calc_days_in_country() returns 0s
test_that("calc_days_in_country works correctly with date_obs_end <= some date_crossing", {
  personId <- c(1, 1, 1, 1)
  date_crossing <- as.Date(c("2000-01-01", "2000-01-05", "2000-02-01", "2000-03-31"))
  date_obs_end <- as.Date("2000-03-01")
  dur_test <- as.Date("2001-04-01") - as.Date("2000-01-01")  # 456 days

  ## 1) last crossing is a departure
  is_arrival <- c(TRUE, FALSE, TRUE, FALSE)
  ans_obtained <- calc_days_in_country(personId = personId,
                                       date_crossing = date_crossing,
                                       is_arrival = is_arrival,
                                       date_obs_end = date_obs_end,
                                       dur_test = dur_test)
  ans_expected <- as.integer(c((date_crossing[2] - date_crossing[1]) +
                                 (date_obs_end - date_crossing[3]),
                               date_obs_end - date_crossing[3],
                               date_obs_end - date_crossing[3],
                               0))
  expect_identical(ans_obtained, ans_expected)

  ## 2) last crossing is an arrival
  is_arrival <- c(FALSE, TRUE, FALSE, TRUE)
  ans_obtained <- calc_days_in_country(personId = personId,
                                       date_crossing = date_crossing,
                                       is_arrival = is_arrival,
                                       date_obs_end = date_obs_end,
                                       dur_test = dur_test)
  ans_expected <- as.integer(c(date_crossing[3] - date_crossing[2],
                               date_crossing[3] - date_crossing[2],
                               0,
                               0))
  expect_identical(ans_obtained, ans_expected)
})


## when distance between crossing dates (for a passenger) are greater than dur_test,
## then calc_days_in_country() truncates
test_that("calc_days_in_country works correctly with spells between crossings > dur_test", {
  personId <- c(1, 1, 1, 1, 1, 1)
  date_crossing <- as.Date(c("2000-06-01", "2000-07-01", "2002-01-01",
                             "2002-04-15", "2002-12-31", "2004-05-20"))
  date_obs_end <- as.Date("2004-06-30")
  dur_test <- 480

  ## 1) travel sequence starting with arrival
  is_arrival <- c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE)
  ans_obtained <- calc_days_in_country(personId = personId,
                                       date_crossing = date_crossing,
                                       is_arrival = is_arrival,
                                       date_obs_end = date_obs_end,
                                       dur_test = dur_test)
  ans_expected <- as.integer(c((date_crossing[2] - date_crossing[1]),
                               0,
                               dur_test - (date_crossing[5] - date_crossing[4]),
                               dur_test - (date_crossing[5] - date_crossing[4]),
                               dur_test,
                               0))
  expect_identical(ans_obtained, ans_expected)

  ## 2) travel sequence starting with departure
  is_arrival <- c(FALSE, TRUE, FALSE, TRUE, FALSE, TRUE)
  ans_obtained <- calc_days_in_country(personId = personId,
                                       date_crossing = date_crossing,
                                       is_arrival = is_arrival,
                                       date_obs_end = date_obs_end,
                                       dur_test = dur_test)
  ans_expected <- as.integer(c(dur_test - (date_crossing[2] - date_crossing[1]),
                               dur_test,
                               date_crossing[5] - date_crossing[4],
                               date_crossing[5] - date_crossing[4],
                               0,
                               date_obs_end - date_crossing[6]))
  expect_identical(ans_obtained, ans_expected)
})


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## calc_days_in_country() tested against calc_days_in_country2() with randomly
## generated input
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## function: alternate implementation of calc_days_in_country(), written by
## tester from definition
calc_days_in_country2 <- function(personId, date_crossing,
                                  is_arrival,
                                  date_obs_end, dur_test) {
  num_crossings <- length(date_crossing)
  days_in_country <- integer(num_crossings)
  ## end of observation window for each crossing
  obs_window_end <- pmin(date_crossing + dur_test, date_obs_end)
  ## computing days_in_country:
  for(j in personId) {
    person_j <- (personId == j)
    num_crossings_j <- sum(person_j)
    days_in_country_j <- integer(num_crossings_j)  # initialize
    is_arrival_j = is_arrival[person_j]
    obs_window_end_j <- obs_window_end[person_j]
    date_crossing_j = date_crossing[person_j]
    if(num_crossings_j == 1) {
      days_in_country_j <- max(0, obs_window_end_j - date_crossing_j) *
                           as.integer(is_arrival_j)
    } else {
        for(i in 1:(num_crossings_j - 1)) {
          if(date_crossing_j[i] >= obs_window_end_j[i]) {
            days_in_country_j[i] <- 0L
          } else {
              days_in_country_j[i] <- 0L
              k <- i + 1
              while(date_crossing_j[k] <= obs_window_end_j[i] & k <= num_crossings_j) {
              days_in_country_j[i] <- days_in_country_j[i] +
                        as.integer(date_crossing_j[k] - date_crossing_j[k-1]) *
                        as.integer(is_arrival_j[k-1])
              k <- k + 1
              }
              ## add the last spell, which may be censored
              days_in_country_j[i] <- days_in_country_j[i] +
                max(0, as.integer(obs_window_end_j[i] - date_crossing_j[k - 1])) *
                as.integer(is_arrival_j[k-1])
          }
        }
        ## calculate days spent in country for the last crossing separately:
        days_in_country_j[num_crossings_j] <- max(0, obs_window_end_j[num_crossings_j] -
                                                     date_crossing_j[num_crossings_j]) *
                                              as.integer(is_arrival_j[num_crossings_j])
    }
    days_in_country[person_j] <- days_in_country_j
  }
  return(as.integer(days_in_country))
}


## function: generates test input data for calc_days_in_country()
generator_for_calc_days_in_country <- function(num_persons, num_crossings,
                                               period_start, period_end) {
  if(num_crossings < num_persons)
    stop("number of crossings cannot be less than number of passengers")
  personId <- sort(sample(num_persons, num_crossings, replace = TRUE))
  dates_set <- seq(as.Date(period_start), as.Date(period_end), by = 1)
  date_crossing <- rep(as.Date(period_start), num_crossings)  # just initializing
  is_arrival <- NULL
  date_obs_end <- sample(dates_set, 1)
  dur_test <- sample(seq(1, 500, 1), 1)
  for(j in personId) {
    person_j <- (personId == j)
    num_crossings_j <- sum(person_j)
    ## direction
    is_arrival_j <- sample(c(T, F), 1)  # direction of 1st crossing
    if(num_crossings_j > 1) {
      for(i in 2:num_crossings_j)
        is_arrival_j[i] <- !is_arrival_j[i-1]
    }
    is_arrival[person_j] <- is_arrival_j
    ## date_crossing
    date_crossing[person_j] <- sort(sample(dates_set, num_crossings_j,
                                           replace = TRUE))
  }
  return(list("personId" = personId,
              "date_crossing" = as.Date(date_crossing),
              "is_arrival" = is_arrival,
              "date_obs_end" = date_obs_end,
              "dur_test" = dur_test))
}


## we randomly generate test input data and test whether both implementations of
## function to calculate days spent in country return the same output
test_that("calc_days_in_country runs as expected", {
  for(iteration in 1:num_iter) {  # set number of iterations
    test_input <- generator_for_calc_days_in_country(num_persons = num_persons,
                                                  num_crossings = num_crossings,
	                                                period_start = "2015-01-01",
											                            period_end = "2018-06-30")
    ans_obtained <- calc_days_in_country(personId = test_input$personId,
                                       date_crossing = test_input$date_crossing,
                                       is_arrival = test_input$is_arrival,
                                       date_obs_end = test_input$date_obs_end,
                                       dur_test = test_input$dur_test)
    ans_expected <- calc_days_in_country2(personId = test_input$personId,
                                       date_crossing = test_input$date_crossing,
                                       is_arrival = test_input$is_arrival,
                                       date_obs_end = test_input$date_obs_end,
                                       dur_test = test_input$dur_test)
	  expect_identical(ans_obtained, ans_expected)
  }
})


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## calc_days_in_country() imputes when a crossing is missing
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## the logical vector is_arrival needs to be alternating
## if not, then calc_days_in_country() will impute the missing crossing in the
## middle before computing days spent in country

## missing departures
test_that("calc_days_in_country imputes correctly with two successive arrivals", {
  personId <- c(1, 1, 1)
  date_crossing <- as.Date(c("2000-01-05", "2000-02-01", "2000-03-10"))
  is_arrival <- c(FALSE, TRUE, TRUE)  # imputed = c(FALSE, TRUE, FALSE, TRUE)
  date_obs_end <- as.Date("2000-12-31")
  dur_test <- as.Date("2001-04-01") - as.Date("2000-01-01")  # 456 days
  ans_obtained <- calc_days_in_country(personId = personId,
                                       date_crossing = date_crossing,
                                       is_arrival = is_arrival,
                                       date_obs_end = date_obs_end,
                                       dur_test = dur_test)
  ans_expected <- c(as.integer(date_crossing[3] - date_crossing[2]) %/% 2L,
                    as.integer(date_crossing[3] - date_crossing[2]) %/% 2L,
                    0L) +
                  as.integer(date_obs_end - date_crossing[3])
  expect_identical(ans_obtained, ans_expected)

  ## multiple missing departures
  personId <- c(1, 1, 1)
  date_crossing <- as.Date(c("2000-01-01", "2000-02-10", "2000-03-01"))
  is_arrival <- c(TRUE, TRUE, TRUE)
  ans_obtained <- calc_days_in_country(personId = personId,
                                       date_crossing = date_crossing,
                                       is_arrival = is_arrival,
                                       date_obs_end = date_obs_end,
                                       dur_test = dur_test)
  ans_expected <- c(ceiling(as.numeric(date_crossing[2]-date_crossing[1]) / 2) +
                      ceiling(as.numeric(date_crossing[3]-date_crossing[2]) %/% 2),
                    ceiling(as.numeric(date_crossing[3]-date_crossing[2]) %/% 2),
                    0L) +
                  as.integer(date_obs_end - date_crossing[3])
  expect_identical(ans_obtained, as.integer(ans_expected))
})


## missing arrivals
test_that("calc_days_in_country imputes correctly with two successive departures", {
  personId <- c(1, 1, 1)
  date_crossing <- as.Date(c("2000-01-05", "2000-02-01", "2000-03-10"))
  is_arrival <- c(TRUE, FALSE, FALSE)  # imputed = c(TRUE, FALSE, TRUE, FALSE)
  date_obs_end <- as.Date("2000-12-31")
  dur_test <- as.Date("2001-04-01") - as.Date("2000-01-01")  # 456 days
  ans_obtained <- calc_days_in_country(personId = personId,
                                       date_crossing = date_crossing,
                                       is_arrival = is_arrival,
                                       date_obs_end = date_obs_end,
                                       dur_test = dur_test)
  ans_expected <- c(as.integer(date_crossing[2] - date_crossing[1]) +
                      as.integer(date_crossing[3] - date_crossing[2]) %/% 2L,
                    as.integer(date_crossing[3] - date_crossing[2]) %/% 2L,
                    0L)
  expect_identical(ans_obtained, ans_expected)

  ## multiple missing arrivals
  personId <- c(1, 1, 1)
  date_crossing <- as.Date(c("2000-01-01", "2000-02-10", "2000-03-01"))
  is_arrival <- c(FALSE, FALSE, FALSE)
  ans_obtained <- calc_days_in_country(personId = personId,
                                       date_crossing = date_crossing,
                                       is_arrival = is_arrival,
                                       date_obs_end = date_obs_end,
                                       dur_test = dur_test)
  ans_expected <- c(ceiling(as.numeric(date_crossing[2]-date_crossing[1]) / 2) +
                      ceiling(as.numeric(date_crossing[3]-date_crossing[2]) %/% 2),
                    ceiling(as.numeric(date_crossing[3]-date_crossing[2]) %/% 2),
                    0)
  expect_identical(ans_obtained, as.integer(ans_expected))
})


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## tests for classes of input vectors
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

test_that("calc_days_in_country deals correctly with dur_test of class duration", {
  personId <- c(1, 1, 1)
  date_crossing <- as.Date(c("2000-01-05", "2000-02-01", "2000-03-10"))
  is_arrival <- c(FALSE, TRUE, FALSE)
  date_obs_end <- as.Date("2000-12-31")
  dur_test <- lubridate::duration(num = 456, units = "days") # class duration
  ans_obtained <- calc_days_in_country(personId = personId,
                                       date_crossing = date_crossing,
                                       is_arrival = is_arrival,
                                       date_obs_end = date_obs_end,
                                       dur_test = dur_test)
  ans_expected <- as.integer(c(date_crossing[3] - date_crossing[2],
                               date_crossing[3] - date_crossing[2],
                               0))
  expect_identical(ans_obtained, ans_expected)
})


## dur_test cannot be of class character
test_that("calc_days_in_country returns correct error when dur_test is of class character", {
  personId <- c(1, 1, 1)
  date_crossing <- as.Date(c("2000-01-05", "2000-02-01", "2000-03-10"))
  is_arrival <- c(FALSE, TRUE, FALSE)
  date_obs_end <- as.Date("2000-12-31")
  dur_test <- "456"
  expect_error(calc_days_in_country(personId = personId,
                                    date_crossing = date_crossing,
                                    is_arrival = is_arrival,
                                    date_obs_end = date_obs_end,
                                    dur_test = dur_test),
               "'dur_test' has class \"character\"")
})


## calc_days_in_country() coerces dates from class character to class Date
test_that("calc_days_in_country can handle date_crossing of class character", {
  personId <- c(1, 1, 1)
  date_crossing <- c("2000-01-05", "2000-02-01", "2000-03-10")
  is_arrival <- c(FALSE, TRUE, FALSE)
  date_obs_end <- as.Date("2000-12-31")
  dur_test <- 456
  ans_obtained <- calc_days_in_country(personId = personId,
                                       date_crossing = date_crossing,
                                       is_arrival = is_arrival,
                                       date_obs_end = date_obs_end,
                                       dur_test = dur_test)
  date_crossing <- as.Date(date_crossing)
  ans_expected <- as.integer(c(date_crossing[3] - date_crossing[2],
                               date_crossing[3] - date_crossing[2],
                               0))
  expect_identical(ans_obtained, ans_expected)
})


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## invalid input patterns
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## for calc_days_in_country(), date_crossing needs to be ordered within each person ID
test_that("calc_days_in_country produces error when date_crossing is not ordered", {
  personId <- c(1, 1, 1, 2, 2)
  date_crossing <- as.Date(c("2000-01-05", "2000-02-01", "2000-03-10",
                             "2000-04-03", "2000-01-25"))
  is_arrival <- c(FALSE, TRUE, FALSE, FALSE, TRUE)
  date_obs_end <- as.Date("2000-12-31")
  dur_test <- as.Date("2001-04-01") - as.Date("2000-01-01")
  expect_error(calc_days_in_country(personId = personId,
                                    date_crossing = date_crossing,
                                    is_arrival = is_arrival,
                                    date_obs_end = date_obs_end,
                                    dur_test = dur_test),
    "elements of 'date_crossing' for 'personId' equal to '2' not ordered correctly")
})


## personId needs to be grouped
test_that("calc_days_in_country produces error when personId is not grouped", {
  personId <- c(1, 2, 2, 3, 1)
  date_crossing <- as.Date(c("2000-08-05", "2000-02-01", "2000-03-11",
                             "2000-08-09", "2000-01-09"))
  is_arrival <- c(TRUE, TRUE, FALSE, TRUE, FALSE)
  date_obs_end <- as.Date("2000-12-31")
  dur_test <- as.Date("2001-04-01") - as.Date("2000-01-01")
  expect_error(calc_days_in_country(personId = personId,
                                    date_crossing = date_crossing,
                                    is_arrival = is_arrival,
                                    date_obs_end = date_obs_end,
                                    dur_test = dur_test),
               "'personId' not ordered correctly")

  ## when personId is re-ordered correctly (1st record moved to the end)
  personId <- c(2, 2, 3, 1, 1)
  date_crossing <- as.Date(c("2000-02-01", "2000-03-11", "2000-08-09",
                             "2000-01-09", "2000-08-05"))
  is_arrival <- c(TRUE, FALSE, TRUE, FALSE, TRUE)
  ans_obtained <- calc_days_in_country(personId = personId,
                                       date_crossing = date_crossing,
                                       is_arrival = is_arrival,
                                       date_obs_end = date_obs_end,
                                       dur_test = dur_test)
  ans_expected <- as.integer(c(date_crossing[2] - date_crossing[1], 0,
                               min(dur_test, date_obs_end - date_crossing[3]),
                               min(dur_test, date_obs_end - date_crossing[5]),
                               min(dur_test, date_obs_end - date_crossing[5])))
  expect_identical(ans_obtained, ans_expected)
})


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## NA or blank input values
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## NA crossing dates
test_that("calc_days_in_country produces error when date_crossing has NAs", {
  personId <- c(1, 1, 1)
  date_crossing <- c("2000-01-05", "2000-02-01", NA)
  is_arrival <- c(TRUE, FALSE, TRUE)
  date_obs_end <- as.Date("2000-12-31")
  dur_test <- as.Date("2001-04-01") - as.Date("2000-01-01")
  expect_error(calc_days_in_country(personId = personId,
                                    date_crossing = date_crossing,
                                    is_arrival = is_arrival,
                                    date_obs_end = date_obs_end,
                                    dur_test = dur_test),
               "'date_crossing' has missing values")
})


## blank crossing dates
test_that("calc_days_in_country produces error when date_crossing has blanks", {
  personId <- c(1, 1, 1)
  date_crossing <- c("2000-01-05", "2000-02-01", "")
  is_arrival <- c(TRUE, FALSE, TRUE)
  date_obs_end <- as.Date("2000-12-31")
  dur_test <- as.Date("2001-04-01") - as.Date("2000-01-01")
  expect_error(calc_days_in_country(personId = personId,
                                    date_crossing = date_crossing,
                                    is_arrival = is_arrival,
                                    date_obs_end = date_obs_end,
                                    dur_test = dur_test),
               "'date_crossing' contains invalid date")
})






















