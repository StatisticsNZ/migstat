################################################################################
context("calc_res_status_after")
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


## ID 1, travel history spells are c(10, 190); days obs. are c(200, 190)
## ID 2, travel history spells are c(20, 5, 110, 365); days obs. are c(500, 480, 475, 365)
## ID 3, travel history spell is c(365); days obs. is c(365)
## ID 4, travel history spell is c(365); days obs. is c(365)
test_that("calc_res_status_after works with valid inputs", {
  personId <- c(1, 1, 2, 2, 2, 2, 3, 4)
  is_arrival <- c(TRUE, FALSE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE)
  days_in_country <- c(10, 0, 350, 370, 365, 365, 0, 365)
  days_obs <- c(200, 190, 480, 480, 475, 365, 365, 365)  # days_obs[3] is min(500, dur_test)
  res_status_initial <- c(0, 0, 1, 1, 1, 1, 1, 0)
  dur_test <- 480
  dur_threshold <- 365

  ## parallel - n_core = 1
  ans_obtained <- calc_res_status_after(personId = personId,
                                        is_arrival = is_arrival,
                                        days_in_country = days_in_country,
                                        days_obs = days_obs,
                                        res_status_initial = res_status_initial,
                                        dur_test = dur_test,
                                        dur_threshold = dur_threshold,
                                        n_core = 1)
  ans_expected <- as.integer(c(0, 0, 1, 1, 1, 1, 0, 1))
  expect_identical(ans_obtained, ans_expected)

  ## parallel - n_core = 2
  ans_obtained <- calc_res_status_after(personId = personId,
                                        is_arrival = is_arrival,
                                        days_in_country = days_in_country,
                                        days_obs = days_obs,
                                        res_status_initial = res_status_initial,
                                        dur_test = dur_test,
                                        dur_threshold = dur_threshold,
                                        n_core = 2)
  ans_expected <- as.integer(c(0, 0, 1, 1, 1, 1, 0, 1))
  expect_identical(ans_obtained, ans_expected)

  ## non-parallel
  ans_obtained <- calc_res_status_after(personId = personId,
                                        is_arrival = is_arrival,
                                        days_in_country = days_in_country,
                                        days_obs = days_obs,
                                        res_status_initial = res_status_initial,
                                        dur_test = dur_test,
                                        dur_threshold = dur_threshold,
                                        parallel = FALSE)
  ans_expected <- as.integer(c(0, 0, 1, 1, 1, 1, 0, 1))
  expect_identical(ans_obtained, ans_expected)
})


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## calc_res_status_after() tested against calc_res_status_after2() with randomly
## generated input
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## function: alternate implementation of calc_res_status_after(), written by
## tester from definition
calc_res_status_after2 <- function(personId, is_arrival,
                                   days_in_country, days_obs,
                                   res_status_initial,
                                   dur_test, dur_threshold) {
  res_status_after <- integer(length(personId))
  W <- dur_test
  V <- dur_threshold
  for(j in personId) {
    person_j <- (personId == j)
    num_crossings_j <- sum(person_j)
    is_arrival_j = is_arrival[person_j]
    days_in_country_j = days_in_country[person_j]
    days_obs_j = days_obs[person_j]
    res_status_initial_j = res_status_initial[person_j]
    ## calculate res_status_after
    res_status_after_j <- integer(num_crossings_j)
    for(i in 1:num_crossings_j) {
      res_status_before <- ifelse(i > 1, res_status_after_j[i-1],
                                         res_status_initial_j[1])
      if(is.na(res_status_before)) {
          res_status_after_j[i] <- NA
      } else if(res_status_before == 1) {  # tree 1:
               if(is_arrival_j[i]) {
                 res_status_after_j[i] <- 1
               } else {
                   if(days_obs_j[i] < W - V) {
                     res_status_after_j[i] <- NA
                   } else {
                       if(days_in_country_j[i] > W - V) {
                         res_status_after_j[i] <- 1
                       } else {
                           if(days_obs_j[i] >= W) {
                             res_status_after_j[i] <- 0
                           } else {
                              if(days_obs_j[i] - days_in_country_j[i] >= V) {
                                res_status_after_j[i] <- 0
                              } else res_status_after_j[i] <- NA
                           }
                       }
                   }
               }
      } else {  # tree 2:
          if(!is_arrival_j[i]) {
            res_status_after_j[i] <- 0
          } else {
              if(days_obs_j[i] < W - V) {
                res_status_after_j[i] <- NA
              } else {
                  if(days_obs_j[i] - days_in_country_j[i] > W - V) {
                         res_status_after_j[i] <- 0
                  } else {
                      if(days_obs_j[i] >= W) {
                        res_status_after_j[i] <- 1
                      } else {
                          if(days_in_country_j[i] >= V) {
                            res_status_after_j[i] <- 1
                          } else res_status_after_j[i] <- NA
                      }
                  }
              }
          }
      }
    }
    res_status_after[person_j] <- res_status_after_j
  }
  return(as.integer(res_status_after))
}


## function: generates test input data for calc_res_status_after()
## crossings histories for passengers are in a period of length dur_test
generator_for_calc_res_status_after <- function(num_persons, num_crossings) {
  if(num_crossings < num_persons)
    stop("number of crossings cannot be less than number of passengers")
  dur_test <- sample(seq(5, 500, 1), 1)  # has to be >= 3
  dur_threshold <- sample(seq(dur_test %/% 2 + 1, dur_test, 1), 1)
  personId <- sort(sample(num_persons, num_crossings, replace = TRUE))
  res_status_initial <- integer(num_crossings)
  days_in_country <- integer(num_crossings)
  days_obs <- integer(num_crossings)
  is_arrival <- NULL
  for(j in personId) {
    person_j <- (personId == j)
    num_crossings_j <- sum(person_j)
    ## initial residence status
    res_status_initial[person_j] <- sample(c(0, 1), 1)
    ## days observed
    days_obs_j <- sort(sample(seq(0, dur_test, 1), num_crossings_j,
                              replace = TRUE),
                       decreasing = TRUE)
    days_obs[person_j] <- days_obs_j
    ## direction
    is_arrival_j <- sample(c(T, F), 1)  # direction of 1st crossing
    if(num_crossings_j > 1) {
      for(i in 2:num_crossings_j)
        is_arrival_j[i] <- !is_arrival_j[i-1]
    }
    is_arrival[person_j] <- is_arrival_j
    ## spell length (to compute days spent in country)
    spells_j <- c(-diff(days_obs_j, lag = 1), days_obs_j[num_crossings_j])
    spells_j[!is_arrival_j] <- 0
    ## days spent in country
    days_in_country_j <- integer(num_crossings_j)
    for(i in 1:num_crossings_j)
        days_in_country_j[i] <- sum(spells_j[i:num_crossings_j])
    days_in_country[person_j] <- days_in_country_j
  }
  return(list("personId" = personId,
              "is_arrival" = is_arrival,
              "days_in_country" = days_in_country,
              "days_obs" = days_obs,
              "res_status_initial" = res_status_initial,
              "dur_test" = dur_test,
              "dur_threshold" = dur_threshold))
}


## we randomly generate test input data and test whether both implementations of
## function to calculate residence status after return the same output
test_that("calc_res_status_after runs as expected", {
  for(iteration in 1:num_iter) {  # set number of iterations
    test_input <- generator_for_calc_res_status_after(num_persons = num_persons,
                                                      num_crossings = num_crossings)
    ans_obtained <- calc_res_status_after(personId = test_input$personId,
                            is_arrival = test_input$is_arrival,
                            days_in_country = test_input$days_in_country,
                            days_obs = test_input$days_obs,
                            res_status_initial = test_input$res_status_initial,
                            dur_test = test_input$dur_test,
                            dur_threshold = test_input$dur_threshold)
    ans_expected <- calc_res_status_after2(personId = test_input$personId,
                            is_arrival = test_input$is_arrival,
                            days_in_country = test_input$days_in_country,
                            days_obs = test_input$days_obs,
                            res_status_initial = test_input$res_status_initial,
                            dur_test = test_input$dur_test,
                            dur_threshold = test_input$dur_threshold)
	   expect_identical(ans_obtained, ans_expected)
  }
})


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## node (a) of each of the two main trees (res_status_before \in {0, 1})
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## by definition, regardless of the values of days_obs and days_in_country,
## res_status_after() should return 1, for a resident arrival
test_that("calc_res_status_after returns 1 for resident arrivals", {
  personId <- c(1)  # a single crossing for a single passenger
  is_arrival <- c(TRUE)
  res_status_initial <- c(1)
  for(iteration in 1:num_iter) {
    dur_test <- sample(seq(5, 500, 1), 1)  # has to be >= 3
    dur_threshold <- sample(seq(dur_test %/% 2 + 1, dur_test, 1), 1)
    days_obs <- sample(seq(0, dur_test, 1), 1)
    days_in_country <- sample(seq(0, days_obs, 1), 1)
    ans_obtained <- calc_res_status_after(personId = personId,
                                          is_arrival = is_arrival,
                                          days_in_country = days_in_country,
                                          days_obs = days_obs,
                                          res_status_initial = res_status_initial,
                                          dur_test = dur_test,
                                          dur_threshold = dur_threshold,
                                          n_core = 1)
  ans_expected <- as.integer(c(1))
  expect_identical(ans_obtained, ans_expected)
  }
})


## by definition, regardless of the values of days_obs and days_in_country,
## res_status_after() should return 0, for a non-resident departure
test_that("calc_res_status_after returns 0 for non-resident departures", {
  personId <- c(1)  # a single crossing for a single passenger
  is_arrival <- c(FALSE)
  res_status_initial <- c(0)
  for(iteration in 1:num_iter) {
    dur_test <- sample(seq(5, 500, 1), 1)  # has to be >= 3
    dur_threshold <- sample(seq(dur_test %/% 2 + 1, dur_test, 1), 1)
    days_obs <- sample(seq(0, dur_test, 1), 1)
    days_in_country <- sample(seq(0, days_obs, 1), 1)
    ans_obtained <- calc_res_status_after(personId = personId,
                                          is_arrival = is_arrival,
                                          days_in_country = days_in_country,
                                          days_obs = days_obs,
                                          res_status_initial = res_status_initial,
                                          dur_test = dur_test,
                                          dur_threshold = dur_threshold,
                                          n_core = 1)
  ans_expected <- as.integer(c(0))
  expect_identical(ans_obtained, ans_expected)
  }
})


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## nodes (b) to (f) of the two main trees (res_status_before \in {0, 1})
## (some edge cases)
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## for a resident departure or a non-resident arrival, if no further crossings
## for that person are observed, then as soon as days_obs >= dur_threshold,
## residence status after can be determined
test_that("calc_res_status_after returns correct output for edge case of days_obs", {
  dur_test <- 480
  personId <- c(1, 2, 3, 4)
  is_arrival <- c(FALSE, FALSE, TRUE, TRUE)
  res_status_initial <- c(1, 1, 0, 0)
  # test for three different values of dur_threshold
  for(dur_threshold in c(dur_test %/% 2 + 1, 365, dur_test)) {
  days_obs <- c(dur_threshold - 1, dur_threshold, dur_threshold - 1, dur_threshold)
  days_in_country <- c(0, 0, days_obs[is_arrival])
  ans_obtained <- calc_res_status_after(personId = personId,
                                        is_arrival = is_arrival,
                                        days_in_country = days_in_country,
                                        days_obs = days_obs,
                                        res_status_initial = res_status_initial,
                                        dur_test = dur_test,
                                        dur_threshold = dur_threshold)
  ans_expected <- as.integer(c(NA, 0, NA, 1))
  expect_identical(ans_obtained, ans_expected)
  }
})


## for the following travel history, residence status after will be alternating:
## here, travel history spells are all equal c(365, 365, 365, 365)
test_that("calc_res_status_after returns correct output for edge cases of days_in_country", {
  personId <- c(2, 2, 2, 2)
  dur_test <- 480
  ## for three different values of dur_threshold
  for(dur_threshold in c(dur_test %/% 2 + 1, 365, dur_test)) {
  ## days_obs is truncated (each crossing is observed for a max. of dur_test days)
  days_obs <- c(min(dur_test, 4*dur_threshold),
                min(dur_test, 3*dur_threshold),
                min(dur_test, 2*dur_threshold),
                dur_threshold)

  ## 1) when first crossing is departure
  is_arrival <- c(FALSE, TRUE, FALSE, TRUE)
  days_in_country <- c(dur_test - dur_threshold,
                       dur_threshold,
                       dur_test - dur_threshold,
                       dur_threshold)
  ## this is the case whether the initial residence status is 0 or 1
  for(res_status_initial in list(c(1, 1, 1, 1),
                                 c(0, 0, 0, 0))) {
  ans_obtained <- calc_res_status_after(personId = personId,
                                        is_arrival = is_arrival,
                                        days_in_country = days_in_country,
                                        days_obs = days_obs,
                                        res_status_initial = res_status_initial,
                                        dur_test = dur_test,
                                        dur_threshold = dur_threshold,
                                        parallel = FALSE)
  ans_expected <- as.integer(c(0, 1, 0, 1))
  expect_identical(ans_obtained, ans_expected)
  }

 ## 2) when first crossing is arrival
  is_arrival <- c(TRUE, FALSE, TRUE, FALSE)
  days_in_country <- c(dur_threshold,
                       dur_test - dur_threshold,
                       dur_threshold,
                       0)
  ## this is the case whether the initial residence status is 0 or 1
  for(res_status_initial in list(c(1, 1, 1, 1),
                                 c(0, 0, 0, 0))) {
  ans_obtained <- calc_res_status_after(personId = personId,
                                        is_arrival = is_arrival,
                                        days_in_country = days_in_country,
                                        days_obs = days_obs,
                                        res_status_initial = res_status_initial,
                                        dur_test = dur_test,
                                        dur_threshold = dur_threshold,
                                        parallel = FALSE)
  ans_expected <- as.integer(c(1, 0, 1, 0))
  expect_identical(ans_obtained, ans_expected)
  }}
})


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## when not enough time has passed to classify a crossing
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## when res_status_before is NA, then res_status_after should also be NA;
## here, days_obs is chosen such that res_status_after the 1st crossing is NA,
## and therefore res_status_before 2nd crossing is also NA, and so on.

## a single passenger:
test_that("calc_res_status_after always returns NA when res_status_before is NA", {
    personId <- c(1, 1)  # two crossings for same person
  for(iteration in 1:num_iter) {
    is_arrival <- (if(runif(1) < 0.5) c(FALSE, TRUE) else c(TRUE, FALSE))
    res_status_initial <- rep(c(0, 1)[is_arrival], 2)
    dur_test <- sample(seq(5, 500, 1), 1)  # has to be >= 3
    dur_threshold <- sample(seq(dur_test %/% 2 + 1, dur_test, 1), 1)
    days_obs <- sort(sample(seq(0, dur_test - dur_threshold, 1), 2,
                            replace = TRUE),
                     decreasing = TRUE) # days_obs <= dur_test - dur_threshold
    days_in_country <- (if(is_arrival[2])
                          rep(days_obs[2], 2)
                        else c(days_obs[1], 0))
    ans_obtained <- calc_res_status_after(personId = personId,
                                          is_arrival = is_arrival,
                                          days_in_country = days_in_country,
                                          days_obs = days_obs,
                                          res_status_initial = res_status_initial,
                                          dur_test = dur_test,
                                          dur_threshold = dur_threshold,
                                          n_core = 1)
  ans_expected <- as.integer(c(NA, NA))
  expect_identical(ans_obtained, ans_expected)
  }
})


## three different passengers:
test_that("calc_res_status_after assigns NA when not enough time has passed to classify", {
            personId <- c(1, 1, 2, 2, 3)
            is_arrival <- c(TRUE, FALSE, FALSE, TRUE, TRUE)
            days_in_country <- c(120, 0, 20, 20, 364)
            days_obs <- c(200, 80, 50, 20, 364)
            res_status_initial <- c(0, 0, 1, 1, 0)
            dur_test <- 480
            dur_threshold <- 365
            ans_obtained <- calc_res_status_after(personId = personId,
                                                  is_arrival = is_arrival,
                                                  days_in_country = days_in_country,
                                                  days_obs = days_obs,
                                                  res_status_initial = res_status_initial,
                                                  dur_test = dur_test,
                                                  dur_threshold = dur_threshold)
            ans_expected <- as.integer(c(NA, NA, NA, NA, NA))
            expect_identical(ans_obtained, ans_expected)
          })


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## empty argument vectors
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

test_that("calc_res_status_after returns empty integer vector when passed no data", {
  personId <- c()
  is_arrival <- c()
  days_in_country <- c()
  days_obs <- c()
  res_status_initial <- c()
  dur_test <- 480
  dur_threshold <- 365
  ans_obtained <- calc_res_status_after(personId = personId,
                                        is_arrival = is_arrival,
                                        days_in_country = days_in_country,
                                        days_obs = days_obs,
                                        res_status_initial = res_status_initial,
                                        dur_test = dur_test,
                                        dur_threshold = dur_threshold)
  ans_expected <- as.integer(c())
  expect_identical(ans_obtained, ans_expected)
})


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## (conceptually) invalid input
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## days_in_country cannot be greater than days_obs
test_that("calc_res_status_after produces correct error when days_in_country > days_obs", {
  personId <- c(1, 1)
  is_arrival <- c(TRUE, FALSE)
  days_in_country <- c(210, 0)  # should be c(10, 0)
  days_obs <- c(200, 190)
  res_status_initial <- c(0, 0)
  dur_test <- 480
  dur_threshold <- 365
  expect_error(calc_res_status_after(personId = personId,
                                     is_arrival = is_arrival,
                                     days_in_country = days_in_country,
                                     days_obs = days_obs,
                                     res_status_initial = res_status_initial,
                                     dur_test = dur_test,
                                     dur_threshold = dur_threshold),
    "element of 'days_in_country' \\[210\\] greater than corresponding element of 'days_obs' \\[200\\]")
})


## days_in_country cannot be greater than dur_test
## add a check to calc_res_status_after() to make sure days_obs is truncated,
## and then remove this (since days_in_country <= days_obs <= dur_test implies
## that days_in_country <= dur_test)
test_that("calc_res_status_after produces correct error when days_in_country > dur_test", {
  personId <- c(1, 1)
  is_arrival <- c(TRUE, FALSE)
  days_in_country <- c(490, 0)
  days_obs <- c(500, 10)
  res_status_initial <- c(0, 0)
  dur_test <- 480
  dur_threshold <- 365
  expect_error(calc_res_status_after(personId = personId,
                                     is_arrival = is_arrival,
                                     days_in_country = days_in_country,
                                     days_obs = days_obs,
                                     res_status_initial = res_status_initial,
                                     dur_test = dur_test,
                                     dur_threshold = dur_threshold),
               "element of 'days_in_country' \\[490\\] greater than 'dur_test' \\[480\\]")
})


## for each person ID, res_status_initial must have the same value for all crossings
## ID2 travel spells are c(20, 5, 110, 365); days obs. are c(500, 480, 475, 365)
test_that("calc_res_status_after produces correct error when res_status_initial is incorrect", {
  personId <- c(1, 1, 2, 2, 2, 2)
  is_arrival <- c(TRUE, FALSE, FALSE, TRUE, FALSE, TRUE)
  days_in_country <- c(10, 0, 350, 370, 365, 365)
  days_obs <- c(200, 190, 480, 480, 475, 365)
  res_status_initial <- c(0, 0, 1, 1, 0, 1)  # should be c(0, 0, 1, 1, 1, 1)
  dur_test <- 480
  dur_threshold <- 365
  expect_error(calc_res_status_after(personId = personId,
                                     is_arrival = is_arrival,
                                     days_in_country = days_in_country,
                                     days_obs = days_obs,
                                     res_status_initial = res_status_initial,
                                     dur_test = dur_test,
                                     dur_threshold = dur_threshold),
               "values of 'res_status_initial' not all identical for 'personId' equal to \"2\"")
})


## personId needs to be ordered (into groups)
test_that("calc_res_status_after produces correct error when personId not in order", {
  personId <- c(1, 2, 1, 2, 2, 2, 3)
  is_arrival <- c(TRUE, FALSE, FALSE, TRUE, FALSE, TRUE, FALSE)
  days_in_country <- c(10, 350, 0, 370, 365, 365, 0)
  days_obs <- c(200, 480, 190, 480, 475, 365, 365)
  res_status_initial <- c(0, 1, 0, 1, 1, 1, 1)
  dur_test <- 480
  dur_threshold <- 365
  expect_error(calc_res_status_after(personId = personId,
                                     is_arrival = is_arrival,
                                     days_in_country = days_in_country,
                                     days_obs = days_obs,
                                     res_status_initial = res_status_initial,
                                     dur_test = dur_test,
                                     dur_threshold = dur_threshold),
               "'personId' not ordered correctly")

  ## re-ordered correctly:
  ## (personId needs to be grouped, not necessarily in non-decreasing order)
  personId <- c(2, 2, 2, 2, 1, 1, 3)
  is_arrival <- c(FALSE, TRUE, FALSE, TRUE, TRUE, FALSE, FALSE)
  days_in_country <- c(350, 370, 365, 365, 10, 0, 0)
  days_obs <- c(480, 480, 475, 365, 200, 190, 365)
  res_status_initial <- c(1, 1, 1, 1, 0, 0, 1)
  expect_identical(calc_res_status_after(personId = personId,
                                         is_arrival = is_arrival,
                                         days_in_country = days_in_country,
                                         days_obs = days_obs,
                                         res_status_initial = res_status_initial,
                                         dur_test = dur_test,
                                         dur_threshold = dur_threshold),
                   as.integer(c(1, 1, 1, 1, 0, 0, 0)))
})


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## NA input values
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## function includes is.na() tests for all input variables

## res_status_initial cannot have NAs
test_that("calc_res_status_after produces correct error when res_status_initial has NAs", {
  personId <- c(1, 1, 2, 2, 2, 2)
  is_arrival <- c(TRUE, FALSE, FALSE, TRUE, FALSE, TRUE)
  days_in_country <- c(10, 0, 350, 370, 365, 365)
  days_obs <- c(200, 190, 480, 480, 475, 365)
  res_status_initial <- c(0, 0, 1, NA, 1, 1)
  dur_test <- 480
  dur_threshold <- 365
  expect_error(calc_res_status_after(personId = personId,
                                     is_arrival = is_arrival,
                                     days_in_country = days_in_country,
                                     days_obs = days_obs,
                                     res_status_initial = res_status_initial,
                                     dur_test = dur_test,
                                     dur_threshold = dur_threshold),
               "'res_status_initial' has missing values")
})


## personId cannot have NAs
test_that("calc_res_status_after produces correct error when personId has NAs", {
  personId <- c(1, 1, 2, 2, 2, NA)
  is_arrival <- c(TRUE, FALSE, FALSE, TRUE, FALSE, TRUE)
  days_in_country <- c(10, 0, 350, 370, NA, 365)
  days_obs <- c(200, 190, 480, 480, 475, 365)
  res_status_initial <- c(0, 0, 1, 1, 1, 1)
  dur_test <- 480
  dur_threshold <- 365
  expect_error(calc_res_status_after(personId = personId,
                                     is_arrival = is_arrival,
                                     days_in_country = days_in_country,
                                     days_obs = days_obs,
                                     res_status_initial = res_status_initial,
                                     dur_test = dur_test,
                                     dur_threshold = dur_threshold),
               "'personId' has missing values")
})


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## non-numeric input values
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## function already tests for class of input variables and coerces to correct
## type. maybe make this more comprehensive and skip the tests here

test_that("calc_res_status_after produces correct error when res_status_initial is non-numeric", {
  personId <- c(1, 1, 2, 2, 2, 2)
  is_arrival <- c(TRUE, FALSE, FALSE, TRUE, FALSE, TRUE)
  days_in_country <- c(10, 0, 350, 370, 365, 365)
  days_obs <- c(200, 190, 480, 480, 475, 365)
  res_status_initial <- c("0", 0, 1, 1, 1, 1)  # coerced to character
  dur_test <- 480
  dur_threshold <- 365
  expect_error(calc_res_status_after(personId = personId,
                                     is_arrival = is_arrival,
                                     days_in_country = days_in_country,
                                     days_obs = days_obs,
                                     res_status_initial = res_status_initial,
                                     dur_test = dur_test,
                                     dur_threshold = dur_threshold),
               "'res_status_initial' is non-numeric")
})











