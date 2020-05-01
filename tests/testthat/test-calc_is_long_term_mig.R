################################################################################
context("calc_is_long_term_mig")
################################################################################

## number of iterations for tests
num_iter <- 10
num_persons <- 10
num_crossings <- 10


## ID 1, travel history spells are c(10, 190); days obs. are c(200, 190)
## ID 2, travel history spells are c(20, 5, 110, 365); days obs. are c(500, 480, 475, 365)
## ID 3, travel history spell is c(365); days obs. is c(365)
## ID 4, travel history spell is c(365); days obs. is c(365)
test_that("calc_is_long_term_mig works with valid inputs", {
  personId <- c(1, 1, 2, 2, 2, 2, 3, 4)
  is_arrival <- c(TRUE, FALSE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE)
  days_in_country <- c(10, 0, 350, 370, 365, 365, 0, 365)
  days_obs <- c(200, 190, 480, 480, 475, 365, 365, 365)  # days_obs[3] is min(500, dur_test)
  res_status_initial <- c(0, 0, 1, 1, 1, 1, 1, 0)
  dur_test <- 480
  dur_threshold <- 365
  res_status_after <- calc_res_status_after(personId = personId,
                                            is_arrival = is_arrival,
                                            days_in_country = days_in_country,
                                            days_obs = days_obs,
                                            res_status_initial = res_status_initial,
                                            dur_test = dur_test,
                                            dur_threshold = dur_threshold)
  res_status_before <- calc_res_status_before(personId = personId,
                                              res_status_initial = res_status_initial,
                                              res_status_after = res_status_after)

  ## parallel - n_core = 1
  ans_obtained <- calc_is_long_term_mig(personId = personId,
                                        is_arrival = is_arrival,
                                        res_status_before = res_status_before,
                                        res_status_after = res_status_after,
                                        days_in_country = days_in_country,
                                        days_obs = days_obs,
                                        dur_test = dur_test,
                                        dur_threshold = dur_threshold,
                                        n_core = 1)
  ans_expected <- as.integer(c(0, 0, 0, 0, 0, 0, 1, 1))
  expect_identical(ans_obtained, ans_expected)

  ## parallel - n_core = 2
  ans_obtained <- calc_is_long_term_mig(personId = personId,
                                        is_arrival = is_arrival,
                                        res_status_before = res_status_before,
                                        res_status_after = res_status_after,
                                        days_in_country = days_in_country,
                                        days_obs = days_obs,
                                        dur_test = dur_test,
                                        dur_threshold = dur_threshold,
                                        n_core = 2)
  ans_expected <- as.integer(c(0, 0, 0, 0, 0, 0, 1, 1))
  expect_identical(ans_obtained, ans_expected)

  ## non-parallel
  ans_obtained <- calc_is_long_term_mig(personId = personId,
                                        is_arrival = is_arrival,
                                        res_status_before = res_status_before,
                                        res_status_after = res_status_after,
                                        days_in_country = days_in_country,
                                        days_obs = days_obs,
                                        dur_test = dur_test,
                                        dur_threshold = dur_threshold,
                                        parallel = FALSE)
  ans_expected <- as.integer(c(0, 0, 0, 0, 0, 0, 1, 1))
  expect_identical(ans_obtained, ans_expected)
})


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## calc_is_long_term_mig() tested against calc_is_long_term_mig2() with randomly
## generated input
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## function: alternate implementation of calc_long_term_mig(), written by
## tester from definition
calc_is_long_term_mig2 <- function(personId, is_arrival,
                                   res_status_before, res_status_after,
                                   days_in_country, days_obs,
                                   dur_test, dur_threshold) {
  is_long_term_mig <- rep(NA, length(personId))  # initialize
  na_res_after <- is.na(res_status_after)
  is_long_term_mig[!na_res_after] <- ifelse(res_status_after[!na_res_after] == 
                                            res_status_before[!na_res_after], 0, 1)
  W <- dur_test
  V <- dur_threshold
  for(j in personId) {
    person_j <- (personId == j)
    num_crossings_j <- sum(person_j)
    is_arrival_j <- is_arrival[person_j]
    res_status_before_j <- res_status_before[person_j]
    res_status_after_j <- res_status_after[person_j]
    days_in_country_j <- days_in_country[person_j]
    days_obs_j <- days_obs[person_j]
    is_long_term_mig_j <- is_long_term_mig[person_j]
    ## calculate is_long_term_mig: 'null-null's
    if(num_crossings_j > 1) {
      for(i in 2:num_crossings_j) {
        if(is.na(res_status_before_j[i])) {
          idx_last_known <- max(which(!is.na(res_status_before_j[1:(i-1)])))
          difference <- (if(is_arrival_j[idx_last_known]) 
                           days_in_country_j[idx_last_known] - days_in_country_j[i]
                         else
                           days_obs_j[idx_last_known] - days_obs_j[i] -
                           (days_in_country_j[idx_last_known] - days_in_country_j[i]))
          if(difference < 2*dur_threshold - dur_test &
             is_arrival_j[idx_last_known] != is_arrival_j[i])
            is_long_term_mig_j[i] <- 0
        }
      }
    }
    is_long_term_mig[person_j] <- is_long_term_mig_j  # overwrites (bad practice!)
  }
  return(as.integer(is_long_term_mig))
}


## function: generates test input data for calc_is_long_term_mig()
## generates crossings histories for passengers in a period of length dur_test
generator_for_calc_is_long_term_mig <- function(num_persons, num_crossings) {
  if(num_crossings < num_persons)
    stop("number of crossings cannot be less than number of passengers")
  dur_test <- sample(seq(300, 500, 1), 1)  # W
  dur_threshold <- sample(seq(dur_test %/% 2 + 1, dur_test, 1), 1)  # V
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
  ## res_status_after
  res_status_after <- calc_res_status_after(personId = personId,
                                            is_arrival = is_arrival,
                                            days_in_country = days_in_country,
                                            days_obs = days_obs,
                                            res_status_initial = res_status_initial,
                                            dur_test = dur_test,
                                            dur_threshold = dur_threshold)
  ## res_status_before
  res_status_before <- calc_res_status_before(personId = personId,
                                              res_status_initial = res_status_initial,
                                              res_status_after = res_status_after)
  return(list("personId" = personId,
              "is_arrival" = is_arrival,
              "res_status_before" = res_status_before,
              "res_status_after" = res_status_after,
              "days_in_country" = days_in_country,
              "days_obs" = days_obs,
              "dur_test" = dur_test,
              "dur_threshold" = dur_threshold))
}


## we randomly generate test input data and test whether both implementations of
## function to calculate long-term migration status return the same output
test_that("calc_is_long_term_mig runs as expected", {
  for(iteration in 1:num_iter) {  # set number of iterations
    test_input <- generator_for_calc_is_long_term_mig(num_persons = num_persons,
                                                      num_crossings = num_crossings)
    ans_obtained <- calc_is_long_term_mig(personId = test_input$personId,
                            is_arrival = test_input$is_arrival,
                            res_status_before = test_input$res_status_before,
                            res_status_after = test_input$res_status_after,
                            days_in_country = test_input$days_in_country,
                            days_obs = test_input$days_obs,
                            dur_test = test_input$dur_test,
                            dur_threshold = test_input$dur_threshold)
    ans_expected <- calc_is_long_term_mig2(personId = test_input$personId,
                            is_arrival = test_input$is_arrival,
                            res_status_before = test_input$res_status_before,
                            res_status_after = test_input$res_status_after,
                            days_in_country = test_input$days_in_country,
                            days_obs = test_input$days_obs,
                            dur_test = test_input$dur_test,
                            dur_threshold = test_input$dur_threshold)
	   expect_identical(ans_obtained, ans_expected)
  }
})


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## NULL-NULL departures: last known crossing is non-resident arrival
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

test_that("calc_is_long_term_mig produces correct output for null-null departures", {
  ## exactly V - (W - V) days spent in country between crossings 1 and 4
  personId <- c(2, 2, 2, 2)
  is_arrival <- c(TRUE, FALSE, TRUE, FALSE)
  res_status_before <- as.numeric(c(0, NA, NA, NA))
  res_status_after <- as.numeric(c(NA, NA, NA, NA))
  days_in_country <- c(196, 148, 148, 0)
  days_obs <- c(248, 200, 150, 2)
  dur_test <- 468
  dur_threshold <- 332

  ans_obtained <- calc_is_long_term_mig(personId = personId,
                                        is_arrival = is_arrival,
                                        res_status_before = res_status_before,
                                        res_status_after = res_status_after,
                                        days_in_country = days_in_country,
                                        days_obs = days_obs,
                                        dur_test = dur_test,
                                        dur_threshold = dur_threshold)

  ans_obtained2 <- calc_is_long_term_mig2(personId = personId,
                                          is_arrival = is_arrival,
                                          res_status_before = res_status_before,
                                          res_status_after = res_status_after,
                                          days_in_country = days_in_country,
                                          days_obs = days_obs,
                                          dur_test = dur_test,
                                          dur_threshold = dur_threshold)
  
  ans_expected <- as.integer(c(NA, 0, NA, NA))
  expect_identical(ans_obtained, ans_expected)
  expect_identical(ans_obtained, ans_obtained2)


  ## V - (W - V) - 1 days spent in country between crossings 1 and 4
  personId <- c(2, 2, 2, 2)
  is_arrival <- c(TRUE, FALSE, TRUE, FALSE)
  res_status_before <- as.numeric(c(0, NA, NA, NA))
  res_status_after <- as.numeric(c(NA, NA, NA, NA))
  days_in_country <- c(195, 147, 147, 0)
  days_obs <- c(248, 200, 150, 3)
  dur_test <- 468
  dur_threshold <- 332

  ans_obtained <- calc_is_long_term_mig(personId = personId,
                                        is_arrival = is_arrival,
                                        res_status_before = res_status_before,
                                        res_status_after = res_status_after,
                                        days_in_country = days_in_country,
                                        days_obs = days_obs,
                                        dur_test = dur_test,
                                        dur_threshold = dur_threshold)

  ans_obtained2 <- calc_is_long_term_mig2(personId = personId,
                                          is_arrival = is_arrival,
                                          res_status_before = res_status_before,
                                          res_status_after = res_status_after,
                                          days_in_country = days_in_country,
                                          days_obs = days_obs,
                                          dur_test = dur_test,
                                          dur_threshold = dur_threshold)
  
  ans_expected <- as.integer(c(NA, 0, NA, 0))
  expect_identical(ans_obtained, ans_expected)
  expect_identical(ans_obtained, ans_obtained2)
  

  ## > V - (W - V) days spent in country between crossings 1 and 6 
  # expected answer: NA  0 NA  0 NA NA
  personId <- c(2, 2, 2, 2, 2, 2)
  is_arrival <- c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE)
  res_status_before <- as.numeric(c(0, NA, NA, NA, NA, NA))
  res_status_after <- as.numeric(c(NA, NA, NA, NA, NA, NA))
  days_in_country <- c(205, 157, 157, 146, 146, 0)
  days_obs <- c(248, 200, 185, 174, 159, 13)
  dur_test <- 468
  dur_threshold <- 332

  ans_obtained <- calc_is_long_term_mig(personId = personId,
                                        is_arrival = is_arrival,
                                        res_status_before = res_status_before,
                                        res_status_after = res_status_after,
                                        days_in_country = days_in_country,
                                        days_obs = days_obs,
                                        dur_test = dur_test,
                                        dur_threshold = dur_threshold)

  ans_obtained2 <- calc_is_long_term_mig2(personId = personId,
                                          is_arrival = is_arrival,
                                          res_status_before = res_status_before,
                                          res_status_after = res_status_after,
                                          days_in_country = days_in_country,
                                          days_obs = days_obs,
                                          dur_test = dur_test,
                                          dur_threshold = dur_threshold)
  
  ans_expected <- as.integer(c(NA, 0, NA, 0, NA, NA))
  expect_identical(ans_obtained, ans_expected)
  expect_identical(ans_obtained, ans_obtained2)
})


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## NULL-NULL arrivals: last known crossing is resident departure
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

test_that("calc_is_long_term_mig produces correct output for null-null arrivals", {
  ## exactly V - (W - V) days spent outside country between crossings 1 and 4
  personId <- c(2, 2, 2, 2)
  is_arrival <- c(FALSE, TRUE, FALSE, TRUE)
  res_status_before <- as.numeric(c(1, NA, NA, NA))
  res_status_after <- as.numeric(c(NA, NA, NA, NA))
  days_in_country <- c(52, 52, 2, 2)
  days_obs <- c(248, 200, 150, 2)
  dur_test <- 468
  dur_threshold <- 332

  ans_obtained <- calc_is_long_term_mig(personId = personId,
                                        is_arrival = is_arrival,
                                        res_status_before = res_status_before,
                                        res_status_after = res_status_after,
                                        days_in_country = days_in_country,
                                        days_obs = days_obs,
                                        dur_test = dur_test,
                                        dur_threshold = dur_threshold)

  ans_obtained2 <- calc_is_long_term_mig2(personId = personId,
                                          is_arrival = is_arrival,
                                          res_status_before = res_status_before,
                                          res_status_after = res_status_after,
                                          days_in_country = days_in_country,
                                          days_obs = days_obs,
                                          dur_test = dur_test,
                                          dur_threshold = dur_threshold)

  ans_expected <- as.integer(c(NA, 0, NA, NA))
  expect_identical(ans_obtained, ans_expected)
  expect_identical(ans_obtained, ans_obtained2)

  ## V - (W - V) - 1 days spent outside country between crossings 1 and 4
  personId <- c(2, 2, 2, 2)
  is_arrival <- c(FALSE, TRUE, FALSE, TRUE)
  res_status_before <- as.numeric(c(1, NA, NA, NA))
  res_status_after <- as.numeric(c(NA, NA, NA, NA))
  days_in_country <- c(53, 53, 3, 3)
  days_obs <- c(248, 200, 150, 3)
  dur_test <- 468
  dur_threshold <- 332

  ans_obtained <- calc_is_long_term_mig(personId = personId,
                                        is_arrival = is_arrival,
                                        res_status_before = res_status_before,
                                        res_status_after = res_status_after,
                                        days_in_country = days_in_country,
                                        days_obs = days_obs,
                                        dur_test = dur_test,
                                        dur_threshold = dur_threshold)

  ans_obtained2 <- calc_is_long_term_mig2(personId = personId,
                                          is_arrival = is_arrival,
                                          res_status_before = res_status_before,
                                          res_status_after = res_status_after,
                                          days_in_country = days_in_country,
                                          days_obs = days_obs,
                                          dur_test = dur_test,
                                          dur_threshold = dur_threshold)
  
  ans_expected <- as.integer(c(NA, 0, NA, 0))
  expect_identical(ans_obtained, ans_expected)
  expect_identical(ans_obtained, ans_obtained2)


  ## > V - (W - V) days spent outside country between crossings 1 and 6 
  personId <- c(2, 2, 2, 2, 2, 2)
  is_arrival <- c(FALSE, TRUE, FALSE, TRUE, FALSE, TRUE)
  res_status_before <- as.numeric(c(1, NA, NA, NA, NA, NA))
  res_status_after <- as.numeric(c(NA, NA, NA, NA, NA, NA))
  days_in_country <- c(43, 43, 28, 28, 13, 13)
  days_obs <- c(248, 200, 185, 174, 159, 13)
  dur_test <- 468
  dur_threshold <- 332

  ans_obtained <- calc_is_long_term_mig(personId = personId,
                                        is_arrival = is_arrival,
                                        res_status_before = res_status_before,
                                        res_status_after = res_status_after,
                                        days_in_country = days_in_country,
                                        days_obs = days_obs,
                                        dur_test = dur_test,
                                        dur_threshold = dur_threshold)

  ans_obtained2 <- calc_is_long_term_mig2(personId = personId,
                                          is_arrival = is_arrival,
                                          res_status_before = res_status_before,
                                          res_status_after = res_status_after,
                                          days_in_country = days_in_country,
                                          days_obs = days_obs,
                                          dur_test = dur_test,
                                          dur_threshold = dur_threshold)
  
  ans_expected <- as.integer(c(NA, 0, NA, 0, NA, NA))
  expect_identical(ans_obtained, ans_expected)
  expect_identical(ans_obtained, ans_obtained2)
})









