################################################################################
context("calc_days_obs")
################################################################################

## number of iterations for tests
long_tests <- TRUE

if(long_tests) {
  num_iter <- 1000
  num_crossings <- 500
} else {
    num_iter <- 100
    num_crossings <- 50
}


## calc_days_obs() should return the number of days from the crossing until
## date_obs_end or dur_test, which ever comes first
test_that("calc_days_obs returns correct output given valid inputs", {
  date_crossing <- as.Date(c("2000-01-05",
                             "2000-12-31",
                             "2001-06-13",
                             "2002-01-01"))
  date_obs_end <- as.Date("2001-12-31")
  dur_test <- 365L
  ans_obtained <- calc_days_obs(date_crossing = date_crossing,
                                date_obs_end = date_obs_end,
                                dur_test = dur_test)
  ans_expected <- as.integer(c(dur_test,
                               dur_test,
                               as.integer(date_obs_end - date_crossing[3]),
                               0))
  expect_identical(ans_obtained, ans_expected)
})


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## calc_days_obs() tested against calc_days_obs2() with randomly generated input
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## function: alternate implementation of calc_days_obs(), written by tester from
## definition
calc_days_obs2 <- function(date_crossing, date_obs_end, dur_test) {
  date_crossing <- as.Date(date_crossing)
  date_obs_end <- as.Date(date_obs_end)
  dur_test <- as.integer(dur_test)
  n <- length(date_crossing)
  obs_window_end <- pmin(date_crossing + dur_test, date_obs_end)
  days_obs <- pmax(obs_window_end - date_crossing, rep(0, n))
  return(as.integer(days_obs))
}

## function: generates test input data for calc_days_obs()
generator_for_calc_days_obs <- function(num_crossings,
                                        period_start, period_end) {
  dates_set <- seq(as.Date(period_start), as.Date(period_end), by = 1)
  date_crossing <- sample(dates_set, num_crossings, replace = TRUE)
  date_obs_end <- sample(dates_set, 1)
  dur_test <- sample(seq(50, 500, 10), 1)
  return(list("date_crossing" = date_crossing,
              "date_obs_end" = date_obs_end,
              "dur_test" = dur_test))
}

## we randomly generate test input data and test whether both implementations of
## function to calculate days observed return the same output
test_that("calc_days_obs runs as expected", {
  for(iteration in 1:num_iter) {  # set number of iterations
    test_input <- generator_for_calc_days_obs(num_crossings = num_crossings,
	                                             period_start = "1980-01-01",
											                                   period_end = "2018-06-30")
    ans_obtained <- calc_days_obs(date_crossing = test_input$date_crossing,
	                                 date_obs_end = test_input$date_obs_end,
								                          dur_test = test_input$dur_test)
    ans_expected <- calc_days_obs2(date_crossing = test_input$date_crossing,
	                                  date_obs_end = test_input$date_obs_end,
								                           dur_test = test_input$dur_test)
	   expect_identical(ans_obtained, ans_expected)
  }
})


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## tests for classes of input vectors
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## character inputs are coerced to class Date in calc_days_obs()
test_that("calc_days_obs works with input dates of class character", {
  date_crossing <- c("2000-01-05",
                     "2000-12-31",
                     "2001-06-13",
                     "2002-01-01")
  date_obs_end <- "2001-12-31"
  dur_test <- 365L
  ans_obtained <- calc_days_obs(date_crossing = date_crossing,
                                date_obs_end = date_obs_end,
                                dur_test = dur_test)
  ans_expected <- as.integer(c(dur_test,
                               dur_test,
                               as.integer(as.Date(date_obs_end) - as.Date(date_crossing[3])),
                               0))
  expect_identical(ans_obtained, ans_expected)
})


## dur_test can be of class integer, Duration or difftime
test_that("calc_days_obs works with dur_test of class Duration or difftime", {
  date_crossing <- as.Date(c("2000-01-05",
                             "2000-12-31",
                             "2001-06-13",
                             "2002-01-01"))
  date_obs_end <- as.Date("2001-12-31")

  ## class(dur_test) = Duration
  dur_test <- lubridate::ddays(365)
  ans_obtained <- calc_days_obs(date_crossing = date_crossing,
                                date_obs_end = date_obs_end,
                                dur_test = dur_test)
  ans_expected <- as.integer(c(365,
                               365,
                               as.integer(date_obs_end - date_crossing[3]),
                               0))
  expect_identical(ans_obtained, ans_expected)

  ## class(dur_test) = difftime
  dur_test <- difftime("2001-12-31", "2000-12-31")
  ans_obtained <- calc_days_obs(date_crossing = date_crossing,
                                date_obs_end = date_obs_end,
                                dur_test = dur_test)
  ans_expected <- as.integer(c(365,
                               365,
                               as.integer(date_obs_end - date_crossing[3]),
                               0))
  expect_identical(ans_obtained, ans_expected)
})


## currently calc_days_obs() produces errors when passed inputs of class factor
test_that("calc_days_obs produces error when inputs are of class factor", {
  # date_crossing as factor
  expect_error(calc_days_obs(date_crossing = as.factor("2000-01-05"),
                             date_obs_end = as.Date("2001-12-31"),
                             dur_test = 365L),
               "'date_crossing' has class \"factor\"")

  # date_obs_end as factor
  expect_error(calc_days_obs(date_crossing = as.Date("2002-01-05"),
                             date_obs_end = as.factor("2001-12-31"),
                             dur_test = 365L),
               "'date_obs_end' has class \"factor\"")

  # dur_test as factor
  expect_error(calc_days_obs(date_crossing = as.Date("2000-01-05"),
                             date_obs_end = as.Date("2001-12-31"),
                             dur_test = as.factor(365L)),
               "'dur_test' has class \"factor\"")
})

## how should the function behave when passed factors? should the function
## produce an error or warning or deal with factors?
## Tester1: Can't think of an instance where the function could be passed factors.
## Tester2: What if crossing dates are deliberately set to factors in a dataframe,
## for instance to fit linear models or to group by (which in some formulae/plotting
## functions need to have class factor)?


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## NA inputs
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

test_that("calc_days_obs produces correct errors for missing input values", {
  ## dur_test missing
  expect_error(calc_days_obs(date_crossing = as.Date("2000-01-05"),
                             date_obs_end = as.Date("2001-12-31"),
                             dur_test = as.integer("")),
               "'dur_test' is missing")

  ## date_crossing missing
  expect_error(calc_days_obs(date_crossing = as.Date(c("2001-01-01", NA)),
                             date_obs_end = as.Date("2001-12-31"),
                             dur_test = 365L),
               "'date_crossing' has missing values")

  ## date_obs_end missing
  expect_error(calc_days_obs(date_crossing = as.Date("2000-01-05"),
                             date_obs_end = c(NA),
                             dur_test = 365L),
               "'date_obs_end' is missing")
})

## should the function deal with NAs in date_crossing? (i.e. return NAs)


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## empty or NULL inputs
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## these tests also produce warnings because of the test is.na(NULL)
test_that("calc_days_obs produces correct errors for NULL input values", {
  ## empty date_crossing vector
  expect_error(calc_days_obs(date_crossing = c(),
                             date_obs_end = as.Date("2001-12-31"),
                             dur_test = as.integer(365)),
               "'date_crossing' has class \"NULL\"")

  ## empty date_obs_end argument
  expect_error(calc_days_obs(date_crossing = as.Date("2001-01-01"),
                             date_obs_end = c(),
                             dur_test = as.integer(365)),
               "'date_obs_end' does not have length 1")

  ## empty dur_test argument
  expect_error(calc_days_obs(date_crossing = as.Date("2001-01-01"),
                             date_obs_end = as.Date("2001-12-31"),
                             dur_test = c()),
               "'dur_test' does not have length 1")
})

























