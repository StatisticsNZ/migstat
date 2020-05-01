################################################################################
context("calc_res_status_before")
################################################################################

## number of iterations for tests
long_tests <- FALSE

if(long_tests) {
  num_iter <- 1000
  num_persons <- 100
  num_crossings <- 1000
} else {
    num_iter <- 100
    num_persons <- 50
    num_crossings <- 500
}


## simple case
test_that("calc_res_status_before returns correct output given valid inputs", {
  personId <- c(1, 1, 2, 2, 2, 3)
  res_status_initial <- c(0, 0, 1, 1, 1, 1)
  res_status_after <- c(1, 1, 1, 0, 0, 1)
  ans_obtained <- calc_res_status_before(personId = personId,
                                         res_status_initial = res_status_initial,
                                         res_status_after = res_status_after)
  ans_expected <- as.integer(c(0, 1, 1, 1, 0, 1))
  expect_identical(ans_obtained, ans_expected)
})


## calc_res_status_before() requires personId to be ordered (1, 2, ..., n);
## is this too strict?
## date_crossing must be ordered within each person ID.
## also, should date_crossing be included (for checking order)?
test_that("calc_res_status_before produces correct error when personId is not ordered", {
  personId <- c(1, 1, 2, 2, 3, 2)
  res_status_initial <- c(0, 0, 1, 1, 1, 1)
  res_status_after <- c(1, 1, 1, 0, 1, 0)
  expect_error(calc_res_status_before(personId = personId,
                                      res_status_initial = res_status_initial,
                                      res_status_after = res_status_after),
               "'personId' not ordered correctly")
})

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## calc_res_status_before() tested against calc_res_status_before2()
## with randomly generated input
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## function: alternate implementation of calc_res_status_before(), written by
## tester from definition
calc_res_status_before2 <- function(personId, res_status_initial,
                                    res_status_after) {
  res_status_before <- numeric(length(personId))
  for(j in personId) {
    person_j <- (personId == j)
    num_j <- sum(person_j)
    res_status_initial_j <- res_status_initial[person_j]
    res_status_after_j <- res_status_after[person_j]
    res_status_before[person_j] <- (if(num_j > 1)
                                      c(res_status_initial_j[1],
	                                       res_status_after_j[1:(num_j - 1)])
								                            else
								                              res_status_initial_j[1])
  }
  return(as.integer(res_status_before))
}

## function: generates test input data for calc_res_status_before()
generator_for_calc_res_status_before <- function(num_persons, num_crossings) {
  if(num_crossings < num_persons)
    stop("number of crossings cannot be less than number of passengers")
  personId <- sort(sample(num_persons, num_crossings, replace = TRUE))
  res_status_initial <- numeric(length(personId))
  res_status_after <- numeric(length(personId))
  for(j in personId) {
    person_j <- (personId == j)
    res_status_initial[person_j] <- sample(c(0, 1), 1)
    res_status_after_j <- sample(c(0, 1, NA), sum(person_j), replace = TRUE)
    res_status_after[person_j] <- (if(any(is.na(res_status_after_j)))
	                                    c(res_status_after_j[!is.na(res_status_after_j)],
                                       res_status_after_j[is.na(res_status_after_j)])
								                           else
								                             res_status_after_j)
  }
  return(list("personId" = personId,
              "res_status_initial" = res_status_initial,
              "res_status_after" = res_status_after))
}

## we randomly generate test input data and test whether both implementations of
## function to calculate residence status before return the same output
test_that("calc_res_status_before runs as expected", {
  for(iteration in 1:num_iter) {  # set number of iterations
    test_input <- generator_for_calc_res_status_before(num_persons = num_persons,
	                                                      num_crossings = num_crossings)
    ans_obtained <- calc_res_status_before(personId = test_input$personId,
	                            res_status_initial = test_input$res_status_initial,
							                      res_status_after = test_input$res_status_after)
    ans_expected <- calc_res_status_before2(personId = test_input$personId,
	                            res_status_initial = test_input$res_status_initial,
							                      res_status_after = test_input$res_status_after)
	  expect_identical(ans_obtained, ans_expected)
  }
})

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## NA input values
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## NAs in personId
test_that("calc_res_status_before produces correct error when personId contains NAs", {
  personId <- c(1, 1, 2, NA, 2, 3)
  res_status_initial <- c(0, 0, 1, 1, 1, 1)
  res_status_after <- c(1, 1, 1, 0, 0, 1)
  expect_error(calc_res_status_before(personId = personId,
                                      res_status_initial = res_status_initial,
                                      res_status_after = res_status_after),
               "'personId' has missing values")
})


## NAs in res_status_initial
test_that("calc_res_status_before produces correct error when res_status_initial contains NAs", {
  personId <- c(1, 1, 2, 2, 2, 3)
  res_status_initial <- c(0, 0, 1, NA, 1, 1)
  res_status_after <- c(1, 1, 1, 0, 0, 1)
  expect_error(calc_res_status_before(personId = personId,
                                      res_status_initial = res_status_initial,
                                      res_status_after = res_status_after),
               "'res_status_initial' has missing values")
})


## NAs in res_status_after
test_that("calc_res_status_before returns correct error when res_status_after contains NAs", {
  personId <- c(1, 1, 2, 2, 2, 3)
  res_status_initial <- c(0, 0, 1, 1, 1, 1)

  ## res_status_after can have missing values, but they must appear at the end
  ## of the sequence within each person ID
  res_status_after <- c(1, 1, 1, NA, NA, 1)
  ans_obtained <- calc_res_status_before(personId = personId,
                                         res_status_initial = res_status_initial,
                                         res_status_after = res_status_after)
  ans_expected <- as.integer(c(0, 1, 1, 1, NA, 1))
  expect_identical(ans_obtained, ans_expected)

  ## if NAs appear in the middle of the sequence, an error is produced
  res_status_after <- c(1, 1, 1, NA, 0, 1)
  expect_error(calc_res_status_before(personId = personId,
                                      res_status_initial = res_status_initial,
                                      res_status_after = res_status_after),
               "values of 'res_status_after' for 'personId' equal to")
})


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## empty input vectors
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## if at least one vector is non-empty, the function fails on equal length test

test_that("calc_res_status_before produces correct error when some inputs are empty", {
  ## when personId is empty
  personId <- numeric()
  res_status_initial <- c(0, 0, 1, 1, 1, 1)
  res_status_after <- c(1, 1, 1, 0, 0, 1)
  expect_error(calc_res_status_before(personId = personId,
                                      res_status_initial = res_status_initial,
                                      res_status_after = res_status_after),
               "'personId' and 'res_status_initial' have different lengths")

  ## when res_status_initial is empty
  personId <- c(1, 1, 2, 2, 2, 3)
  res_status_initial <- numeric()
  res_status_after <- c(1, 1, 1, 0, 0, 1)
  expect_error(calc_res_status_before(personId = personId,
                                      res_status_initial = res_status_initial,
                                      res_status_after = res_status_after),
               "'personId' and 'res_status_initial' have different lengths")

  ## when res_status_after is empty
  personId <- c(1, 1, 2, 2, 2, 3)
  res_status_initial <- c(0, 0, 1, 1, 1, 1)
  res_status_after <- numeric()
  expect_error(calc_res_status_before(personId = personId,
                                      res_status_initial = res_status_initial,
                                      res_status_after = res_status_after),
               "'personId' and 'res_status_after' have different lengths")
})

## note that, if type is NULL, i.e. input is c() instead of numeric(), we get
## different errors, e.g. "'x' must be a vector of an atomic type" at rle(personId)


test_that("calc_res_status_before works when res_status_initial contains blanks", {
  personId <- c(1, 1, 2, 2, 4,5 )
  res_status_initial <- c(0, 0, 1, "", 1, 1)
  res_status_after <- c(1, 1, 1, 0, 0, 1)
  expect_error(calc_res_status_before(personId = personId,
                                      res_status_initial = res_status_initial,
                                      res_status_after = res_status_initial),
               "'res_status_initial' is non-numeric")
})

test_that("calc_res_status_before works when res_status_after contains blanks", {
  personId <- c(1, 1, 2, 2, 4,5 )
  res_status_initial <- c(0, 0, 1, 1, 1, 1)
  res_status_after <- c(1, 1, 1, "", 0, 1)
  expect_error(calc_res_status_before(personId = personId,
                                      res_status_initial = res_status_initial,
                                      res_status_after = res_status_after),
               "'res_status_after' is non-numeric")
})








