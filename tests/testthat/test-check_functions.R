
context("check_functions")

test_that("check_and_tidy_date_first_last works with valid inputs", {
    check_and_tidy_date_first_last <- migstat:::check_and_tidy_date_first_last
    ans_obtained <- check_and_tidy_date_first_last(date = as.Date("2000-01-01"),
                                                   date_crossing = as.Date("2000-02-02"),
                                                   name = "date_first")
    ans_expected <- as.Date("2000-01-01")
    expect_identical(ans_obtained, ans_expected)
    check_and_tidy_date_first_last <- migstat:::check_and_tidy_date_first_last
    ans_obtained <- check_and_tidy_date_first_last(date = "2000-05-01",
                                                   date_crossing = as.Date(c("2000-02-02", "2000-04-03")),
                                                   name = "date_last")
    ans_expected <- as.Date("2000-05-01")
    expect_identical(ans_obtained, ans_expected)
    ans_obtained <- check_and_tidy_date_first_last(date = NULL,
                                                   date_crossing = as.Date(c("2000-02-02", "2000-04-03")),
                                                   name = "date_first")
    ans_expected <- as.Date("2000-02-02") - 0.1 * ((as.Date("2000-04-03") - as.Date("2000-02-02")) / lubridate::ddays(1))
    expect_identical(ans_obtained, ans_expected)
    ans_obtained <- check_and_tidy_date_first_last(date = NULL,
                                                   date_crossing = as.Date(c("2000-02-02", "2000-04-03")),
                                                   name = "date_last")
    ans_expected <- as.Date("2000-04-03") + 0.1 * ((as.Date("2000-04-03") - as.Date("2000-02-02")) / lubridate::ddays(1))
    expect_identical(ans_obtained, ans_expected)
})

test_that("check_and_tidy_date_first_last throws appropriate errors", {
    check_and_tidy_date_first_last <- migstat:::check_and_tidy_date_first_last
    expect_error(check_and_tidy_date_first_last(date = c("2000-10-01", "2000-10-01"),
                                                date_crossing = as.Date(c("2000-02-02", "2000-04-03")),
                                                name = "date_last"),
                 "'date_last' does not have length 1")
    expect_error(check_and_tidy_date_first_last(date = NA,
                                                date_crossing = as.Date(c("2000-02-02", "2000-04-03")),
                                                name = "date_last"),
                 "'date_last' is missing")
    expect_error(check_and_tidy_date_first_last(date = "01-01-2000",
                                                date_crossing = as.Date(c("2000-02-02", "2000-04-03")),
                                                name = "date_last"),
                 "'date_last' does not have valid year\\-month\\-date format")
    expect_error(check_and_tidy_date_first_last(date = "2000-02-03",
                                                date_crossing = as.Date(c("2000-02-02", "2000-04-03")),
                                                name = "date_first"),
                 "'date_first' is later than first element of 'date_crossing'")
    expect_error(check_and_tidy_date_first_last(date = "2000-02-03",
                                                date_crossing = as.Date(c("2000-02-02", "2000-04-03")),
                                                name = "date_last"),
                 "'date_last' is earlier than last element of 'date_crossing'")
})

test_that("check_n_core works with valid inputs", {
    check_n_core <- migstat:::check_n_core
    expect_identical(check_n_core(4L), NULL)
    expect_identical(check_n_core(1L), NULL)
})

test_that("check_n_core throws appropriate errors", {
    check_n_core <- migstat:::check_n_core
    expect_error(check_n_core(1.1),
                 "'n_core' is not an integer")
    expect_error(check_n_core(1:2),
                 "'n_core' does not have length 1")
})

test_that("check_parallel works with valid inputs", {
    check_parallel <- migstat:::check_parallel
    expect_identical(check_parallel(TRUE), NULL)
    expect_identical(check_parallel(FALSE), NULL)
})

test_that("check_parallel throws appropriate errors", {
    check_parallel <- migstat:::check_parallel
    expect_error(check_parallel(c(TRUE, FALSE)),
                 "'parallel' does not have length 1")
    expect_error(check_parallel("TRUE"),
                 "'parallel' does not have type \"logical\"")
})

test_that("check_res_status_after_and_person_id works with valid inputs", {
    check_res_status_after_and_person_id <- migstat:::check_res_status_after_and_person_id
    res_status_after <- c(1L, 0L, NA, NA,
                          1L,
                          NA, NA,
                          NA)
    personId <- c(1, 1, 1, 1,
                   2,
                   3, 3,
                   4)
    ans_obtained <- check_res_status_after_and_person_id(res_status_after = res_status_after,
                                                         personId = personId)
    ans_expected <- NULL
    expect_identical(ans_obtained, ans_expected)
})

test_that("check_res_status_after_and_person_id throws appropriate errors", {
    check_res_status_after_and_person_id <- migstat:::check_res_status_after_and_person_id
    res_status_after <- c(1L, 0L, NA, 1L,
                          1L,
                          NA, NA)
    personId <- c(1, 1, 1, 1,
                   2,
                   3, 3)
    expect_error(check_res_status_after_and_person_id(res_status_after = res_status_after,
                                                      personId = personId),
                 "values of 'res_status_after' for 'personId' equal to \"1\" switch from missing back to non-missing")
    res_status_after <- c(1L, 0L, NA, NA,
                          1L,
                          NA, 1L)
    personId <- c(1, 1, 1, 1,
                   2,
                   3, 3)
    expect_error(check_res_status_after_and_person_id(res_status_after = res_status_after,
                                                      personId = personId),
                 "values of 'res_status_after' for 'personId' equal to \"3\" switch from missing back to non-missing")
})

test_that("check_res_status_initial_and_res_status_after works with valid inputs", {
    check_res_status_initial_and_res_status_after <- migstat:::check_res_status_initial_and_res_status_after
    res_status_initial <- c(1L, 1L, 1L, 1L,
                                 1L,
                                 NA, NA,
                                 0L)
    res_status_after <- c(1L, 0L, NA, NA,
                          1L,
                          NA, NA,
                          NA)
    ans_obtained <- check_res_status_initial_and_res_status_after(res_status_initial = res_status_initial,
                                                                       res_status_after = res_status_after)
    ans_expected <- NULL
    expect_identical(ans_obtained, ans_expected)
})

test_that("check_res_status_initial_and_res_status_after throws appropriate errors", {
    check_res_status_initial_and_res_status_after <- migstat:::check_res_status_initial_and_res_status_after
    res_status_initial <- c(1L, 1L, 1L, 1L,
                                 NA,
                                 NA, NA,
                                 0L)
    res_status_after <- c(1L, 0L, NA, NA,
                          1L,
                          NA, NA,
                          NA)
    expect_error(check_res_status_initial_and_res_status_after(res_status_initial = res_status_initial,
                                                                    res_status_after = res_status_after),
                 "value of 'res_status_after' non-missing even though value for 'res_status_initial' missing")
})


