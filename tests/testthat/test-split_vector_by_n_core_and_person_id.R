
context("split_vector_by_n_core_and_person_id")

test_that("split_vector_by_n_core_and_person_id works", {
    split_vector_by_n_core_and_person_id <- migstat:::split_vector_by_n_core_and_person_id
    personId <- rep(1:10, times = 10:1)
    vector <- seq_along(personId)
    ## n_core = 2
    ans_obtained <- split_vector_by_n_core_and_person_id(vector = vector,
                                                         personId = personId,
                                                         n_core = 2)
    ans_expected <- list(vector[1:40],
                         vector[41:55])
    expect_identical(ans_obtained, ans_expected)
    ## n_core = 3
    ans_obtained <- split_vector_by_n_core_and_person_id(vector = vector,
                                                         personId = personId,
                                                         n_core = 3)
    ans_expected <- list(vector[1:27],
                         vector[28:40],
                         vector[51:55])
    ## n_core = 4
    ans_obtained <- split_vector_by_n_core_and_person_id(vector = vector,
                                                         personId = personId,
                                                         n_core = 4)
    ans_expected <- list(vector[1:19],
                         vector[20:40],
                         vector[41:49],
                         vector[50:55])
    ## n_core = 10
    ans_obtained <- split_vector_by_n_core_and_person_id(vector = vector,
                                                         personId = personId,
                                                         n_core = 10)
    ans_expected <- as.list(10:1)
    ## n_core = 20
    ans_obtained <- split_vector_by_n_core_and_person_id(vector = vector,
                                                         personId = personId,
                                                         n_core = 20)
    ans_expected <- as.list(10:1)
})
