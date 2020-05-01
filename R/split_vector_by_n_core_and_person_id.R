
## Helper function for parallel processing


## HAS_TESTS
split_vector_by_n_core_and_person_id <- function(vector, n_core, personId) {
    run_lengths <- rle(personId)$lengths
    n_person <- length(run_lengths)
    ends <- cumsum(run_lengths)
    frac <- seq_len(n_core) / n_core
    i <- unique(as.integer(frac * n_person))
    i <- i[i > 0L]
    n_ans <- length(i)
    ends <- ends[i]
    starts <- c(1L, ends[-n_ans] + 1L)
    mapply(function(i, j) vector[i : j], starts, ends, SIMPLIFY = FALSE)
}










