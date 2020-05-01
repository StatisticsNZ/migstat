
#' Simple function for assigning person IDs
#'
#' Function \code{make_person_id} uses data on two keys,
#' to assign person IDs to records. Two records are assumed
#' to belong to the same person if the records share at least one
#' key.
#'
#' Function \code{update_person_id} adds new IDs to an existing
#' vector of IDs. These new IDs are consistent with the
#' existing ones.
#'
#' Function \code{update_person_id} returns the same vector of IDs
#' whether the input records are passed in batches or all at once,
#' provided that the orders of the input records in both instances
#' are identical. (See example below.)
#'
#' Note that the keys themselves can be built from more than
#' one variable, which permits more sophisticated matching rules.
#'
#' At present \code{make_person_id} and \code{update_person_id}
#' do not allow \code{key1} and \code{key2} to include missing
#' values or blanks (i.e. zero-length strings). This is perhaps
#' too strict, and we may revisit it in the future.
#'
#' @param key1 Variable used to identify unique individuals,
#' e.g. passport number, or passport number combined with name.
#' @param key2 Variable used to identify unique individuals,
#' e.g. date of birth, or date of birth combined with name.
#' @param personId Vector, the same length as \code{key1}
#' and \code{key2}, with previously created IDs, and \code{NA}s
#' for newly added records (where IDs should be created).
#'
#' @return An integer vector the same length as \code{key1} and
#' \code{key2}.
#'
#' @examples
#' ## function 'make_person_id'
#' name <- c("M Akhbar", "A Smith", "D Lee", "D Lee", "X Martinez", "M Akhbar")
#' passport_no <- c("AB3908", "CG0809", "QV2809", "QZ8097", "LE3980", "AB3908")
#' personId <- make_person_id(key1 = name, key2 = passport_no)
#' personId
#'
#' ## function 'update_person_id'
#' personId_extended <- c(personId, NA, NA)
#' name_extended <- c(name, "D Lee", "A Sen")
#' passport_no_extended <- c(passport_no, "QV2809", "ZZ2546")
#' update_person_id(personId = personId_extended,
#'                  key1 = name_extended,
#'                  key2 = passport_no_extended)
#' ## (note that the new record for D Lee gets an ID of 3)
#'
#' ## update_person_id returns identical results in both instances
#' identical(update_person_id(personId = personId_extended,
#'                            key1 = name_extended,
#'                            key2 = passport_no_extended),
#'           update_person_id(personId = as.integer(rep(NA, length(personId_extended))),
#'                            key1 = name_extended,
#'                            key2 = passport_no_extended))
#'
#' @export
make_person_id <- function(key1, key2) {
  if (is.factor(key1))
    key1 <- as.character(key1)
  if (is.factor(key2))
    key2 <- as.character(key2)
  for (name in c("key1", "key2")) {
    value <- get(name)
    if (any(is.na(value)))
      stop(gettextf("'%s' has missing values",
                    name))
    if (any(!nzchar(value)))
      stop(gettextf("'%s' has blanks",
                    name))
  }
  n <- length(key1)
  if (!identical(length(key2), n))
    stop(gettextf("'%s' and '%s' have different lengths",
                  "key1", "key2"))
  personId <- rep.int(NA_integer_, times = n)
  personId_is_na <- rep.int(1L, times = n)
  match1 <- match(key1, key1)
  match2 <- match(key2, key2)
  first_id <- 1L
  ## function updates 'personId' in-place
  person_id_internal(personId = personId,
                     personId_is_na = personId_is_na,
                     match1 = match1,
                     match2 = match2,
                     first_id = first_id)
  personId
}

#' @rdname make_person_id
#' @export
update_person_id <- function(personId, key1, key2) {
  if (is.factor(key1))
    key1 <- as.character(key1)
  if (is.factor(key2))
    key2 <- as.character(key2)
  for (name in c("key1", "key2")) {
    value <- get(name)
    if (any(is.na(value)))
      stop(gettextf("'%s' has missing values",
                    name))
    if (any(!nzchar(value)))
      stop(gettextf("'%s' has blanks",
                    name))
  }
  n <- length(personId)
  if (!identical(length(key1), n))
    stop(gettextf("'%s' and '%s' have different lengths",
                  "key1", "personId"))
  if (!identical(length(key2), n))
    stop(gettextf("'%s' and '%s' have different lengths",
                  "key2", "personId"))
  personId <- as.integer(personId) + 0L # force R to make a new version of 'personId'
  personId_is_na <- as.integer(is.na(personId))
  match1 <- match(key1, key1)
  match2 <- match(key2, key2)
  personId_has_non_na <- sum(personId_is_na) < n
  if (personId_has_non_na) {
    max_current <- max(personId, na.rm = TRUE)
    first_id <- max_current + 1L
  }
  else
    first_id <- 1L
  ## function updates 'personId' in-place
  person_id_internal(personId = personId,
                     personId_is_na = personId_is_na,
                     match1 = match1,
                     match2 = match2,
                     first_id = first_id)
  personId
}
