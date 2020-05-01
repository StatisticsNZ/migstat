################################################################################
context("make_person_id")

################################################################################
## tests for make_person_id()
################################################################################

## person ID is assigned based on matches in at least one of the two keys
## e.g. match in passenger name (key1) and/or passport number (key2)
## the order in which the records appear can change the output

test_that("make_person_id runs as expected", {
  ## linked based on match in key1 only
  key1 <- c("Josh F", "Anitra F", "Anna M", "Anitra F", "Josh F")
  key2 <- c("AB3908", "CG0809", "ZX6427", "QV2809", "BH7153")
  ans_obtained <- make_person_id(key1 = key1, key2 = key2)
  ans_expected <- as.integer(c(1, 2, 3, 2, 1))
  expect_identical(ans_obtained, ans_expected)

  ## linked based on match in key2 only
  key1 <- c("Josh F", "Anitra F", "Anna M", "Anitra G", "A Moore")
  key2 <- c("AB3908", "QV2809", "ZX6427", "QV2809", "ZX6427")
  ans_obtained <- make_person_id(key1 = key1, key2 = key2)
  ans_expected <- as.integer(c(1, 2, 3, 2, 3))
  expect_identical(ans_obtained, ans_expected)

  ## linked based on match in both keys
  key1 <- c("Josh F", "Anitra F", "Anna M", "Anna M", "Josh F")
  key2 <- c("AB3908", "QV2809", "ZX6427", "ZX6427", "AB3908")
  ans_obtained <- make_person_id(key1 = key1, key2 = key2)
  ans_expected <- as.integer(c(1, 2, 3, 3, 1))
  expect_identical(ans_obtained, ans_expected)

  ## linked based on match in key1 overlapped with match in key2
  key1 <- c("Josh F", "A Moore", "Anitra F", "Anna M", "Anna M")
  key2 <- c("AB3908", "ZX6427", "QV2809", "ZX6427", "DK2571")
  ans_obtained <- make_person_id(key1 = key1, key2 = key2)
  ans_expected <- as.integer(c(1, 2, 3, 2, 2))
  expect_identical(ans_obtained, ans_expected)

  ## note that when records are swapped, the output changes (see previous test)
  key1 <- c("Josh F", "A Moore", "Anitra F", "Anna M", "Anna M")
  key2 <- c("AB3908", "ZX6427", "QV2809", "DK2571", "ZX6427")  ## swapped 4 & 5
  ans_obtained <- make_person_id(key1 = key1, key2 = key2)
  ans_expected <- as.integer(c(1, 2, 3, 4, 4))
  expect_identical(ans_obtained, ans_expected)

  ## string matching is case sensitive (should we make this optional?)
  key1 <- c("Josh F", "A Moore", "Anitra F", "Anna M", "anna M")
  key2 <- c("AB3908", "ZX6427", "QV2809", "ZX6427", "DK2571")
  ans_obtained <- make_person_id(key1 = key1, key2 = key2)
  ans_expected <- as.integer(c(1, 2, 3, 2, 4))
  expect_identical(ans_obtained, ans_expected)
})


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## empty argument (key1 and key2) vectors
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

test_that("make_person_id returns empty integer vector when passed no data", {
  key1 <- c()
  key2 <- c()
  ans_obtained <- make_person_id(key1 = key1, key2 = key2)
  ans_expected <- as.integer(c())
  expect_identical(ans_obtained, ans_expected)

  ## function call also produces warnings because of the test is.na(NULL)
  # expect_warning(make_person_id(key1 = key1, key2 = key2))

  ## if at least one vector is non-empty, it will fail on the equal lengths test
  key1 <- c()
  key2 <- c("AB3908")
  expect_error(make_person_id(key1 = key1, key2 = key2),
               "'key1' and 'key2' have different lengths")
})


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## blank (strings with nchar() = 0) key values
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

test_that("make_person_id produces error when key values are blanks", {
  ## blanks in key1 vector
  key1 <- c("Josh F", "Anitra F", "")
  key2 <- c("AB3908", "CG0809", "QV2809")
  expect_error(make_person_id(key1 = key1, key2 = key2),
               "'key1' has blanks")

  ## blanks in key2 vector
  key1 <- c("Josh F", "Anitra F", "Anitra F")
  key2 <- c("AB3908", "", "QV2809")
  expect_error(make_person_id(key1 = key1, key2 = key2),
               "'key2' has blanks")
})


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## input vectors of differing lengths
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

test_that("make_person_id produces error when key vectors have different lengths", {
  ## |key1| < |key2|
  key1 <- c("Josh F")
  key2 <- c("AB3908", "CG0809")
  expect_error(make_person_id(key1 = key1, key2 = key2),
               "'key1' and 'key2' have different lengths")

  ## |key2| < |key1|
  key1 <- c("Josh F", "Anitra F")
  key2 <- c("AB3908")
  expect_error(make_person_id(key1 = key1, key2 = key2),
               "'key1' and 'key2' have different lengths")
})


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## NA key values
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

test_that("make_person_id produces errors when key values are NA", {
  # NA in key1 values
  key1 <- c("Josh F", "Anitra F", NA)
  key2 <- c("AB3908", "QV2809", "QV2809")
  expect_error(make_person_id(key1 = key1, key2 = key2),
               "'key1' has missing values")

  ## NA in key2 values
  key1 <- c("Josh F", "Anitra F", "Anitra F")
  key2 <- c("AB3908", NA, "QV2809")
  expect_error(make_person_id(key1 = key1, key2 = key2),
               "'key2' has missing values")
})


## make_person_id() needs to be updated in the future to handle NAs;
## for now (by default), the following tests won't run;
## call test_when_na_allowed(na_allowed = TRUE) when ready to run these tests;
## test_when_na_allowed() may need debugging, since written without testing!

test_when_na_allowed <- function(na_allowed = FALSE) {
  if(na_allowed) {
    ## when only one of the two key values is NA (for a given crossing/record)
    test_that("make_person_id can deal with NA values for either key1 xor key2", {
      ## NAs and no matches
      key1 <- c("Josh F", "Anitra F", NA, NA)
      key2 <- c("AB3908", NA, "QV2809", "DL7302")
      ans_obtained <- make_person_id(key1 = key1, key2 = key2)
      ans_expected <- as.integer(c(1, 2, 3, 4))
      expect_identical(ans_obtained, ans_expected)

      ## NAs and match in key1
      key1 <- c("Josh F", "Anitra F", NA, "Anitra F")
      key2 <- c("AB3908", NA, "QV2809", "DL7302")
      ans_obtained <- make_person_id(key1 = key1, key2 = key2)
      ans_expected <- as.integer(c(1, 2, 3, 2))
      expect_identical(ans_obtained, ans_expected)

      ## NAs and match in key2
      key1 <- c("Josh F", "Anitra F", NA, NA)
      key2 <- c("AB3908", NA, "QV2809", "QV2809")
      ans_obtained <- make_person_id(key1 = key1, key2 = key2)
      ans_expected <- as.integer(c(1, 2, 3, 3))
      expect_identical(ans_obtained, ans_expected)

	  ## NAs and matches in key1 and key2 overlapping
	  key1 <- c("Josh F", "Anna M", "Anitra F", NA, "Anna M")
      key2 <- c("AB3908", "ZX6427", NA, "ZX6427", NA)
      ans_obtained <- make_person_id(key1 = key1, key2 = key2)
      ans_expected <- as.integer(c(1, 2, 3, 2, 2))
      expect_identical(ans_obtained, ans_expected)
    })

    ## when both key values are missing for a crossing/record. how should our
   	## function behave?
    ## (the argument vectors will likely come from a dataframe that can have
    ## records with NAs for both keys)
    test_that("make_person_id can deal with NA values for both keys", {
      key1 <- c("Josh F", "Anitra F", NA, NA, "Anna M", "J Ford")
      key2 <- c("AB3908", NA, "QV2809", NA, "QV2809", NA)
      ans_obtained <- personId(key1 = key1, key2 = key2)
      ans_expected <- as.integer(c(1, 2, 3, NA, 3, 4))  # what to expect?
      expect_identical(ans_obtained, ans_expected)
    })
  }
}

test_when_na_allowed()  # won't run tests by default


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## non-character key values
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

test_that("make_person_id can deal with numeric keys", {
  ## numeric key2 vector
  key1 <- c("Josh F", "Anitra F", "Anitra G", "Josh F", "Anna M")
  key2 <- c(5673908, 5687222, 5687222, 6587234, 3918236)
  ans_obtained <- make_person_id(key1 = key1, key2 = key2)
  ans_expected <- as.integer(c(1, 2, 2, 1, 3))
  expect_identical(ans_obtained, ans_expected)
})



test_that("make_person_id accepts factor keys", {
  ## factor key1 vector;
  key1 <- as.factor(c("Josh F", "Anitra F", "Anitra G", "Josh F", "Anna M"))
  key2 <- c(5673908, 5687222, 5687222, 6587234, 3918236)
  ans_obtained <- make_person_id(key1 = key1, key2 = key2)
  ans_expected <- make_person_id(key1 = as.character(key1), key2 = key2)
  expect_identical(ans_obtained, ans_expected)
  ## factor key2 vector;
  key1 <- c("Josh F", "Anitra F", "Anitra G", "Josh F", "Anna M")
  key2 <- factor(c(5673908, 5687222, 5687222, 6587234, 3918236))
  ans_obtained <- make_person_id(key1 = key1, key2 = key2)
  ans_expected <- make_person_id(key1 = key1, key2 = as.character(key2))
  expect_identical(ans_obtained, ans_expected)
})


################################################################################
## tests for update_person_id()
################################################################################

## update_person_id() makes person IDs for newly added records (with NA person IDs)

test_that("update_person_id updates as expected", {
  ##
  personId <- c(1, 2, 3, 1, NA, NA)
  key1 <- c("Josh F", "Anitra F", "Anna M", "Josh F", "A Moore", "Pubudu S")
  key2 <- c("AB3908", "CG0809", "ZX6427", "AB3908", "ZX6427", "QZ8097")
  ans_obtained <- update_person_id(personId = personId,
                                   key1 = key1, key2 = key2)
  ans_expected <- as.integer(c(1, 2, 3, 1, 3, 4))
  expect_identical(ans_obtained, ans_expected)
  ## the order in which the records appear changes the output:
  ## case 1)
  key1 <- c("Josh F", "A Moore", "Anitra F", "Anna M", "Anna M")
  key2 <- c("AB3908", "ZX6427", "QV2809", "ZX6427", "DK2571")
  personId <- c(1, 2, NA, NA, NA)
  ans_obtained <- update_person_id(personId = personId,
                                   key1 = key1, key2 = key2)
  ans_expected <- as.integer(c(1, 2, 3, 2, 2))
  expect_identical(ans_obtained, ans_expected)
  ## case 2) swapped records 4 and 5
  key1 <- c("Josh F", "A Moore", "Anitra F", "Anna M", "Anna M")
  key2 <- c("AB3908", "ZX6427", "QV2809", "DK2571", "ZX6427")
  personId <- c(1, 2, NA, NA, NA)
  ans_obtained <- update_person_id(personId = personId,
                                   key1 = key1, key2 = key2)
  ans_expected <- as.integer(c(1, 2, 3, 4, 4))
  expect_identical(ans_obtained, ans_expected)
  ## the new records need not be appended to the end of the dataset, as long as
  ## within each person ID (1, 2, ..., n), their new records appear after their
  ## records with ID assigned??? e.g.
  ## (1)
  person_id_vectors <- list(c(1, 2, NA, 1, NA, NA),
                            c(1, NA, NA, 1, NA, NA))
  for(id_vec in person_id_vectors) {
    personId <- id_vec
    key1 <- c("Josh F", "Anitra F", "Anna M", "Josh F", "A Moore", "Pubudu S")
    key2 <- c("AB3908", "CG0809", "ZX6427", "AB3908", "ZX6427", "QZ8097")
    ans_obtained <- update_person_id(personId = personId,
                                     key1 = key1, key2 = key2)
    ans_expected <- as.integer(c(1, 2, 3, 1, 3, 4))
    expect_identical(ans_obtained, ans_expected)
  }
  ## (2)
  person_id_vectors <- list(c(1, NA, 2, NA, 3, NA, NA),
                            c(1, NA, 2, 2, 3, NA, NA))
  for(id_vec in person_id_vectors) {
  personId <- id_vec
    key1 <- c("Josh F", "Josh F", "Anitra F", "Anitra F", "A Moore", "Anna M", "J F")
    key2 <- c("AB3908", "CG0809", "ZX6427", "QV2809", "QZ8097", "QZ8097", "CG0809")
    ans_obtained <- update_person_id(personId = personId,
                                     key1 = key1, key2 = key2)
    ans_expected <- as.integer(c(1, 1, 2, 2, 3, 3, 1))
    expect_identical(ans_obtained, ans_expected)
  }
})


## when there are no new records to assign person IDs to, update_person_id()
## must return the original person IDs input vector
test_that("update_person_id does not break when no records to update", {
  personId <- c(1, 2, 3, 1, 3, 4)
  key1 <- c("Josh F", "Anitra F", "Anna M", "Josh F", "A Moore", "Pubudu S")
  key2 <- c("AB3908", "CG0809", "ZX6427", "AB3908", "ZX6427", "QZ8097")
  ans_obtained <- update_person_id(personId = personId,
                                   key1 = key1, key2 = key2)
  ans_expected <- as.integer(c(1, 2, 3, 1, 3, 4))
  expect_identical(ans_obtained, ans_expected)
  expect_identical(make_person_id(key1 = key1, key2 = key2),
                   ans_obtained)  # make and update return same output
})


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## empty argument (personId, key1 and key2) vectors
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

test_that("update_person_id returns empty integer vector when passed no data", {
  ## all three input vectors are empty
  personId <- c()
  key1 <- c()
  key2 <- c()
  ans_obtained <- update_person_id(personId = personId,
                                   key1 = key1, key2 = key2)
  ans_expected <- as.integer(c())
  expect_identical(ans_obtained, ans_expected)
  
  ## function call also produces warnings because of the test is.na(NULL)
  # expect_warning(update_person_id(personId = personId,
  #                                 key1 = key1, key2 = key2))
  
  ## if at least one vector is non-empty, it will fail on the equal lengths test
  personId <- c()
  key1 <- c()
  key2 <- c("AB3908")
  expect_error(update_person_id(personId = personId,
                                key1 = key1, key2 = key2),
               "'key2' and 'personId' have different lengths")
})


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## input vectors of differing lengths
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

test_that("update_person_id produces error when input vectors have different lengths", {
  ## all three input vectors have different lengths
  personId <- c(1, NA, NA)
  key1 <- c("Josh F")
  key2 <- c("AB3908", "CG0809")
  expect_error(update_person_id(personId = personId,
                                key1 = key1, key2 = key2),
               "'key1' and 'personId' have different lengths")
  ## key2 has different length
  personId <- c(1, NA, NA)
  key1 <- c("Josh F", "Anitra F", "Anitra F")
  key2 <- c("AB3908")
  expect_error(update_person_id(personId = personId,
                                key1 = key1, key2 = key2),
               "'key2' and 'personId' have different lengths")
})


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## non-character key values
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

test_that("update_person_id can deal with numeric keys", {
  ## numeric key2 vector
  personId <- c(1, 2, NA, NA, NA)
  key1 <- c("Josh F", "Anitra F", "Anitra G", "Josh F", "Anna M")
  key2 <- c(5673908, 5687222, 5687222, 6587234, 3918236)
  ans_obtained <- update_person_id(personId = personId,
                                   key1 = key1, key2 = key2)
  ans_expected <- as.integer(c(1, 2, 2, 1, 3))
  expect_identical(ans_obtained, ans_expected)
})



test_that("update_person_id allows factor keys", {
  ## factor key1 vector;
  personId <- c(1, 2, NA, NA, NA)
  key1 <- as.factor(c("Josh F", "Anitra F", "Anitra G", "Josh F", "Anna M"))
  key2 <- c(5673908, 5687222, 5687222, 6587234, 3918236)
  ans_obtained <- update_person_id(personId = personId, key1 = key1, key2 = key2)
  ans_expected <- update_person_id(personId = personId, key1 = as.character(key1), key2 = key2)
  expect_identical(ans_obtained, ans_expected)
  ## factor key2 vector;
  personId <- c(1, 2, NA, NA, NA)
  key1 <- c("Josh F", "Anitra F", "Anitra G", "Josh F", "Anna M")
  key2 <- factor(c(5673908, 5687222, 5687222, 6587234, 3918236))
  ans_obtained <- update_person_id(personId = personId, key1 = key1, key2 = key2)
  ans_expected <- update_person_id(personId = personId, key1 = key1, key2 = as.character(key2))
  expect_identical(ans_obtained, ans_expected)
})


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## blank (strings with nchar() = 0) key values
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## checks for blank string values were added to update_person_id()
test_that("update_person_id produces error when key values are blanks", {
  ## blanks in key1 vector
  personId <- c(1, NA, NA)
  key1 <- c("Josh F", "Anitra F", "")
  key2 <- c("AB3908", "CG0809", "QV2809")
  expect_error(update_person_id(personId = personId,
                                key1 = key1, key2 = key2),
               "'key1' has blanks")
  ## blanks in key2 vector
  personId <- c(1, NA, NA)
  key1 <- c("Josh F", "Anitra F", "Anitra F")
  key2 <- c("AB3908", "", "QV2809")
  expect_error(update_person_id(personId = personId,
                                key1 = key1, key2 = key2),
               "'key2' has blanks")
  ## if personId includes blanks, update_person_id() does not produce error,
  ## because personId is coerced to class integer before being used
  personId <- c(1, "", "")
  key1 <- c("Josh F", "Anitra F", "Josh F")
  key2 <- c("AB3908", "CG0809", "QV2809")
  ans_obtained <- update_person_id(personId = personId,
                                   key1 = key1, key2 = key2)
  ans_expected <- as.integer(c(1, 2, 1))
  expect_identical(ans_obtained, ans_expected)
})


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## NA key values
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## checks for NA key values were added to update_person_id()
test_that("update_person_id produces error when key values are NA", {
  ## missing values in key1
  personId <- c(1, 2, 3, 1, NA, NA)
  key1 <- c("Josh F", "Anitra F", NA, "Josh F", "A Moore", "Pubudu S")
  key2 <- c("AB3908", "CG0809", "ZX6427", "AB3908", "ZX6427", "QZ8097")
  expect_error(update_person_id(personId = personId,
                                key1 = key1, key2 = key2),
			   "'key1' has missing values")
  ## missing values in key2
  personId <- c(1, 2, 3, 1, NA, NA)
  key1 <- c("Josh F", "Anitra F", "Anna M", "Josh F", "A Moore", "Pubudu S")
  key2 <- c("AB3908", "CG0809", "ZX6427", NA, "ZX6427", "QZ8097")
  expect_error(update_person_id(personId = personId,
                                key1 = key1, key2 = key2),
			   "'key2' has missing values")
})


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Function does not alter original vector in place
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

test_that("update_person_id does not overwrite input ID vector (global variable)", {
  personId <- c(1L, NA)
  key1 <- c("Josh F", "Anitra F")
  key2 <- c("AB3908", "CG0809")
  personId_updated <- update_person_id(personId = personId,
                                       key1 = key1, key2 = key2)
  expect_identical(personId_updated, c(1:2))
  expect_identical(personId, c(1L, NA))  # should remain unaltered
})














