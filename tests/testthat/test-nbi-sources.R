.person_nbi_source_test_data <- function() {
  tibble::tibble(
    id_hogar = c("h1", "h1", "h2", "h2", "h3", "h3"),
    p01 = c(1, 2, 1, 2, 1, 2),
    p03 = c(40, 8, 45, 10, 50, 20),
    p04 = c(1, 3, 1, 3, 1, 3),
    p07 = c(2, 2, 2, 1, 2, 2),
    p10a = c(1, 3, 2, 3, 4, 4),
    p10b = c(0, 2, 1, 3, 5, 5),
    empleo = c(0, 0, 1, 0, 1, 1),
    area = c(1, 1, 2, 2, 1, 1),
    fexp = 1,
    upm = c(1, 1, 2, 2, 3, 3),
    estrato = c(1, 1, 2, 2, 3, 3)
  )
}

.household_nbi_source_test_data <- function() {
  tibble::tibble(
    id_hogar = c("h1", "h2", "h3"),
    vi04a = c(7, 1, 1),
    vi05a = c(1, 6, 1),
    vi07 = c(0, 1, 2),
    vi09 = c(5, 1, 1),
    vi10 = c(1, 2, 1),
    vi10a = c(1, 3, 1)
  )
}

test_that("NBI source join appends housing variables to person data", {
  person <- .person_nbi_source_test_data()
  household <- .household_nbi_source_test_data()

  out <- enemdu_join_nbi_sources(person, household)

  expect_s3_class(out, "enemdu_tbl")
  expect_true(all(c("vi04a", "vi05a", "vi07", "vi09", "vi10", "vi10a") %in% names(out)))
  expect_equal(out$vi04a, c(7, 7, 1, 1, 1, 1))
  expect_equal(out$vi05a, c(1, 1, 6, 6, 1, 1))
})

test_that("NBI source join preserves person row count and order", {
  person <- .person_nbi_source_test_data()
  person$row_id <- seq_len(nrow(person))
  household <- .household_nbi_source_test_data()

  out <- enemdu_join_nbi_sources(person, household)

  expect_equal(nrow(out), nrow(person))
  expect_equal(out$row_id, person$row_id)
  expect_equal(out$id_hogar, person$id_hogar)
})

test_that("NBI source join validates source data frames", {
  person <- .person_nbi_source_test_data()
  household <- .household_nbi_source_test_data()

  expect_error(
    enemdu_join_nbi_sources(list(), household),
    class = "enemdu_error_invalid_nbi_source_data"
  )

  expect_error(
    enemdu_join_nbi_sources(person, list()),
    class = "enemdu_error_invalid_nbi_source_data"
  )
})

test_that("NBI source join requires household key in person data", {
  person <- .person_nbi_source_test_data()
  household <- .household_nbi_source_test_data()
  person$id_hogar <- NULL

  expect_error(
    enemdu_join_nbi_sources(person, household),
    class = "enemdu_error_missing_nbi_source_key"
  )
})

test_that("NBI source join requires household key in household data", {
  person <- .person_nbi_source_test_data()
  household <- .household_nbi_source_test_data()
  household$id_hogar <- NULL

  expect_error(
    enemdu_join_nbi_sources(person, household),
    class = "enemdu_error_missing_nbi_source_key"
  )
})

test_that("NBI source join requires requested housing variables", {
  person <- .person_nbi_source_test_data()
  household <- .household_nbi_source_test_data()
  household$vi10a <- NULL

  expect_error(
    enemdu_join_nbi_sources(person, household),
    class = "enemdu_error_missing_nbi_housing_vars"
  )
})

test_that("NBI source join rejects duplicated household rows", {
  person <- .person_nbi_source_test_data()
  household <- .household_nbi_source_test_data()
  household <- rbind(household, household[1, ])

  expect_error(
    enemdu_join_nbi_sources(person, household),
    class = "enemdu_error_duplicate_nbi_household_ids"
  )
})

test_that("NBI source join protects existing housing variables unless overwrite is requested", {
  person <- .person_nbi_source_test_data()
  household <- .household_nbi_source_test_data()
  person$vi04a <- 99

  expect_error(
    enemdu_join_nbi_sources(person, household),
    class = "enemdu_error_existing_nbi_housing_vars"
  )

  out <- enemdu_join_nbi_sources(person, household, overwrite = TRUE)

  expect_equal(out$vi04a, c(7, 7, 1, 1, 1, 1))
})

test_that("NBI source join aborts on unmatched households in strict mode", {
  person <- .person_nbi_source_test_data()
  household <- .household_nbi_source_test_data()
  person$id_hogar[6] <- "h4"

  expect_error(
    enemdu_join_nbi_sources(person, household, strict = TRUE),
    class = "enemdu_error_unmatched_nbi_households"
  )
})

test_that("NBI source join keeps unmatched households with NA housing values in non-strict mode", {
  person <- .person_nbi_source_test_data()
  household <- .household_nbi_source_test_data()
  person$id_hogar[6] <- "h4"

  out <- enemdu_join_nbi_sources(person, household, strict = FALSE)

  expect_equal(nrow(out), nrow(person))
  expect_true(is.na(out$vi04a[6]))
  expect_true(is.na(out$vi10a[6]))
})

test_that("NBI source join attaches diagnostics", {
  person <- .person_nbi_source_test_data()
  household <- .household_nbi_source_test_data()

  out <- enemdu_join_nbi_sources(person, household)
  diagnostics <- attr(out, "nbi_source_join_diagnostics")

  expect_type(diagnostics, "list")
  expect_equal(diagnostics$person_rows_before, nrow(person))
  expect_equal(diagnostics$person_rows_after, nrow(person))
  expect_equal(diagnostics$household_rows, nrow(household))
  expect_equal(diagnostics$unique_person_households, 3L)
  expect_equal(diagnostics$unique_household_rows, 3L)
  expect_equal(diagnostics$housing_vars, c("vi04a", "vi05a", "vi07", "vi09", "vi10", "vi10a"))
  expect_equal(diagnostics$household_id, "id_hogar")
  expect_true(diagnostics$strict)
  expect_false(diagnostics$overwrite)
})

test_that("NBI source join output feeds component and flag builders", {
  person <- .person_nbi_source_test_data()
  household <- .household_nbi_source_test_data()

  out <- enemdu_join_nbi_sources(person, household)
  out <- enemdu_build_nbi_components(out)
  flagged <- enemdu_build_nbi_flags(out)

  expect_true(all(paste0("comp", 1:5) %in% names(flagged)))
  expect_true(all(c("knbi", "nbi", "xnbi") %in% names(flagged)))
  expect_equal(flagged$knbi[1], 5L)
  expect_equal(flagged$nbi[1], 1L)
  expect_equal(flagged$xnbi[1], 1L)
})
