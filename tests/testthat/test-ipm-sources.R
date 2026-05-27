.ipm_sources_repo_path <- function(...) {
  repo_path <- file.path(...)
  if (file.exists(repo_path)) {
    return(repo_path)
  }

  test_path <- file.path("..", "..", ...)
  if (file.exists(test_path)) {
    return(test_path)
  }

  repo_path
}

.ipm_sources_extdata_path <- function(file) {
  repo_path <- file.path("inst", "extdata", file)
  if (file.exists(repo_path)) {
    return(repo_path)
  }

  test_path <- file.path("..", "..", "inst", "extdata", file)
  if (file.exists(test_path)) {
    return(test_path)
  }

  installed_path <- system.file("extdata", file, package = "enemduR")
  if (nzchar(installed_path)) {
    return(installed_path)
  }

  repo_path
}

if (!exists("enemdu_join_ipm_sources")) {
  source(.ipm_sources_repo_path("R", "nbi_sources.R"), local = TRUE)
  source(.ipm_sources_repo_path("R", "ipm_sources.R"), local = TRUE)
}

.ipm_sources_default_housing_vars <- function() {
  c(
    "vi03a",
    "vi03b",
    "vi04a",
    "vi04b",
    "vi05a",
    "vi05b",
    "vi07",
    "vi09",
    "vi10",
    "vi13"
  )
}

.ipm_sources_component_names <- function() {
  registry <- read.csv(
    .ipm_sources_extdata_path("ipm_component_registry.csv"),
    stringsAsFactors = FALSE,
    check.names = FALSE,
    fileEncoding = "UTF-8"
  )

  registry$expected_component_name
}

.person_ipm_source_test_data <- function() {
  tibble::tibble(
    id_hogar = c("h1", "h1", "h2", "h2", "h3", "h3"),
    row_id = seq_len(6),
    p01 = c(1, 2, 1, 2, 1, 2),
    p03 = c(40, 8, 45, 10, 50, 20),
    area = c(1, 1, 2, 2, 1, 1)
  )
}

.household_ipm_source_test_data <- function() {
  tibble::tibble(
    id_hogar = c("h1", "h2", "h3"),
    vi03a = c(1, 2, 3),
    vi03b = c(1, 1, 2),
    vi04a = c(7, 1, 1),
    vi04b = c(1, 1, 2),
    vi05a = c(1, 6, 1),
    vi05b = c(1, 2, 2),
    vi07 = c(0, 1, 2),
    vi09 = c(5, 1, 1),
    vi10 = c(1, 2, 1),
    vi13 = c(2, 1, 1),
    custom_var = c(10, 20, 30)
  )
}

test_that("IPM source join preserves person row count", {
  person <- .person_ipm_source_test_data()
  household <- .household_ipm_source_test_data()

  out <- enemdu_join_ipm_sources(person, household)

  expect_equal(nrow(out), nrow(person))
})

test_that("IPM source join preserves person row order", {
  person <- .person_ipm_source_test_data()
  household <- .household_ipm_source_test_data()

  out <- enemdu_join_ipm_sources(person, household)

  expect_equal(out$row_id, person$row_id)
  expect_equal(out$id_hogar, person$id_hogar)
})

test_that("IPM source join appends the default household variables", {
  person <- .person_ipm_source_test_data()
  household <- .household_ipm_source_test_data()

  out <- enemdu_join_ipm_sources(person, household)

  expect_true(all(.ipm_sources_default_housing_vars() %in% names(out)))
  expect_false("vi10a" %in% names(out))
  expect_equal(out$vi03a, c(1, 1, 2, 2, 3, 3))
  expect_equal(out$vi13, c(2, 2, 1, 1, 1, 1))
})

test_that("IPM source join supports custom housing variables", {
  person <- .person_ipm_source_test_data()
  household <- .household_ipm_source_test_data()

  out <- enemdu_join_ipm_sources(
    data = person,
    household_data = household,
    housing_vars = "custom_var"
  )

  expect_true("custom_var" %in% names(out))
  expect_equal(out$custom_var, c(10, 10, 20, 20, 30, 30))
})

test_that("IPM source join aborts when household variables are missing", {
  person <- .person_ipm_source_test_data()
  household <- .household_ipm_source_test_data()
  household$vi13 <- NULL

  expect_error(
    enemdu_join_ipm_sources(person, household, strict = TRUE),
    class = "enemdu_error_missing_nbi_housing_vars"
  )
})

test_that("IPM source join aborts on duplicated household rows in strict mode", {
  person <- .person_ipm_source_test_data()
  household <- .household_ipm_source_test_data()
  household <- rbind(household, household[1, ])

  expect_error(
    enemdu_join_ipm_sources(person, household, strict = TRUE),
    class = "enemdu_error_duplicate_nbi_household_ids"
  )
})

test_that("IPM source join aborts on unmatched households in strict mode", {
  person <- .person_ipm_source_test_data()
  household <- .household_ipm_source_test_data()
  person$id_hogar[6] <- "h4"

  expect_error(
    enemdu_join_ipm_sources(person, household, strict = TRUE),
    class = "enemdu_error_unmatched_nbi_households"
  )
})

test_that("IPM source join protects existing variables unless overwrite is requested", {
  person <- .person_ipm_source_test_data()
  household <- .household_ipm_source_test_data()
  person$vi03a <- 99

  expect_error(
    enemdu_join_ipm_sources(person, household, overwrite = FALSE),
    class = "enemdu_error_existing_nbi_housing_vars"
  )
})

test_that("IPM source join replaces existing variables when overwrite is true", {
  person <- .person_ipm_source_test_data()
  household <- .household_ipm_source_test_data()
  person$vi03a <- 99

  out <- enemdu_join_ipm_sources(person, household, overwrite = TRUE)

  expect_equal(out$vi03a, c(1, 1, 2, 2, 3, 3))
})

test_that("IPM source join allows unmatched households when strict is false", {
  person <- .person_ipm_source_test_data()
  household <- .household_ipm_source_test_data()
  person$id_hogar[6] <- "h4"

  out <- enemdu_join_ipm_sources(person, household, strict = FALSE)
  diagnostics <- attr(out, "ipm_source_join_diagnostics")

  expect_equal(nrow(out), nrow(person))
  expect_true(is.na(out$vi03a[6]))
  expect_equal(diagnostics$unmatched_person_households, "h4")
  expect_false(diagnostics$strict)
})

test_that("IPM source join attaches IPM diagnostics", {
  person <- .person_ipm_source_test_data()
  household <- .household_ipm_source_test_data()

  out <- enemdu_join_ipm_sources(person, household)
  diagnostics <- attr(out, "ipm_source_join_diagnostics")

  expect_type(diagnostics, "list")
  expect_equal(diagnostics$person_rows_before, nrow(person))
  expect_equal(diagnostics$person_rows_after, nrow(person))
  expect_equal(diagnostics$household_rows, nrow(household))
  expect_equal(diagnostics$joined_housing_vars, .ipm_sources_default_housing_vars())
  expect_equal(diagnostics$source_join, "enemdu_join_nbi_sources_wrapper")
})

test_that("IPM source join does not create IPM component columns", {
  person <- .person_ipm_source_test_data()
  household <- .household_ipm_source_test_data()

  out <- enemdu_join_ipm_sources(person, household)

  expect_false(any(.ipm_sources_component_names() %in% names(out)))
})

test_that("IPM source join does not create IPM score, flags, or aggregate columns", {
  person <- .person_ipm_source_test_data()
  household <- .household_ipm_source_test_data()

  out <- enemdu_join_ipm_sources(person, household)

  expect_false(any(c("ipm_score", "tpm", "tpem", "A", "ipm") %in% names(out)))
})
