.ipm_repro_repo_path <- function(...) {
  candidates <- c(
    file.path(...),
    file.path("..", "..", ...)
  )
  existing <- candidates[file.exists(candidates)]

  if (length(existing) > 0) {
    return(existing[[1]])
  }

  candidates[[1]]
}

.ipm_repro_extdata_path <- function(file) {
  installed_path <- system.file("extdata", file, package = "enemduR")

  if (nzchar(installed_path)) {
    return(installed_path)
  }

  candidates <- c(
    file.path("inst", "extdata", file),
    file.path("..", "..", "inst", "extdata", file)
  )
  existing <- candidates[file.exists(candidates)]

  if (length(existing) > 0) {
    return(existing[[1]])
  }

  candidates[[1]]
}

if (!exists("enemdu_run_ipm_reproducibility")) {
  source(.ipm_repro_repo_path("R", "utils-errors.R"), local = TRUE)
  source(.ipm_repro_repo_path("R", "utils-metadata.R"), local = TRUE)
  .enemdu_extdata_path <- function(file) .ipm_repro_extdata_path(file)
  .enemdu_read_csv_registry <- function(file) {
    readr::read_csv(
      .ipm_repro_extdata_path(file),
      show_col_types = FALSE,
      progress = FALSE
    )
  }
  source(.ipm_repro_repo_path("R", "design.R"), local = TRUE)
  source(.ipm_repro_repo_path("R", "representativity_scope.R"), local = TRUE)
  source(.ipm_repro_repo_path("R", "representativity_rules.R"), local = TRUE)
  source(.ipm_repro_repo_path("R", "indicator_estimate.R"), local = TRUE)
  source(.ipm_repro_repo_path("R", "tabulate.R"), local = TRUE)
  source(.ipm_repro_repo_path("R", "survey_estimators.R"), local = TRUE)
  source(.ipm_repro_repo_path("R", "nbi_sources.R"), local = TRUE)
  source(.ipm_repro_repo_path("R", "ipm_sources.R"), local = TRUE)
  source(.ipm_repro_repo_path("R", "ipm_flags.R"), local = TRUE)
  source(.ipm_repro_repo_path("R", "ipm_components_household.R"), local = TRUE)
  source(.ipm_repro_repo_path("R", "ipm_components.R"), local = TRUE)
  source(.ipm_repro_repo_path("R", "ipm_kpis.R"), local = TRUE)
  source(.ipm_repro_repo_path("R", "ipm_benchmarks.R"), local = TRUE)
  source(.ipm_repro_repo_path("R", "ipm_reproducibility.R"), local = TRUE)
}

.ipm_repro_component_registry <- function() {
  registry <- read.csv(
    .ipm_repro_extdata_path("ipm_component_registry.csv"),
    stringsAsFactors = FALSE,
    check.names = FALSE,
    fileEncoding = "UTF-8"
  )
  registry[order(registry$indicator_order), , drop = FALSE]
}

.ipm_repro_component_names <- function() {
  .ipm_repro_component_registry()$expected_component_name
}

.ipm_repro_flag_data <- function(scores = c(0, 0.4, 0.5, 0.8, 0, 0, 0.4, 0.9),
                                 area = c("1", "1", "1", "1", "2", "2", "2", "2")) {
  tibble::tibble(
    upm = seq_along(scores),
    estrato = rep(seq_len(length(scores) / 2), each = 2L),
    fexp = rep(1, length(scores)),
    area = area,
    ipm_score = scores,
    tpm = as.integer(scores >= 1 / 3),
    tpem = as.integer(scores >= 1 / 2)
  )
}

.ipm_repro_component_data <- function() {
  component_names <- .ipm_repro_component_names()
  n <- 8L
  values <- matrix(0L, nrow = n, ncol = length(component_names))
  values[2, 1:4] <- 1L
  values[3, 1:6] <- 1L
  values[4, ] <- 1L
  values[8, 7:10] <- 1L
  colnames(values) <- component_names

  out <- tibble::as_tibble(as.data.frame(values, optional = TRUE))
  out$upm <- seq_len(n)
  out$estrato <- rep(seq_len(n / 2), each = 2L)
  out$fexp <- rep(1, n)
  out$area <- rep(c("1", "2"), each = n / 2)
  out
}

test_that("IPM reproducibility functions are exported when namespace is available", {
  if (!"enemduR" %in% loadedNamespaces()) {
    testthat::skip("Export checks require the package namespace.")
  }

  expected <- c(
    "enemdu_validate_ipm_reproducibility_inputs",
    "enemdu_run_ipm_reproducibility"
  )

  expect_true(all(expected %in% getNamespaceExports("enemduR")))
  expect_true(is.function(getExportedValue("enemduR", expected[[1]])))
  expect_true(is.function(getExportedValue("enemduR", expected[[2]])))
})

test_that("IPM reproducibility input validation passes with required variables", {
  preflight <- enemdu_validate_ipm_reproducibility_inputs(
    .ipm_repro_flag_data(),
    strict = TRUE
  )

  expect_s3_class(preflight, "enemdu_ipm_reproducibility_preflight")
  expect_true(isTRUE(attr(preflight, "preflight_passed")))
  expect_true(all(preflight$issue == "ok"))
})

test_that("IPM reproducibility input validation fails when key variables are missing", {
  data <- .ipm_repro_flag_data()
  data$ipm_score <- NULL

  expect_error(
    enemdu_validate_ipm_reproducibility_inputs(data, strict = TRUE),
    class = "enemdu_error_ipm_reproducibility_preflight_failed"
  )

  preflight <- enemdu_validate_ipm_reproducibility_inputs(data, strict = FALSE)
  score_row <- preflight[preflight$variable == "ipm_score", , drop = FALSE]

  expect_false(isTRUE(attr(preflight, "preflight_passed")))
  expect_equal(score_row$issue, "missing_variable")
})

test_that("IPM reproducibility workflow returns structured outputs", {
  result <- enemdu_run_ipm_reproducibility(
    .ipm_repro_flag_data(),
    build_flags = FALSE,
    strict = TRUE,
    sample_n_min = 1
  )

  expect_s3_class(result, "enemdu_ipm_reproducibility_result")
  expect_true(all(c(
    "preflight",
    "validation",
    "estimates",
    "benchmarks",
    "comparison",
    "official_validation_status"
  ) %in% names(result)))
  expect_equal(result$validation$validation_status, "passed")
})

test_that("IPM reproducibility workflow does not claim official validation", {
  result <- enemdu_run_ipm_reproducibility(
    .ipm_repro_flag_data(),
    build_flags = FALSE,
    strict = TRUE,
    sample_n_min = 1
  )

  expect_equal(result$official_validation_status, "not_officially_validated")
  expect_equal(unique(result$comparison$official_validation_status), "not_officially_validated")
})

test_that("IPM reproducibility workflow can run on prebuilt score and flags", {
  result <- enemdu_run_ipm_reproducibility(
    .ipm_repro_flag_data(),
    build_flags = FALSE,
    strict = TRUE,
    sample_n_min = 1
  )

  expect_true(all(c("tpm", "tpem", "A", "ipm") %in% result$estimates$indicator_id))
  expect_true(all(c("national", "urban", "rural") %in% result$estimates$domain_value))
})

test_that("IPM reproducibility workflow can run from synthetic components", {
  result <- enemdu_run_ipm_reproducibility(
    .ipm_repro_component_data(),
    build_flags = TRUE,
    strict = TRUE,
    sample_n_min = 1
  )

  expect_true(all(c("tpm", "tpem", "ipm") %in% result$comparison$indicator_id))
  expect_true(all(c("national", "urban", "rural") %in% result$comparison$domain_value))
})

test_that("IPM reproducibility workflow does not require raw microdata files", {
  result <- enemdu_run_ipm_reproducibility(
    .ipm_repro_flag_data(),
    build_flags = FALSE,
    strict = TRUE,
    sample_n_min = 1
  )

  expect_false("microdata_file" %in% names(result))
  expect_false("microdata_path" %in% names(attr(result, "reproducibility_policy")))
})
