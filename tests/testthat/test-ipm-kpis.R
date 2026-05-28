.ipm_kpis_repo_path <- function(...) {
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

.ipm_kpis_extdata_path <- function(file) {
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

if (!exists("enemdu_kpi_ipm")) {
  source(.ipm_kpis_repo_path("R", "utils-errors.R"), local = TRUE)
  source(.ipm_kpis_repo_path("R", "utils-metadata.R"), local = TRUE)
  .enemdu_extdata_path <- function(file) .ipm_kpis_extdata_path(file)
  .enemdu_read_csv_registry <- function(file) {
    readr::read_csv(
      .ipm_kpis_extdata_path(file),
      show_col_types = FALSE,
      progress = FALSE
    )
  }
  source(.ipm_kpis_repo_path("R", "design.R"), local = TRUE)
  source(.ipm_kpis_repo_path("R", "representativity_scope.R"), local = TRUE)
  source(.ipm_kpis_repo_path("R", "representativity_rules.R"), local = TRUE)
  source(.ipm_kpis_repo_path("R", "indicator_estimate.R"), local = TRUE)
  source(.ipm_kpis_repo_path("R", "tabulate.R"), local = TRUE)
  source(.ipm_kpis_repo_path("R", "survey_estimators.R"), local = TRUE)
  source(.ipm_kpis_repo_path("R", "nbi_sources.R"), local = TRUE)
  source(.ipm_kpis_repo_path("R", "ipm_sources.R"), local = TRUE)
  source(.ipm_kpis_repo_path("R", "ipm_flags.R"), local = TRUE)
  source(.ipm_kpis_repo_path("R", "ipm_components_household.R"), local = TRUE)
  source(.ipm_kpis_repo_path("R", "ipm_components.R"), local = TRUE)
  source(.ipm_kpis_repo_path("R", "ipm_kpis.R"), local = TRUE)
}

.ipm_kpis_component_registry <- function() {
  registry <- read.csv(
    .ipm_kpis_extdata_path("ipm_component_registry.csv"),
    stringsAsFactors = FALSE,
    check.names = FALSE,
    fileEncoding = "UTF-8"
  )
  registry[order(registry$indicator_order), , drop = FALSE]
}

.ipm_kpis_component_names <- function() {
  .ipm_kpis_component_registry()$expected_component_name
}

.ipm_kpi_flag_data <- function(scores = c(0, 0.4, 0.5, 0.8, 0, 0, 0.4, 0.9),
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

.ipm_kpi_component_data <- function() {
  component_names <- .ipm_kpis_component_names()
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

test_that("IPM KPI functions are exported when package namespace is available", {
  if (!"enemduR" %in% loadedNamespaces()) {
    testthat::skip("Export checks require the package namespace.")
  }

  expect_true("enemdu_kpi_ipm" %in% getNamespaceExports("enemduR"))
  expect_true(is.function(getExportedValue("enemduR", "enemdu_kpi_ipm")))
})

test_that("IPM KPI estimates TPM and TPEM from prebuilt flags", {
  out <- enemdu_kpi_ipm(
    .ipm_kpi_flag_data(),
    build_flags = FALSE,
    sample_n_min = 1
  )

  tpm <- out[out$indicator_id == "tpm", , drop = FALSE]
  tpem <- out[out$indicator_id == "tpem", , drop = FALSE]

  expect_equal(tpm$estimate, 5 / 8, tolerance = 1e-12)
  expect_equal(tpem$estimate, 3 / 8, tolerance = 1e-12)
})

test_that("IPM KPI estimates A among multidimensionally poor persons", {
  out <- enemdu_kpi_ipm(
    .ipm_kpi_flag_data(),
    build_flags = FALSE,
    sample_n_min = 1
  )

  a_row <- out[out$indicator_id == "A", , drop = FALSE]

  expect_equal(a_row$estimate, 0.6, tolerance = 1e-12)
})

test_that("IPM KPI estimates aggregate IPM as TPM multiplied by A", {
  out <- enemdu_kpi_ipm(
    .ipm_kpi_flag_data(),
    build_flags = FALSE,
    sample_n_min = 1
  )

  ipm_row <- out[out$indicator_id == "ipm", , drop = FALSE]

  expect_equal(ipm_row$estimate, (5 / 8) * 0.6, tolerance = 1e-12)
})

test_that("IPM KPI can build flags from 12 synthetic components", {
  out <- enemdu_kpi_ipm(
    .ipm_kpi_component_data(),
    build_flags = TRUE,
    sample_n_min = 1
  )

  expect_true(all(c("tpm", "tpem", "A", "ipm") %in% out$indicator_id))
  expect_true(any(out$estimate[out$indicator_id == "tpm"] > 0))
})

test_that("IPM KPI supports by = area", {
  out <- enemdu_kpi_ipm(
    .ipm_kpi_flag_data(),
    by = "area",
    build_flags = FALSE,
    sample_n_min = 1
  )

  tpm_rows <- out[out$indicator_id == "tpm", , drop = FALSE]

  expect_true(all(c("urban", "rural") %in% tpm_rows$domain_value))
  expect_equal(nrow(tpm_rows), 2L)
})

test_that("IPM KPI handles domains with no poor persons safely", {
  data <- .ipm_kpi_flag_data(
    scores = c(0.4, 0.5, 0.8, 0.9, 0, 0, 0, 0),
    area = c("1", "1", "1", "1", "2", "2", "2", "2")
  )

  out <- enemdu_kpi_ipm(
    data,
    by = "area",
    build_flags = FALSE,
    sample_n_min = 1
  )

  a_rural <- out[out$indicator_id == "A" & out$domain_value == "rural", , drop = FALSE]
  ipm_rural <- out[out$indicator_id == "ipm" & out$domain_value == "rural", , drop = FALSE]

  expect_equal(a_rural$estimate, 0)
  expect_equal(ipm_rural$estimate, 0)
})

test_that("IPM KPI does not require raw component derivation unless requested", {
  data <- .ipm_kpi_flag_data()

  out <- enemdu_kpi_ipm(
    data,
    build_components = FALSE,
    build_flags = FALSE,
    sample_n_min = 1
  )

  expect_true(all(c("tpm", "tpem", "A", "ipm") %in% out$indicator_id))
})

test_that("IPM KPI returns expected indicator IDs", {
  out <- enemdu_kpi_ipm(
    .ipm_kpi_flag_data(),
    build_flags = FALSE,
    sample_n_min = 1
  )

  expect_setequal(out$indicator_id, c("tpm", "tpem", "A", "ipm"))
})
