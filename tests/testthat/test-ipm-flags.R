.ipm_flags_extdata_path <- function(file) {
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

.ipm_flags_repo_path <- function(...) {
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

if (!exists("enemdu_build_ipm_flags")) {
  source(.ipm_flags_repo_path("R", "utils-errors.R"), local = TRUE)
  source(.ipm_flags_repo_path("R", "utils-metadata.R"), local = TRUE)
  .enemdu_read_csv_registry <- function(file) {
    readr::read_csv(
      .ipm_flags_extdata_path(file),
      show_col_types = FALSE,
      progress = FALSE
    )
  }
  source(.ipm_flags_repo_path("R", "ipm_flags.R"), local = TRUE)
}

.ipm_flags_component_registry <- function() {
  read.csv(
    .ipm_flags_extdata_path("ipm_component_registry.csv"),
    stringsAsFactors = FALSE,
    check.names = FALSE,
    fileEncoding = "UTF-8"
  )
}

.ipm_flags_component_names <- function() {
  registry <- .ipm_flags_component_registry()
  registry <- registry[order(registry$indicator_order), , drop = FALSE]
  registry$expected_component_name
}

.ipm_flags_weights <- function() {
  registry <- .ipm_flags_component_registry()
  registry <- registry[order(registry$indicator_order), , drop = FALSE]
  registry$indicator_weight
}

.ipm_flags_test_data <- function() {
  component_names <- .ipm_flags_component_names()
  values <- matrix(0L, nrow = 4, ncol = length(component_names))
  values[2, 1:4] <- 1L
  values[3, 1:6] <- 1L
  values[4, ] <- 1L
  colnames(values) <- component_names

  out <- tibble::as_tibble(as.data.frame(values, optional = TRUE))
  out
}

test_that("IPM flags add row-level score and poverty flags", {
  out <- enemdu_build_ipm_flags(.ipm_flags_test_data())

  expect_true(all(c("ipm_score", "tpm", "tpem") %in% names(out)))
  expect_type(out$ipm_score, "double")
  expect_type(out$tpm, "integer")
  expect_type(out$tpem, "integer")
})

test_that("IPM flags preserve rows with no deprivations as non-poor", {
  out <- enemdu_build_ipm_flags(.ipm_flags_test_data())

  expect_equal(out$ipm_score[1], 0)
  expect_equal(out$tpm[1], 0L)
  expect_equal(out$tpem[1], 0L)
})

test_that("IPM flags classify the TPM threshold inclusively", {
  out <- enemdu_build_ipm_flags(.ipm_flags_test_data())

  expect_equal(out$ipm_score[2], 1 / 3, tolerance = 1e-12)
  expect_equal(out$tpm[2], 1L)
  expect_equal(out$tpem[2], 0L)
})

test_that("IPM flags classify the TPEM threshold inclusively", {
  out <- enemdu_build_ipm_flags(.ipm_flags_test_data())

  expect_equal(out$ipm_score[3], 1 / 2, tolerance = 1e-12)
  expect_equal(out$tpm[3], 1L)
  expect_equal(out$tpem[3], 1L)
})

test_that("IPM flags score all deprivations as one", {
  out <- enemdu_build_ipm_flags(.ipm_flags_test_data())

  expect_equal(out$ipm_score[4], 1, tolerance = 1e-12)
  expect_equal(out$tpm[4], 1L)
  expect_equal(out$tpem[4], 1L)
})

test_that("IPM flags abort when required component columns are missing", {
  data <- .ipm_flags_test_data()
  data[[.ipm_flags_component_names()[1]]] <- NULL

  expect_error(
    enemdu_build_ipm_flags(data, strict = TRUE),
    class = "enemdu_error_missing_vars"
  )
})

test_that("IPM flags abort on invalid binary values in strict mode", {
  data <- .ipm_flags_test_data()
  data[[.ipm_flags_component_names()[1]]][1] <- 2L

  expect_error(
    enemdu_build_ipm_flags(data, strict = TRUE),
    class = "enemdu_error_invalid_ipm_component"
  )
})

test_that("IPM flags abort on missing values in strict mode", {
  data <- .ipm_flags_test_data()
  data[[.ipm_flags_component_names()[1]]][1] <- NA_integer_

  expect_error(
    enemdu_build_ipm_flags(data, strict = TRUE),
    class = "enemdu_error_missing_ipm_component"
  )
})

test_that("IPM flags propagate missing values when strict is false", {
  data <- .ipm_flags_test_data()
  data[[.ipm_flags_component_names()[1]]][1] <- NA_integer_

  out <- enemdu_build_ipm_flags(data, strict = FALSE)

  expect_true(is.na(out$ipm_score[1]))
  expect_true(is.na(out$tpm[1]))
  expect_true(is.na(out$tpem[1]))
  expect_equal(out$ipm_score[2], 1 / 3, tolerance = 1e-12)
})

test_that("IPM flags protect existing outputs unless overwrite is requested", {
  data <- .ipm_flags_test_data()
  data$ipm_score <- 99

  expect_error(
    enemdu_build_ipm_flags(data),
    class = "enemdu_error_existing_ipm_output"
  )
})

test_that("IPM flags replace existing outputs when overwrite is true", {
  data <- .ipm_flags_test_data()
  data$ipm_score <- 99
  data$tpm <- 99L
  data$tpem <- 99L

  out <- enemdu_build_ipm_flags(data, overwrite = TRUE)

  expect_equal(out$ipm_score[1], 0)
  expect_equal(out$tpm[1], 0L)
  expect_equal(out$tpem[1], 0L)
})

test_that("IPM flags do not create aggregate A or aggregate ipm", {
  out <- enemdu_build_ipm_flags(.ipm_flags_test_data())

  expect_false("A" %in% names(out))
  expect_false("ipm" %in% names(out))
})

test_that("IPM flags default component names align with the registry", {
  out <- enemdu_build_ipm_flags(.ipm_flags_test_data())
  diagnostics <- attr(out, "ipm_flags_diagnostics")

  expect_equal(diagnostics$component_cols, .ipm_flags_component_names())
  expect_equal(diagnostics$n_components, 12L)
})

test_that("IPM flags use registry weights that sum to one", {
  out <- enemdu_build_ipm_flags(.ipm_flags_test_data())
  diagnostics <- attr(out, "ipm_flags_diagnostics")

  expect_equal(sum(.ipm_flags_weights()), 1, tolerance = 1e-12)
  expect_equal(diagnostics$weights_sum, 1, tolerance = 1e-12)
})

test_that("IPM flags match weights by component name when order is supplied", {
  component_names <- .ipm_flags_component_names()
  reversed_names <- rev(component_names)
  data <- .ipm_flags_test_data()
  data[1, ] <- 0L
  data[[component_names[12]]][1] <- 1L

  out <- enemdu_build_ipm_flags(
    data = data,
    component_cols = reversed_names
  )

  expect_equal(out$ipm_score[1], 0.0625)
})

test_that("IPM flags safely coerce logical and visible 0/1 factor values", {
  component_names <- .ipm_flags_component_names()
  data <- .ipm_flags_test_data()
  data[[component_names[1]]] <- as.logical(data[[component_names[1]]])
  data[[component_names[2]]] <- factor(
    as.character(data[[component_names[2]]]),
    levels = c("0", "1")
  )

  out <- enemdu_build_ipm_flags(data)

  expect_equal(out$tpm[2], 1L)
})
