.ipm_household_components_repo_path <- function(...) {
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

.ipm_household_components_extdata_path <- function(file) {
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

if (!exists(".enemdu_build_ipm_low_risk_household_components")) {
  source(.ipm_household_components_repo_path("R", "utils-errors.R"), local = TRUE)
  .enemdu_read_csv_registry <- function(file) {
    readr::read_csv(
      .ipm_household_components_extdata_path(file),
      show_col_types = FALSE,
      progress = FALSE
    )
  }
  source(.ipm_household_components_repo_path("R", "ipm_flags.R"), local = TRUE)
  source(.ipm_household_components_repo_path("R", "ipm_components_household.R"), local = TRUE)
}

.ipm_household_component_registry <- function() {
  read.csv(
    .ipm_household_components_extdata_path("ipm_component_registry.csv"),
    stringsAsFactors = FALSE,
    check.names = FALSE,
    fileEncoding = "UTF-8"
  )
}

.ipm_household_component_names <- function() {
  registry <- .ipm_household_component_registry()
  registry <- registry[order(registry$indicator_order), , drop = FALSE]
  registry$expected_component_name
}

.ipm_low_risk_household_component_names <- function() {
  registry <- .ipm_household_component_registry()
  rows <- match(
    c("ipm_i08_sin_agua_red_publica", "ipm_i09_hacinamiento"),
    registry$indicator_id
  )
  registry$expected_component_name[rows]
}

.ipm_household_component_test_data <- function() {
  tibble::tibble(
    id_hogar = c("h1", "h1", "h2", "h2", "h3", "h3", "h3", "h4", "h4", "h4", "h4"),
    p01 = c(1, 2, 1, 2, 1, 2, 3, 1, 2, 3, 4),
    row_id = seq_len(11),
    vi10 = c(1, 1, 2, 2, 1, 1, 1, 1, 1, 1, 1),
    vi07 = c(1, 1, 1, 1, 0, 0, 0, 1, 1, 1, 1)
  )
}

test_that("IPM low-risk household helper creates only the two expected components", {
  data <- .ipm_household_component_test_data()
  out <- .enemdu_build_ipm_low_risk_household_components(data)
  added_vars <- setdiff(names(out), names(data))

  expect_setequal(added_vars, .ipm_low_risk_household_component_names())
})

test_that("IPM low-risk household helper does not create other IPM components", {
  data <- .ipm_household_component_test_data()
  out <- .enemdu_build_ipm_low_risk_household_components(data)

  other_components <- setdiff(
    .ipm_household_component_names(),
    .ipm_low_risk_household_component_names()
  )

  expect_false(any(other_components %in% names(out)))
})

test_that("IPM low-risk household helper does not create score, flags, or aggregates", {
  data <- .ipm_household_component_test_data()
  out <- .enemdu_build_ipm_low_risk_household_components(data)

  expect_false(any(c("ipm_score", "tpm", "tpem", "A", "ipm") %in% names(out)))
})

test_that("IPM low-risk household helper flags public-network water as not deprived", {
  out <- .enemdu_build_ipm_low_risk_household_components(.ipm_household_component_test_data())
  water_component <- .ipm_low_risk_household_component_names()[1]

  expect_equal(out[[water_component]][out$id_hogar == "h1"], c(0L, 0L))
})

test_that("IPM low-risk household helper flags non-public-network water as deprived", {
  out <- .enemdu_build_ipm_low_risk_household_components(.ipm_household_component_test_data())
  water_component <- .ipm_low_risk_household_component_names()[1]

  expect_equal(out[[water_component]][out$id_hogar == "h2"], c(1L, 1L))
})

test_that("IPM low-risk household helper aborts on missing water in strict mode", {
  data <- .ipm_household_component_test_data()
  data$vi10[1] <- NA

  expect_error(
    .enemdu_build_ipm_low_risk_household_components(data, strict = TRUE),
    class = "enemdu_error_missing_ipm_household_source"
  )
})

test_that("IPM low-risk household helper propagates missing water when strict is false", {
  data <- .ipm_household_component_test_data()
  data$vi10[1] <- NA

  out <- .enemdu_build_ipm_low_risk_household_components(data, strict = FALSE)
  water_component <- .ipm_low_risk_household_component_names()[1]

  expect_true(is.na(out[[water_component]][1]))
  expect_equal(out[[water_component]][2], 0L)
})

test_that("IPM low-risk household helper derives household size when hsize is absent", {
  out <- .enemdu_build_ipm_low_risk_household_components(.ipm_household_component_test_data())
  overcrowding_component <- .ipm_low_risk_household_component_names()[2]
  diagnostics <- attr(out, "ipm_low_risk_household_component_diagnostics")

  expect_true(diagnostics$hsize_was_derived)
  expect_equal(out[[overcrowding_component]][out$id_hogar == "h4"], rep(1L, 4))
})

test_that("IPM low-risk household helper uses existing hsize when present", {
  data <- .ipm_household_component_test_data()
  data <- data[data$id_hogar == "h1", , drop = FALSE]
  data$hsize <- 4

  out <- .enemdu_build_ipm_low_risk_household_components(data)
  overcrowding_component <- .ipm_low_risk_household_component_names()[2]
  diagnostics <- attr(out, "ipm_low_risk_household_component_diagnostics")

  expect_false(diagnostics$hsize_was_derived)
  expect_equal(diagnostics$hsize_var_used, "hsize")
  expect_equal(out[[overcrowding_component]], c(1L, 1L))
})

test_that("IPM low-risk household helper detects overcrowding when ratio is greater than three", {
  out <- .enemdu_build_ipm_low_risk_household_components(.ipm_household_component_test_data())
  overcrowding_component <- .ipm_low_risk_household_component_names()[2]

  expect_equal(out[[overcrowding_component]][out$id_hogar == "h4"], rep(1L, 4))
})

test_that("IPM low-risk household helper keeps non-overcrowded households at zero", {
  out <- .enemdu_build_ipm_low_risk_household_components(.ipm_household_component_test_data())
  overcrowding_component <- .ipm_low_risk_household_component_names()[2]

  expect_equal(out[[overcrowding_component]][out$id_hogar == "h1"], c(0L, 0L))
})

test_that("IPM low-risk household helper treats zero bedrooms as deprivation", {
  out <- .enemdu_build_ipm_low_risk_household_components(.ipm_household_component_test_data())
  overcrowding_component <- .ipm_low_risk_household_component_names()[2]
  diagnostics <- attr(out, "ipm_low_risk_household_component_diagnostics")

  expect_equal(out[[overcrowding_component]][out$id_hogar == "h3"], rep(1L, 3))
  expect_equal(diagnostics$n_zero_bedrooms, 1L)
})

test_that("IPM low-risk household helper aborts on missing bedrooms in strict mode", {
  data <- .ipm_household_component_test_data()
  data$vi07[1] <- NA

  expect_error(
    .enemdu_build_ipm_low_risk_household_components(data, strict = TRUE),
    class = "enemdu_error_missing_ipm_household_source"
  )
})

test_that("IPM low-risk household helper propagates missing bedrooms when strict is false", {
  data <- .ipm_household_component_test_data()
  data$vi07[1] <- NA

  out <- .enemdu_build_ipm_low_risk_household_components(data, strict = FALSE)
  overcrowding_component <- .ipm_low_risk_household_component_names()[2]

  expect_true(is.na(out[[overcrowding_component]][1]))
  expect_equal(out[[overcrowding_component]][2], 0L)
})

test_that("IPM low-risk household helper aborts on negative bedrooms in strict mode", {
  data <- .ipm_household_component_test_data()
  data$vi07[1] <- -1

  expect_error(
    .enemdu_build_ipm_low_risk_household_components(data, strict = TRUE),
    class = "enemdu_error_invalid_ipm_household_source"
  )
})

test_that("IPM low-risk household helper protects existing outputs unless overwrite is requested", {
  data <- .ipm_household_component_test_data()
  data[[.ipm_low_risk_household_component_names()[1]]] <- 99L

  expect_error(
    .enemdu_build_ipm_low_risk_household_components(data, overwrite = FALSE),
    class = "enemdu_error_existing_ipm_household_components"
  )
})

test_that("IPM low-risk household helper replaces existing outputs when overwrite is true", {
  data <- .ipm_household_component_test_data()
  component_names <- .ipm_low_risk_household_component_names()
  data[[component_names[1]]] <- 99L
  data[[component_names[2]]] <- 99L

  out <- .enemdu_build_ipm_low_risk_household_components(data, overwrite = TRUE)

  expect_equal(out[[component_names[1]]][out$id_hogar == "h1"], c(0L, 0L))
  expect_equal(out[[component_names[2]]][out$id_hogar == "h1"], c(0L, 0L))
})

test_that("IPM low-risk household helper preserves row count and order", {
  data <- .ipm_household_component_test_data()
  out <- .enemdu_build_ipm_low_risk_household_components(data)

  expect_equal(nrow(out), nrow(data))
  expect_equal(out$row_id, data$row_id)
  expect_equal(out$id_hogar, data$id_hogar)
})

test_that("IPM low-risk household helper attaches diagnostics", {
  data <- .ipm_household_component_test_data()
  out <- .enemdu_build_ipm_low_risk_household_components(data)
  diagnostics <- attr(out, "ipm_low_risk_household_component_diagnostics")

  expect_type(diagnostics, "list")
  expect_equal(diagnostics$n_rows, nrow(data))
  expect_equal(diagnostics$n_households, 4L)
  expect_equal(diagnostics$household_id, "id_hogar")
  expect_equal(diagnostics$water_var, "vi10")
  expect_equal(diagnostics$bedrooms_var, "vi07")
  expect_equal(diagnostics$output_components, unname(.ipm_low_risk_household_component_names()))
})

test_that("IPM low-risk household components can feed IPM flags with dummy components", {
  data <- .ipm_household_component_test_data()
  out <- .enemdu_build_ipm_low_risk_household_components(data)
  all_components <- .ipm_household_component_names()
  low_risk_components <- .ipm_low_risk_household_component_names()
  dummy_components <- setdiff(all_components, low_risk_components)

  for (component in dummy_components) {
    out[[component]] <- 0L
  }

  flagged <- enemdu_build_ipm_flags(out)

  expect_true(all(c("ipm_score", "tpm", "tpem") %in% names(flagged)))
  expect_equal(flagged$ipm_score[flagged$id_hogar == "h2"], rep(0.125, 2))
  expect_equal(flagged$ipm_score[flagged$id_hogar == "h4"], rep(0.0625, 4))
})
