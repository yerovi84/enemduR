.ipm_components_repo_path <- function(...) {
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

.ipm_components_extdata_path <- function(file) {
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

if (!exists("enemdu_build_ipm_components")) {
  source(.ipm_components_repo_path("R", "utils-errors.R"), local = TRUE)
  source(.ipm_components_repo_path("R", "utils-metadata.R"), local = TRUE)
  .enemdu_read_csv_registry <- function(file) {
    readr::read_csv(
      .ipm_components_extdata_path(file),
      show_col_types = FALSE,
      progress = FALSE
    )
  }
  source(.ipm_components_repo_path("R", "nbi_sources.R"), local = TRUE)
  source(.ipm_components_repo_path("R", "ipm_sources.R"), local = TRUE)
  source(.ipm_components_repo_path("R", "ipm_flags.R"), local = TRUE)
  source(.ipm_components_repo_path("R", "ipm_components_household.R"), local = TRUE)
  source(.ipm_components_repo_path("R", "ipm_components.R"), local = TRUE)
}

.ipm_components_registry <- function() {
  registry <- read.csv(
    .ipm_components_extdata_path("ipm_component_registry.csv"),
    stringsAsFactors = FALSE,
    check.names = FALSE,
    fileEncoding = "UTF-8"
  )
  registry[order(registry$indicator_order), , drop = FALSE]
}

.ipm_component_names <- function() {
  .ipm_components_registry()$expected_component_name
}

.ipm_component_name <- function(indicator_id) {
  registry <- .ipm_components_registry()
  registry$expected_component_name[match(indicator_id, registry$indicator_id)]
}

.ipm_component_test_data <- function() {
  tibble::tibble(
    id_hogar = c("h1", "h1", "h2", "h2", "h3", "h3", "h3", "h4", "h4", "h4", "h4"),
    p01 = c(1, 2, 1, 2, 1, 2, 3, 1, 2, 3, 4),
    row_id = seq_len(11),
    vi10 = c(1, 1, 2, 2, 1, 1, 1, 1, 1, 1, 1),
    vi07 = c(1, 1, 1, 1, 0, 0, 0, 1, 1, 1, 1),
    expobre = c(0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0),
    nbi = 0L,
    pobre = 0L,
    labor_empleo = 1L
  )
}

.ipm_household_source_test_data <- function() {
  tibble::tibble(
    id_hogar = c("h1", "h2", "h3", "h4"),
    vi03a = c(1, 1, 1, 1),
    vi03b = c(1, 1, 1, 1),
    vi04a = c(1, 1, 1, 1),
    vi04b = c(1, 1, 1, 1),
    vi05a = c(1, 1, 1, 1),
    vi05b = c(1, 1, 1, 1),
    vi07 = c(1, 1, 0, 1),
    vi09 = c(1, 1, 1, 1),
    vi10 = c(1, 2, 1, 1),
    vi13 = c(1, 1, 1, 1)
  )
}

.ipm_precomputed_component_data <- function() {
  component_names <- .ipm_component_names()
  out <- tibble::tibble(
    id_hogar = c("h1", "h2"),
    p01 = c(1, 1),
    row_id = c(1, 2)
  )

  for (component in component_names) {
    out[[component]] <- 0L
  }

  out[[.ipm_component_name("ipm_i08_sin_agua_red_publica")]][2] <- 1L
  out
}

test_that("IPM component builder exists and is exported", {
  expect_true(exists("enemdu_build_ipm_components"))
  expect_true("enemdu_build_ipm_components" %in% getNamespaceExports("enemduR"))
  expect_true(is.function(getExportedValue("enemduR", "enemdu_build_ipm_components")))
})

test_that("IPM component builder preserves row count and order", {
  data <- .ipm_component_test_data()
  out <- enemdu_build_ipm_components(data, strict = FALSE)

  expect_equal(nrow(out), nrow(data))
  expect_equal(out$row_id, data$row_id)
  expect_equal(out$id_hogar, data$id_hogar)
})

test_that("IPM component builder uses names expected by the registry", {
  out <- enemdu_build_ipm_components(.ipm_component_test_data(), strict = FALSE)

  expect_true(all(.ipm_component_names() %in% names(out)))
})

test_that("IPM component builder preserves precomputed components when overwrite is false", {
  data <- .ipm_component_test_data()
  water_component <- .ipm_component_name("ipm_i08_sin_agua_red_publica")
  data[[water_component]] <- 1L

  out <- enemdu_build_ipm_components(data, strict = FALSE, overwrite = FALSE)

  expect_equal(out[[water_component]][out$id_hogar == "h1"], c(1, 1))
})

test_that("IPM component builder replaces implemented components when overwrite is true", {
  data <- .ipm_component_test_data()
  water_component <- .ipm_component_name("ipm_i08_sin_agua_red_publica")
  data[[water_component]] <- 1L

  out <- enemdu_build_ipm_components(data, strict = FALSE, overwrite = TRUE)

  expect_equal(out[[water_component]][out$id_hogar == "h1"], c(0, 0))
})

test_that("IPM component builder constructs water and overcrowding components", {
  out <- enemdu_build_ipm_components(.ipm_component_test_data(), strict = FALSE)
  water_component <- .ipm_component_name("ipm_i08_sin_agua_red_publica")
  overcrowding_component <- .ipm_component_name("ipm_i09_hacinamiento")

  expect_equal(out[[water_component]][out$id_hogar == "h1"], c(0L, 0L))
  expect_equal(out[[water_component]][out$id_hogar == "h2"], c(1L, 1L))
  expect_equal(out[[overcrowding_component]][out$id_hogar == "h4"], rep(1L, 4))
  expect_equal(out[[overcrowding_component]][out$id_hogar == "h1"], c(0L, 0L))
})

test_that("IPM component builder constructs extreme poverty from expobre", {
  out <- enemdu_build_ipm_components(.ipm_component_test_data(), strict = FALSE)
  extreme_component <- .ipm_component_name("ipm_i07_pobreza_extrema_ingresos")

  expect_equal(out[[extreme_component]], .ipm_component_test_data()$expobre)
})

test_that("IPM component builder aborts when expobre and other components are pending in strict mode", {
  data <- .ipm_component_test_data()
  data$expobre <- NULL

  expect_error(
    enemdu_build_ipm_components(data, strict = TRUE),
    class = "enemdu_error_pending_ipm_components"
  )
})

test_that("IPM component builder does not create score, flags, or aggregate columns", {
  out <- enemdu_build_ipm_components(.ipm_component_test_data(), strict = FALSE)

  expect_false(any(c("ipm_score", "tpm", "tpem", "A", "ipm") %in% names(out)))
})

test_that("IPM component builder returns pending components as NA when strict is false", {
  out <- enemdu_build_ipm_components(.ipm_component_test_data(), strict = FALSE)
  diagnostics <- attr(out, "ipm_component_diagnostics")

  pending_components <- diagnostics$components_pending

  expect_true(length(pending_components) > 0)
  expect_true(all(vapply(out[pending_components], function(x) all(is.na(x)), logical(1))))
  expect_true(all(vapply(out[pending_components], typeof, character(1)) == "integer"))
})

test_that("IPM component builder aborts in strict mode when not all components are available", {
  expect_error(
    enemdu_build_ipm_components(.ipm_component_test_data(), strict = TRUE),
    class = "enemdu_error_pending_ipm_components"
  )
})

test_that("IPM component builder accepts all 12 precomputed components", {
  out <- enemdu_build_ipm_components(.ipm_precomputed_component_data(), strict = TRUE)
  diagnostics <- attr(out, "ipm_component_diagnostics")

  expect_true(all(.ipm_component_names() %in% names(out)))
  expect_equal(diagnostics$components_pending, character())
  expect_setequal(diagnostics$components_precomputed, .ipm_component_names())
})

test_that("IPM component builder output can feed IPM flags when all components are available", {
  out <- enemdu_build_ipm_components(.ipm_precomputed_component_data(), strict = TRUE)
  flagged <- enemdu_build_ipm_flags(out)

  expect_true(all(c("ipm_score", "tpm", "tpem") %in% names(flagged)))
  expect_equal(flagged$ipm_score[1], 0)
  expect_equal(flagged$ipm_score[2], 0.125)
})

test_that("IPM component builder can prepare source data from household_data", {
  data <- .ipm_component_test_data()
  data$vi10 <- NULL
  data$vi07 <- NULL
  household <- .ipm_household_source_test_data()

  out <- enemdu_build_ipm_components(
    data = data,
    household_data = household,
    strict = FALSE
  )
  diagnostics <- attr(out, "ipm_component_diagnostics")
  water_component <- .ipm_component_name("ipm_i08_sin_agua_red_publica")

  expect_true(diagnostics$source_join_applied)
  expect_equal(out[[water_component]][out$id_hogar == "h2"], c(1L, 1L))
})

test_that("IPM component builder preserves unrelated NBI, poverty, and labor outputs", {
  data <- .ipm_component_test_data()
  out <- enemdu_build_ipm_components(data, strict = FALSE)

  expect_equal(out$nbi, data$nbi)
  expect_equal(out$pobre, data$pobre)
  expect_equal(out$labor_empleo, data$labor_empleo)
})
