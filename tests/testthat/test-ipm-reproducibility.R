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

.ipm_repro_operational_source_data <- function() {
  tibble::tibble(
    id_hogar = c("h1", "h1", "h2", "h2", "h3", "h3", "h4", "h4"),
    p01 = rep(c(1, 2), 4),
    upm = seq_len(8),
    estrato = rep(seq_len(4), each = 2),
    fexp = rep(1, 8),
    area = c(1, 1, 1, 1, 2, 2, 2, 2),
    p03 = c(10, 25, 16, 35, 40, 70, 12, 45),
    p07 = c(1, 2, 1, 2, 2, 2, 2, 2),
    p09 = c(1, 3, 1, 1, 1, 1, 3, 1),
    p10a = c(5, 7, 6, 4, 6, 6, 5, 7),
    p10b = c(5, 3, 4, 6, 6, 6, 7, 3),
    empleo = c(0, 1, 1, 1, 1, 0, 0, 1),
    desempleo = c(0, 0, 0, 1, 0, 0, 0, 0),
    p24 = c(NA, 20, 31, 20, 40, NA, NA, 20),
    ingrl = c(NA, 600, 600, 300, 600, NA, NA, 600),
    p61b1 = c(6, 1, 1, 5, 1, 6, 6, 1),
    p72a = c(2, 2, 2, 2, 2, 2, 2, 2),
    p75 = c(2, 2, 2, 2, 2, 1, 2, 2),
    p77 = c(2, 2, 2, 2, 2, 2, 2, 2),
    labor_desempleo = c(0, 0, 0, 1, 0, 0, 0, 0),
    labor_subempleo = 0L,
    labor_otro_empleo_no_pleno = 0L,
    labor_empleo_no_remunerado = 0L,
    labor_empleo_no_clasificado = 0L,
    vi03a = 1,
    vi03b = c(1, 1, 3, 3, 1, 1, 1, 1),
    vi04a = 1,
    vi04b = c(1, 1, 1, 1, 1, 1, 1, 1),
    vi05a = 1,
    vi05b = 1,
    vi10 = c(1, 1, 2, 2, 1, 1, 1, 1),
    vi07 = c(2, 2, 1, 1, 0, 0, 1, 1),
    vi09 = c(1, 1, 2, 2, 2, 2, 3, 3),
    vi13 = c(2, 2, 3, 3, 1, 1, 4, 4),
    epobreza = c(0, 0, 1, 1, 0, 0, 0, 0)
  )
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

test_that("IPM reproducibility workflow honors custom score and flag names", {
  data <- .ipm_repro_flag_data()
  data$custom_score <- data$ipm_score
  data$custom_tpm <- data$tpm
  data$custom_tpem <- data$tpem
  data$ipm_score <- NULL
  data$tpm <- NULL
  data$tpem <- NULL

  result <- enemdu_run_ipm_reproducibility(
    data,
    build_flags = FALSE,
    strict = TRUE,
    sample_n_min = 1,
    score_var = "custom_score",
    tpm_var = "custom_tpm",
    tpem_var = "custom_tpem"
  )

  expect_true(all(c(
    "custom_score",
    "custom_tpm",
    "custom_tpem"
  ) %in% result$preflight$variable))

  policy <- attr(result, "reproducibility_policy")
  expect_equal(policy$score_var, "custom_score")
  expect_equal(policy$tpm_var, "custom_tpm")
  expect_equal(policy$tpem_var, "custom_tpem")

  expect_true(all(c("tpm", "tpem", "A", "ipm") %in% result$estimates$indicator_id))
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

test_that("IPM reproducibility default missing component policy errors on incomplete evidence", {
  flag_data <- .ipm_repro_flag_data()
  flag_data$ipm_score[2] <- NA_real_

  expect_error(
    enemdu_run_ipm_reproducibility(
      flag_data,
      build_flags = FALSE,
      strict = TRUE,
      sample_n_min = 1
    ),
    class = "enemdu_error_ipm_reproducibility_incomplete_cases"
  )

  component_data <- .ipm_repro_component_data()
  component_data[[.ipm_repro_component_names()[[1]]]][2] <- NA_integer_

  expect_error(
    enemdu_run_ipm_reproducibility(
      component_data,
      build_flags = TRUE,
      strict = TRUE,
      sample_n_min = 1
    ),
    class = "enemdu_error_ipm_reproducibility_incomplete_cases"
  )
})

test_that("IPM reproducibility complete-case policy filters incomplete flags", {
  data <- .ipm_repro_flag_data()
  data$fexp <- c(1, 2, 3, 4, 5, 6, 7, 8)
  data$ipm_score[2] <- NA_real_
  data$tpm[2] <- NA_integer_
  data$tpem[2] <- NA_integer_

  result <- enemdu_run_ipm_reproducibility(
    data,
    build_flags = FALSE,
    strict = TRUE,
    sample_n_min = 1,
    missing_component_policy = "complete_case"
  )

  diagnostics <- result$complete_case_diagnostics

  expect_true(all(c("tpm", "tpem", "A", "ipm") %in% result$estimates$indicator_id))
  expect_equal(diagnostics$rows_total, 8L)
  expect_equal(diagnostics$rows_complete, 7L)
  expect_equal(diagnostics$rows_excluded, 1L)
  expect_equal(diagnostics$weighted_total, 36)
  expect_equal(diagnostics$weighted_complete, 34)
  expect_equal(diagnostics$weighted_excluded, 2)
  expect_equal(diagnostics$share_rows_excluded, 1 / 8)
  expect_equal(diagnostics$share_weighted_excluded, 2 / 36)
  expect_equal(diagnostics$missing_component_policy, "complete_case")
  expect_equal(diagnostics$complete_case_source, "flags")
})

test_that("IPM reproducibility complete-case policy filters incomplete components before flags", {
  component_names <- .ipm_repro_component_names()
  data <- .ipm_repro_component_data()
  data$fexp <- c(1, 2, 3, 4, 5, 6, 7, 8)
  data[[component_names[[1]]]][2] <- NA_integer_

  result <- enemdu_run_ipm_reproducibility(
    data,
    build_flags = TRUE,
    strict = TRUE,
    sample_n_min = 1,
    missing_component_policy = "complete_case"
  )

  diagnostics <- result$complete_case_diagnostics

  expect_true(all(c("tpm", "tpem", "A", "ipm") %in% result$estimates$indicator_id))
  expect_equal(diagnostics$rows_total, 8L)
  expect_equal(diagnostics$rows_complete, 7L)
  expect_equal(diagnostics$rows_excluded, 1L)
  expect_equal(diagnostics$weighted_excluded, 2)
  expect_equal(diagnostics$complete_case_source, "components")
  expect_equal(
    result$complete_case_diagnostics$official_validation_status,
    "not_officially_validated"
  )
})

test_that("IPM reproducibility complete-case diagnostics include domain exclusions", {
  component_names <- .ipm_repro_component_names()
  data <- .ipm_repro_component_data()
  data$fexp <- c(1, 2, 3, 4, 5, 6, 7, 8)
  data[[component_names[[1]]]][2] <- NA_integer_
  data[[component_names[[2]]]][6] <- NA_integer_

  result <- enemdu_run_ipm_reproducibility(
    data,
    build_flags = TRUE,
    strict = TRUE,
    sample_n_min = 1,
    missing_component_policy = "complete_case"
  )

  by_domain <- result$complete_case_by_domain
  area_1_incomplete <- by_domain[
    by_domain$domain_value == "1" &
      by_domain$complete_case_status == "incomplete",
    ,
    drop = FALSE
  ]
  area_2_incomplete <- by_domain[
    by_domain$domain_value == "2" &
      by_domain$complete_case_status == "incomplete",
    ,
    drop = FALSE
  ]

  expect_true(all(c("complete", "incomplete") %in% by_domain$complete_case_status))
  expect_equal(unique(by_domain$domain_variable), "area")
  expect_equal(area_1_incomplete$n, 1L)
  expect_equal(area_1_incomplete$weighted_n, 2)
  expect_equal(area_2_incomplete$n, 1L)
  expect_equal(area_2_incomplete$weighted_n, 6)
})

test_that("IPM reproducibility complete-case policy does not impute missing components", {
  component_names <- .ipm_repro_component_names()
  data <- .ipm_repro_component_data()
  data[[component_names[[1]]]][2] <- NA_integer_

  result <- enemdu_run_ipm_reproducibility(
    data,
    build_flags = TRUE,
    strict = TRUE,
    sample_n_min = 1,
    missing_component_policy = "complete_case"
  )

  expect_true(is.na(data[[component_names[[1]]]][2]))
  expect_equal(result$complete_case_diagnostics$rows_excluded, 1L)
  expect_equal(
    result$complete_case_diagnostics$official_validation_status,
    "not_officially_validated"
  )
  expect_equal(result$official_validation_status, "not_officially_validated")
})

test_that("IPM reproducibility complete-case policy aborts when all rows are incomplete", {
  component_names <- .ipm_repro_component_names()
  data <- .ipm_repro_component_data()
  data[[component_names[[1]]]] <- NA_integer_

  expect_error(
    enemdu_run_ipm_reproducibility(
      data,
      build_flags = TRUE,
      strict = TRUE,
      sample_n_min = 1,
      missing_component_policy = "complete_case"
    ),
    class = "enemdu_error_ipm_reproducibility_complete_case_empty"
  )
})

test_that("IPM reproducibility workflow builds custom score and flag names from components", {
  result <- enemdu_run_ipm_reproducibility(
    .ipm_repro_component_data(),
    build_flags = TRUE,
    strict = TRUE,
    sample_n_min = 1,
    score_var = "custom_score",
    tpm_var = "custom_tpm",
    tpem_var = "custom_tpem"
  )

  expect_true(all(c(
    "custom_score",
    "custom_tpm",
    "custom_tpem"
  ) %in% result$preflight$variable))

  policy <- attr(result, "reproducibility_policy")
  expect_equal(policy$score_var, "custom_score")
  expect_equal(policy$tpm_var, "custom_tpm")
  expect_equal(policy$tpem_var, "custom_tpem")
  expect_equal(policy$flags_diagnostics$score_var, "custom_score")
  expect_equal(policy$flags_diagnostics$tpm_var, "custom_tpm")
  expect_equal(policy$flags_diagnostics$tpem_var, "custom_tpem")
})

test_that("IPM reproducibility workflow can build flags from operational sources", {
  result <- enemdu_run_ipm_reproducibility(
    .ipm_repro_operational_source_data(),
    build_components = TRUE,
    build_flags = TRUE,
    strict = TRUE,
    sample_n_min = 1,
    higher_education_economic_reason_codes = 3
  )
  policy <- attr(result, "reproducibility_policy")

  expect_equal(policy$components_diagnostics$components_pending, character())
  expect_true(all(c("tpm", "tpem", "A", "ipm") %in% result$estimates$indicator_id))
  expect_true(isTRUE(policy$build_components))
  expect_true(isTRUE(policy$build_flags))
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
