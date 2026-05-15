.synthetic_poverty_kpi_data <- function(income = c(40, 75, 125, 150),
                                        area = NULL) {
  n <- length(income)

  if (is.null(area)) {
    area <- rep("total", n)
  }

  tibble::tibble(
    idhogar = paste0("h", seq_len(n)),
    hsize = rep(1L, n),
    upm = seq_len(n),
    estrato = rep(seq_len(ceiling(n / 2)), each = 2L)[seq_len(n)],
    fexp = rep(1, n),
    area = area,
    ingtot_pc = income
  )
}

.synthetic_external_poverty_lines <- function() {
  tibble::tibble(
    period = c("2025-12", "2025-12"),
    period_type = c("monthly", "monthly"),
    line_type = c("poverty", "extreme_poverty"),
    line_value = c(100, 50),
    currency = c("USD", "USD"),
    ipc_value = c(NA_real_, NA_real_),
    base_line_value = c(NA_real_, NA_real_),
    base_period = c("external_synthetic", "external_synthetic"),
    update_method = c("external_registry", "external_registry"),
    source_status = c("external_published", "external_published"),
    source_note = c(
      "Synthetic external registry for KPI tests; not official validation.",
      "Synthetic external registry for KPI tests; not official validation."
    ),
    valid_from = c("2025-12", "2025-12"),
    valid_to = c("2025-12", "2025-12"),
    notes = c(
      "Synthetic poverty-line row for KPI workflow tests.",
      "Synthetic extreme-poverty-line row for KPI workflow tests."
    )
  )
}

test_that("manual mode estimates poverty and extreme poverty", {
  data <- .synthetic_poverty_kpi_data()

  out <- enemdu_kpi_income_poverty(
    data = data,
    period = "2025-12",
    mode = "manual",
    poverty_line = 100,
    extreme_poverty_line = 50,
    line_source = "Synthetic manual lines for tests; not official validation.",
    survey_type = "anual",
    sample_n_min = 1
  )

  metadata_cols <- c(
    "poverty_line_period",
    "poverty_line_mode",
    "poverty_income_var",
    "poverty_flag_var",
    "poverty_line_type",
    "poverty_line_value",
    "poverty_line_currency",
    "poverty_line_source_status",
    "poverty_line_source_note",
    "poverty_line_update_method",
    "official_validation_status",
    "official_validation_note"
  )

  expect_s3_class(out, "enemdu_income_poverty_kpi")
  expect_true(all(c("pobreza_ingresos", "pobreza_extrema_ingresos") %in% out$indicator_id))
  expect_true(all(metadata_cols %in% names(out)))
  expect_equal(unique(out$official_validation_status), "not_officially_validated")
  expect_true(all(grepl("not official", out$poverty_line_source_note, fixed = TRUE)))

  policy <- attr(out, "poverty_kpi_policy")
  expect_true(is.list(policy))
  expect_equal(policy$mode, "manual")
  expect_match(policy$note, "Official validation requires comparison", fixed = TRUE)
})

test_that("manual mode estimates only valid non-missing poverty flags", {
  data <- .synthetic_poverty_kpi_data(
    income = c(0, -10, NA_real_, 25, 75, 125)
  )

  out <- enemdu_kpi_income_poverty(
    data = data,
    period = "2025-12",
    mode = "manual",
    poverty_line = 100,
    extreme_poverty_line = 50,
    line_source = "Synthetic manual lines for tests; not official validation.",
    survey_type = "anual",
    sample_n_min = 1
  )

  flagged <- enemdu_build_poverty_flags(
    data = data,
    period = "2025-12",
    mode = "manual",
    poverty_line = 100,
    extreme_poverty_line = 50,
    line_source = "Synthetic manual lines for tests; not official validation."
  )

  expect_equal(sum(!is.na(flagged$pobre)), 3)
  expect_equal(sum(!is.na(flagged$expobre)), 3)
  expect_true(all(out$unweighted_n == 3L))
  expect_equal(out$estimate[out$indicator_id == "pobreza_ingresos"], 2 / 3)
  expect_equal(out$estimate[out$indicator_id == "pobreza_extrema_ingresos"], 1 / 3)
})

test_that("strict mode works with an external valid registry", {
  data <- .synthetic_poverty_kpi_data()

  out <- enemdu_kpi_income_poverty(
    data = data,
    period = "2025-12",
    mode = "strict",
    poverty_lines = .synthetic_external_poverty_lines(),
    survey_type = "anual",
    sample_n_min = 1
  )

  expect_true(all(c("pobreza_ingresos", "pobreza_extrema_ingresos") %in% out$indicator_id))
  expect_equal(unique(out$poverty_line_source_status), "external_published")
  expect_equal(unique(out$poverty_line_mode), "strict")
})

test_that("default package registry fails in strict mode", {
  data <- .synthetic_poverty_kpi_data()

  expect_error(
    enemdu_kpi_income_poverty(
      data = data,
      period = "TEMPLATE",
      mode = "strict",
      survey_type = "anual",
      sample_n_min = 1
    ),
    class = "enemdu_error_invalid_poverty_line"
  )
})

test_that("grouped estimates work", {
  data <- .synthetic_poverty_kpi_data(
    income = c(40, 75, 125, 150),
    area = c("urban", "urban", "rural", "rural")
  )

  out <- enemdu_kpi_income_poverty(
    data = data,
    group_vars = "area",
    period = "2025-12",
    mode = "manual",
    poverty_line = 100,
    extreme_poverty_line = 50,
    line_source = "Synthetic manual lines for tests; not official validation.",
    survey_type = "anual",
    sample_n_min = 1
  )

  expect_true(all(c("urban", "rural") %in% out$area))
  expect_true(all(c("pobreza_ingresos", "pobreza_extrema_ingresos") %in% out$indicator_id))
  expect_equal(nrow(out), 4)
})

test_that("overwrite protection is inherited from poverty flag builder", {
  data <- .synthetic_poverty_kpi_data()
  data$pobre <- c(0L, 0L, 0L, 0L)
  data$expobre <- c(0L, 0L, 0L, 0L)

  expect_error(
    enemdu_kpi_income_poverty(
      data = data,
      period = "2025-12",
      mode = "manual",
      poverty_line = 100,
      extreme_poverty_line = 50,
      line_source = "Synthetic manual lines for tests; not official validation.",
      survey_type = "anual",
      sample_n_min = 1
    ),
    class = "enemdu_error_existing_poverty_output"
  )

  out <- enemdu_kpi_income_poverty(
    data = data,
    period = "2025-12",
    mode = "manual",
    poverty_line = 100,
    extreme_poverty_line = 50,
    line_source = "Synthetic manual lines for tests; not official validation.",
    overwrite = TRUE,
    survey_type = "anual",
    sample_n_min = 1
  )

  expect_true(all(c("pobreza_ingresos", "pobreza_extrema_ingresos") %in% out$indicator_id))
})

test_that("manual mode requires line source", {
  data <- .synthetic_poverty_kpi_data()

  expect_error(
    enemdu_kpi_income_poverty(
      data = data,
      period = "2025-12",
      mode = "manual",
      poverty_line = 100,
      extreme_poverty_line = 50,
      survey_type = "anual",
      sample_n_min = 1
    ),
    class = "enemdu_error_invalid_poverty_line"
  )
})

test_that("extreme poverty line cannot exceed poverty line", {
  data <- .synthetic_poverty_kpi_data()

  expect_error(
    enemdu_kpi_income_poverty(
      data = data,
      period = "2025-12",
      mode = "manual",
      poverty_line = 50,
      extreme_poverty_line = 100,
      line_source = "Synthetic manual lines for tests; not official validation.",
      survey_type = "anual",
      sample_n_min = 1
    ),
    class = "enemdu_error_invalid_poverty_line"
  )
})
