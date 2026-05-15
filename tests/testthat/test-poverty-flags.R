test_that("manual poverty flags are built with explicit source", {
  data <- tibble::tibble(
    ingtot_pc = c(30, 60, 120, NA_real_, 0)
  )

  out <- enemdu_build_poverty_flags(
    data = data,
    period = "2024-12",
    mode = "manual",
    poverty_line = 100,
    extreme_poverty_line = 50,
    line_source = "Test source for unit testing"
  )

  expect_true("pobre" %in% names(out))
  expect_true("expobre" %in% names(out))
  expect_true("linea_pobreza" %in% names(out))
  expect_true("linea_pobreza_extrema" %in% names(out))

  expect_equal(out$pobre, c(1L, 1L, 0L, NA_integer_, NA_integer_))
  expect_equal(out$expobre, c(1L, 0L, 0L, NA_integer_, NA_integer_))
})

test_that("manual poverty flags leave non-positive income unclassified", {
  data <- tibble::tibble(
    ingtot_pc = c(NA_real_, 0, -10, 25, 75, 125)
  )

  out <- enemdu_build_poverty_flags(
    data = data,
    period = "2024-12",
    mode = "manual",
    poverty_line = 100,
    extreme_poverty_line = 50,
    line_source = "Synthetic contract-test source"
  )

  expect_equal(out$pobre, c(NA_integer_, NA_integer_, NA_integer_, 1L, 1L, 0L))
  expect_equal(out$expobre, c(NA_integer_, NA_integer_, NA_integer_, 1L, 0L, 0L))

  policy <- attr(out, "poverty_flag_policy")
  expect_true(is.list(policy))
  expect_match(policy$valid_income_rule, "greater than zero", fixed = TRUE)
})

test_that("scenario poverty flags can use custom output names without replacing base flags", {
  data <- tibble::tibble(
    ingtot_pc_plus_optional_bonos = c(40, 75, 125),
    pobre = c(9L, 9L, 9L),
    expobre = c(8L, 8L, 8L)
  )

  out <- enemdu_build_poverty_flags(
    data = data,
    period = "2024-12",
    income_var = "ingtot_pc_plus_optional_bonos",
    mode = "manual",
    poverty_line = 100,
    extreme_poverty_line = 50,
    line_source = "Synthetic scenario contract-test source",
    poverty_var = "pobreza_ingresos_plus_optional_bonos",
    extreme_poverty_var = "pobreza_extrema_ingresos_plus_optional_bonos"
  )

  expect_equal(out$pobre, data$pobre)
  expect_equal(out$expobre, data$expobre)

  expect_equal(out$pobreza_ingresos_plus_optional_bonos, c(1L, 1L, 0L))
  expect_equal(out$pobreza_extrema_ingresos_plus_optional_bonos, c(1L, 0L, 0L))

  policy <- attr(out, "poverty_flag_policy")
  expect_equal(policy$income_var, "ingtot_pc_plus_optional_bonos")
  expect_equal(policy$poverty_var, "pobreza_ingresos_plus_optional_bonos")
  expect_equal(policy$extreme_poverty_var, "pobreza_extrema_ingresos_plus_optional_bonos")
})

test_that("poverty flag outputs are protected by default", {
  data <- tibble::tibble(
    ingtot_pc = c(40, 75, 125),
    pobre = c(0L, 0L, 0L)
  )

  expect_error(
    enemdu_build_poverty_flags(
      data = data,
      period = "2024-12",
      mode = "manual",
      poverty_line = 100,
      extreme_poverty_line = 50,
      line_source = "Synthetic contract-test source",
      overwrite = FALSE
    ),
    class = "enemdu_error_existing_poverty_output"
  )
})

test_that("manual mode requires a source", {
  data <- tibble::tibble(
    ingtot_pc = c(30, 60, 120)
  )

  expect_error(
    enemdu_build_poverty_flags(
      data = data,
      period = "2024-12",
      mode = "manual",
      poverty_line = 100,
      extreme_poverty_line = 50
    ),
    class = "enemdu_error_invalid_poverty_line"
  )
})

test_that("strict poverty flags are built from a valid registry", {
  registry <- tibble::tibble(
    period = c("2024-12", "2024-12"),
    period_type = c("monthly", "monthly"),
    line_type = c("poverty", "extreme_poverty"),
    line_value = c(100, 50),
    currency = c("USD", "USD"),
    ipc_value = c(NA_real_, NA_real_),
    base_line_value = c(NA_real_, 31.92),
    base_period = c("ECV_2006", "ECV_2006"),
    update_method = c("official_line", "official_line"),
    source_status = c("official", "official"),
    source_note = c("Test official source", "Test official source"),
    valid_from = c("2024-12", "2024-12"),
    valid_to = c("2024-12", "2024-12"),
    notes = c("Test row", "Test row")
  )

  data <- tibble::tibble(
    ingtot_pc = c(30, 60, 120)
  )

  out <- enemdu_build_poverty_flags(
    data = data,
    period = "2024-12",
    mode = "strict",
    poverty_lines = registry
  )

  expect_equal(out$pobre, c(1L, 1L, 0L))
  expect_equal(out$expobre, c(1L, 0L, 0L))

  metadata <- attr(out, "poverty_line_metadata")
  expect_true(is.data.frame(metadata))
  expect_equal(nrow(metadata), 2)
})

test_that("strict poverty flags can use an external registry with provenance", {
  registry <- tibble::tibble(
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
      "Synthetic external registry for workflow tests; not official validation.",
      "Synthetic external registry for workflow tests; not official validation."
    ),
    valid_from = c("2025-12", "2025-12"),
    valid_to = c("2025-12", "2025-12"),
    notes = c(
      "Synthetic poverty-line row for external registry workflow tests.",
      "Synthetic extreme-poverty-line row for external registry workflow tests."
    )
  )

  data <- tibble::tibble(
    ingtot_pc = c(NA_real_, 0, -5, 40, 75, 125)
  )

  out <- enemdu_build_poverty_flags(
    data = data,
    period = "2025-12",
    mode = "strict",
    poverty_lines = registry
  )

  expect_equal(out$pobre, c(NA_integer_, NA_integer_, NA_integer_, 1L, 1L, 0L))
  expect_equal(out$expobre, c(NA_integer_, NA_integer_, NA_integer_, 1L, 0L, 0L))
  expect_equal(unique(out$linea_pobreza), 100)
  expect_equal(unique(out$linea_pobreza_extrema), 50)

  metadata <- attr(out, "poverty_line_metadata")
  input_report <- attr(out, "poverty_input_report")
  policy <- attr(out, "poverty_flag_policy")

  expect_true(is.data.frame(metadata))
  expect_equal(nrow(metadata), 2)
  expect_equal(unique(metadata$source_status), "external_published")
  expect_true(all(grepl("not official validation", metadata$source_note, fixed = TRUE)))
  expect_true(is.data.frame(input_report))
  expect_equal(
    input_report$status[input_report$component == "income_positive"],
    3
  )
  expect_equal(policy$mode, "strict")
  expect_match(policy$valid_income_rule, "greater than zero", fixed = TRUE)
})

test_that("strict mode fails with default template registry", {
  data <- tibble::tibble(
    ingtot_pc = c(30, 60, 120)
  )

  expect_error(
    enemdu_build_poverty_flags(
      data = data,
      period = "2024-12",
      mode = "strict"
    ),
    class = "enemdu_error_missing_poverty_line"
  )
})

test_that("income derivation can feed explicit manual poverty flags without official validation", {
  data <- tibble::tibble(
    idhogar = c("h1", "h2", "h3"),
    p63 = c(40, 120, NA_real_),
    p64b = c(NA_real_, NA_real_, NA_real_),
    p65 = c(NA_real_, NA_real_, NA_real_),
    p66 = c(NA_real_, NA_real_, NA_real_),
    p67 = c(NA_real_, NA_real_, NA_real_),
    p68b = c(NA_real_, NA_real_, NA_real_),
    p69 = c(NA_real_, NA_real_, NA_real_),
    p70b = c(NA_real_, NA_real_, NA_real_),
    p71a = c(2, 2, 2),
    p71b = c(NA_real_, NA_real_, NA_real_),
    p72a = c(2, 2, 2),
    p72b = c(NA_real_, NA_real_, NA_real_),
    p73a = c(2, 2, 2),
    p73b = c(NA_real_, NA_real_, NA_real_),
    p74a = c(2, 2, 2),
    p74b = c(NA_real_, NA_real_, NA_real_),
    p75 = c(2, 2, 2),
    p76 = c(NA_real_, NA_real_, NA_real_),
    p78 = c(NA_real_, NA_real_, NA_real_)
  )

  built <- enemdu_build_variables(data)

  out <- enemdu_build_poverty_flags(
    data = built,
    period = "2024-12",
    mode = "manual",
    poverty_line = 100,
    extreme_poverty_line = 50,
    line_source = "Synthetic contract-test line source"
  )

  expect_equal(out$pobre, c(1L, 0L, NA_integer_))
  expect_equal(out$expobre, c(1L, 0L, NA_integer_))

  line_metadata <- attr(out, "poverty_line_metadata")
  input_report <- attr(out, "poverty_input_report")

  expect_true(is.data.frame(line_metadata))
  expect_true(is.data.frame(input_report))
  expect_equal(unique(line_metadata$source_status), "manual")
  expect_equal(unique(line_metadata$source_note), "Synthetic contract-test line source")
  expect_true("line_source_declared" %in% input_report$component)
})

test_that("extreme poverty line cannot exceed poverty line", {
  data <- tibble::tibble(
    ingtot_pc = c(30, 60, 120)
  )

  expect_error(
    enemdu_build_poverty_flags(
      data = data,
      period = "2024-12",
      mode = "manual",
      poverty_line = 50,
      extreme_poverty_line = 100,
      line_source = "Test source"
    ),
    class = "enemdu_error_invalid_poverty_line"
  )
})

test_that("diagnostic mode returns data with diagnostic attribute", {
  data <- tibble::tibble(
    ingtot_pc = c(30, 60, 120, NA_real_)
  )

  out <- enemdu_build_poverty_flags(
    data = data,
    mode = "diagnostic_only"
  )

  diagnostic <- attr(out, "poverty_input_report")

  expect_true(is.data.frame(diagnostic))
  expect_true("component" %in% names(diagnostic))
  expect_false("pobre" %in% names(out))
  expect_false("expobre" %in% names(out))
})
