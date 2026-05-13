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
