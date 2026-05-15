external_auditable_poverty_registry <- function() {
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
}

test_that("poverty line registry loads with expected columns", {
  registry <- enemdu_poverty_line_registry()

  expect_true(nrow(registry) >= 2)

  expect_true(all(
    c(
      "period",
      "period_type",
      "line_type",
      "line_value",
      "currency",
      "ipc_value",
      "base_line_value",
      "base_period",
      "update_method",
      "source_status",
      "source_note",
      "valid_from",
      "valid_to",
      "notes"
    ) %in% names(registry)
  ))
})

test_that("default template registry is not valid for strict calculation", {
  registry <- enemdu_poverty_line_registry()

  validation <- enemdu_validate_poverty_lines(
    registry = registry,
    require_valid = TRUE
  )

  expect_s3_class(validation, "enemdu_poverty_line_validation")
  expect_true(any(validation$status == "fail"))
  expect_true("message" %in% names(validation))
  expect_true(any(grepl(
    "line_value|source_status|source_note",
    validation$message[validation$status == "fail"]
  )))
})

test_that("external poverty-line registry validates and resolves both lines", {
  registry <- external_auditable_poverty_registry()

  validation <- enemdu_validate_poverty_lines(
    registry = registry,
    require_valid = TRUE
  )

  poverty_line <- enemdu_get_poverty_line(
    period = "2025-12",
    line_type = "poverty",
    registry = registry,
    mode = "strict"
  )

  extreme_poverty_line <- enemdu_get_poverty_line(
    period = "2025-12",
    line_type = "extreme_poverty",
    registry = registry,
    mode = "strict"
  )

  expect_s3_class(validation, "enemdu_poverty_line_validation")
  expect_false(any(validation$status == "fail"))

  expect_s3_class(poverty_line, "enemdu_poverty_line")
  expect_s3_class(extreme_poverty_line, "enemdu_poverty_line")
  expect_equal(poverty_line$line_value, 100)
  expect_equal(extreme_poverty_line$line_value, 50)
  expect_equal(poverty_line$source_status, "external_published")
  expect_equal(extreme_poverty_line$source_status, "external_published")
  expect_match(
    poverty_line$source_note,
    "not official validation",
    fixed = TRUE
  )
})

test_that("poverty line can be resolved from a valid custom registry", {
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

  line <- enemdu_get_poverty_line(
    period = "2024-12",
    line_type = "poverty",
    registry = registry
  )

  expect_s3_class(line, "enemdu_poverty_line")
  expect_equal(line$line_value, 100)
})

test_that("pending poverty line fails in strict mode", {
  registry <- tibble::tibble(
    period = "2024-12",
    period_type = "monthly",
    line_type = "poverty",
    line_value = 100,
    currency = "USD",
    ipc_value = NA_real_,
    base_line_value = NA_real_,
    base_period = "ECV_2006",
    update_method = "official_line",
    source_status = "pending_review",
    source_note = "Pending source",
    valid_from = "2024-12",
    valid_to = "2024-12",
    notes = "Pending row"
  )

  expect_error(
    enemdu_get_poverty_line(
      period = "2024-12",
      line_type = "poverty",
      registry = registry,
      mode = "strict"
    ),
    class = "enemdu_error_invalid_poverty_line"
  )
})

test_that("missing poverty line fails clearly", {
  registry <- tibble::tibble(
    period = "2024-12",
    period_type = "monthly",
    line_type = "poverty",
    line_value = 100,
    currency = "USD",
    ipc_value = NA_real_,
    base_line_value = NA_real_,
    base_period = "ECV_2006",
    update_method = "official_line",
    source_status = "official",
    source_note = "Test official source",
    valid_from = "2024-12",
    valid_to = "2024-12",
    notes = "Test row"
  )

  expect_error(
    enemdu_get_poverty_line(
      period = "2024-12",
      line_type = "extreme_poverty",
      registry = registry
    ),
    class = "enemdu_error_missing_poverty_line"
  )
})
