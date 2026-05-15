test_that("official poverty benchmark registry loads with expected rows", {
  benchmarks <- enemdu_official_poverty_benchmarks()

  required_cols <- c(
    "period",
    "survey_type",
    "benchmark_set",
    "indicator_id",
    "domain_type",
    "domain_value",
    "official_estimate",
    "official_percent",
    "poverty_line_value",
    "extreme_poverty_line_value",
    "currency",
    "source_institution",
    "source_title",
    "source_period",
    "source_page",
    "source_note",
    "official_validation_status"
  )

  expect_s3_class(benchmarks, "tbl_df")
  expect_true(all(required_cols %in% names(benchmarks)))
  expect_true(any(benchmarks$period == "2025-12"))
  expect_true(all(c("pobreza_ingresos", "pobreza_extrema_ingresos") %in% benchmarks$indicator_id))
  expect_true(all(c("national", "urban", "rural") %in% benchmarks$domain_value))
})

test_that("national comparison matches reported rounding", {
  estimates <- tibble::tibble(
    indicator_id = c("pobreza_ingresos", "pobreza_extrema_ingresos"),
    estimate = c(0.214, 0.083)
  )

  comparison <- enemdu_compare_official_poverty(
    estimates,
    period = "2025-12",
    benchmark_set = "income_poverty_december_2025"
  )

  expect_true(all(comparison$comparison_status == "matched_reported_rounding"))
  expect_true(all(comparison$abs_difference_pp == 0))
  expect_equal(
    unique(comparison$comparison_validation_status),
    "comparison_only_not_official_validation"
  )
})

test_that("domain comparison works with character domains", {
  estimates <- tibble::tibble(
    indicator_id = c("pobreza_ingresos", "pobreza_ingresos"),
    area = c("urban", "rural"),
    estimate = c(0.138, 0.376)
  )

  comparison <- enemdu_compare_official_poverty(
    estimates,
    period = "2025-12",
    benchmark_set = "income_poverty_december_2025",
    domain_vars = "area"
  )

  poverty_rows <- comparison[comparison$indicator_id == "pobreza_ingresos", , drop = FALSE]

  expect_true(all(c("urban", "rural") %in% poverty_rows$domain_value))
  expect_true(all(poverty_rows$comparison_status == "matched_reported_rounding"))
})

test_that("domain comparison works with domain map", {
  estimates <- tibble::tibble(
    indicator_id = c("pobreza_ingresos", "pobreza_ingresos"),
    area = c("1", "2"),
    estimate = c(0.138, 0.376)
  )

  comparison <- enemdu_compare_official_poverty(
    estimates,
    period = "2025-12",
    benchmark_set = "income_poverty_december_2025",
    domain_vars = "area",
    domain_map = c("1" = "urban", "2" = "rural")
  )

  poverty_rows <- comparison[comparison$indicator_id == "pobreza_ingresos", , drop = FALSE]

  expect_true(all(c("urban", "rural") %in% poverty_rows$domain_value))
  expect_true(all(poverty_rows$comparison_status == "matched_reported_rounding"))
})

test_that("official poverty comparison respects tolerance below reported rounding", {
  benchmarks <- enemdu_official_poverty_benchmarks(
    period = "2025-12",
    benchmark_set = "income_poverty_december_2025",
    indicator_id = "pobreza_ingresos",
    domain_type = "national",
    domain_value = "national"
  )

  estimates <- tibble::tibble(
    indicator_id = "pobreza_ingresos",
    estimate = 0.2143
  )

  comparison <- enemdu_compare_official_poverty(
    estimates = estimates,
    benchmarks = benchmarks,
    tolerance_pp = 0.01
  )

  expect_equal(comparison$comparison_status, "outside_tolerance")
  expect_equal(comparison$abs_difference_pp, 0.03, tolerance = 1e-10)

  expect_error(
    enemdu_compare_official_poverty(
      estimates = estimates,
      benchmarks = benchmarks,
      tolerance_pp = 0.01,
      strict = TRUE
    ),
    class = "enemdu_error_official_poverty_comparison_mismatch"
  )
})

test_that("official poverty comparison keeps reported rounding when tolerance allows it", {
  benchmarks <- enemdu_official_poverty_benchmarks(
    period = "2025-12",
    benchmark_set = "income_poverty_december_2025",
    indicator_id = "pobreza_ingresos",
    domain_type = "national",
    domain_value = "national"
  )

  estimates <- tibble::tibble(
    indicator_id = "pobreza_ingresos",
    estimate = 0.2143
  )

  comparison <- enemdu_compare_official_poverty(
    estimates = estimates,
    benchmarks = benchmarks,
    tolerance_pp = 0.10
  )

  expect_equal(comparison$comparison_status, "matched_reported_rounding")
  expect_equal(comparison$abs_difference_pp, 0.03, tolerance = 1e-10)
})

test_that("missing benchmark is detected", {
  estimates <- tibble::tibble(
    indicator_id = "nonexistent_indicator",
    estimate = 0.100
  )

  comparison <- enemdu_compare_official_poverty(
    estimates,
    period = "2025-12",
    benchmark_set = "income_poverty_december_2025"
  )

  missing_row <- comparison[comparison$indicator_id == "nonexistent_indicator", , drop = FALSE]

  expect_equal(missing_row$comparison_status, "missing_official_benchmark")
})

test_that("missing package estimates are retained for benchmark rows", {
  estimates <- tibble::tibble(
    indicator_id = "pobreza_ingresos",
    estimate = 0.214
  )

  comparison <- enemdu_compare_official_poverty(
    estimates,
    period = "2025-12",
    benchmark_set = "income_poverty_december_2025"
  )

  missing_row <- comparison[
    comparison$indicator_id == "pobreza_extrema_ingresos" &
      comparison$domain_type == "national",
    ,
    drop = FALSE
  ]

  expect_equal(missing_row$comparison_status, "missing_package_estimate")
})

test_that("poverty comparison returns class and policy attribute", {
  estimates <- tibble::tibble(
    indicator_id = c("pobreza_ingresos", "pobreza_extrema_ingresos"),
    estimate = c(0.214, 0.083)
  )

  comparison <- enemdu_compare_official_poverty(
    estimates,
    period = "2025-12",
    benchmark_set = "income_poverty_december_2025"
  )

  expect_s3_class(comparison, "enemdu_official_poverty_comparison")
  expect_false(is.null(attr(comparison, "comparison_policy")))
  expect_match(
    attr(comparison, "comparison_policy")$note,
    "does not imply official validation",
    fixed = TRUE
  )
})
