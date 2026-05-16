test_that("official NBI benchmark registry loads as a valid tibble", {
  benchmarks <- enemdu_official_nbi_benchmarks()

  required_cols <- c(
    "benchmark_set",
    "period",
    "indicator_id",
    "domain_type",
    "domain_value",
    "domain_label",
    "official_estimate",
    "official_scale",
    "source_file",
    "source_table",
    "source_note",
    "source_status"
  )

  expect_s3_class(benchmarks, "tbl_df")
  expect_true(all(required_cols %in% names(benchmarks)))
  expect_equal(nrow(benchmarks), 0L)
})

test_that("NBI comparison reports missing official benchmarks without validation claim", {
  estimates <- tibble::tibble(
    indicator_id = c("pobreza_nbi", "pobreza_extrema_nbi"),
    estimate = c(0.40, 0.20)
  )

  comparison <- enemdu_compare_official_nbi(estimates)

  expect_s3_class(comparison, "enemdu_official_nbi_comparison")
  expect_true(all(comparison$comparison_status == "missing_official_benchmark"))
  expect_equal(unique(comparison$official_validation_status), "not_officially_validated")
})

test_that("NBI comparison matches synthetic benchmark rows", {
  estimates <- tibble::tibble(
    indicator_id = c("pobreza_nbi", "pobreza_extrema_nbi"),
    estimate = c(0.40, 0.20)
  )

  benchmarks <- tibble::tibble(
    benchmark_set = "synthetic_nbi",
    period = "synthetic",
    indicator_id = c("pobreza_nbi", "pobreza_extrema_nbi"),
    domain_type = "national",
    domain_value = "national",
    domain_label = "National",
    official_estimate = c(0.40, 0.20),
    official_scale = "proportion",
    source_file = "synthetic",
    source_table = "synthetic",
    source_note = "Synthetic benchmark for tests only.",
    source_status = "synthetic_test"
  )

  comparison <- enemdu_compare_official_nbi(
    estimates = estimates,
    benchmarks = benchmarks,
    period = "synthetic",
    benchmark_set = "synthetic_nbi"
  )

  expect_true(all(comparison$comparison_status == "matched_reported_rounding"))
  expect_true(all(comparison$abs_difference_pp == 0))
  expect_equal(unique(comparison$official_validation_status), "not_officially_validated")
})

test_that("NBI comparison normalizes percent-scale official estimates", {
  estimates <- tibble::tibble(
    indicator_id = "pobreza_nbi",
    estimate = 0.40
  )

  benchmarks <- tibble::tibble(
    benchmark_set = "synthetic_nbi",
    period = "synthetic",
    indicator_id = "pobreza_nbi",
    domain_type = "national",
    domain_value = "national",
    domain_label = "National",
    official_estimate = 40,
    official_scale = "percent",
    source_file = "synthetic",
    source_table = "synthetic",
    source_note = "Synthetic benchmark for tests only.",
    source_status = "synthetic_test"
  )

  comparison <- enemdu_compare_official_nbi(
    estimates = estimates,
    benchmarks = benchmarks,
    period = "synthetic",
    benchmark_set = "synthetic_nbi"
  )

  expect_equal(comparison$official_estimate_proportion, 0.40)
  expect_equal(comparison$official_estimate_percent, 40)
  expect_equal(comparison$calculated_proportion, 0.40)
  expect_equal(comparison$calculated_percent, 40)
  expect_equal(comparison$difference, 0)
  expect_equal(comparison$difference_pp, 0)
})

test_that("NBI comparison normalizes proportion-scale official estimates", {
  estimates <- tibble::tibble(
    indicator_id = "pobreza_nbi",
    estimate = 0.40
  )

  benchmarks <- tibble::tibble(
    benchmark_set = "synthetic_nbi",
    period = "synthetic",
    indicator_id = "pobreza_nbi",
    domain_type = "national",
    domain_value = "national",
    domain_label = "National",
    official_estimate = 0.40,
    official_scale = "proportion",
    source_file = "synthetic",
    source_table = "synthetic",
    source_note = "Synthetic benchmark for tests only.",
    source_status = "synthetic_test"
  )

  comparison <- enemdu_compare_official_nbi(
    estimates = estimates,
    benchmarks = benchmarks,
    period = "synthetic",
    benchmark_set = "synthetic_nbi"
  )

  expect_equal(comparison$official_estimate_proportion, 0.40)
  expect_equal(comparison$official_estimate_percent, 40)
  expect_equal(comparison$difference, 0)
  expect_equal(comparison$difference_pp, 0)
})

test_that("NBI comparison rejects unknown official benchmark scales", {
  estimates <- tibble::tibble(
    indicator_id = "pobreza_nbi",
    estimate = 0.40
  )

  benchmarks <- tibble::tibble(
    benchmark_set = "synthetic_nbi",
    period = "synthetic",
    indicator_id = "pobreza_nbi",
    domain_type = "national",
    domain_value = "national",
    domain_label = "National",
    official_estimate = 0.40,
    official_scale = "points",
    source_file = "synthetic",
    source_table = "synthetic",
    source_note = "Synthetic benchmark for tests only.",
    source_status = "synthetic_test"
  )

  expect_error(
    enemdu_compare_official_nbi(
      estimates = estimates,
      benchmarks = benchmarks,
      period = "synthetic",
      benchmark_set = "synthetic_nbi"
    ),
    class = "enemdu_error_invalid_official_nbi_scale"
  )
})

test_that("NBI comparison rejects missing scale when official estimate is present", {
  estimates <- tibble::tibble(
    indicator_id = "pobreza_nbi",
    estimate = 0.40
  )

  benchmarks <- tibble::tibble(
    benchmark_set = "synthetic_nbi",
    period = "synthetic",
    indicator_id = "pobreza_nbi",
    domain_type = "national",
    domain_value = "national",
    domain_label = "National",
    official_estimate = 40,
    official_scale = NA_character_,
    source_file = "synthetic",
    source_table = "synthetic",
    source_note = "Synthetic benchmark for tests only.",
    source_status = "synthetic_test"
  )

  expect_error(
    enemdu_compare_official_nbi(
      estimates = estimates,
      benchmarks = benchmarks,
      period = "synthetic",
      benchmark_set = "synthetic_nbi"
    ),
    class = "enemdu_error_invalid_official_nbi_scale"
  )
})

test_that("NBI comparison does not reject missing scale when official estimate is missing", {
  estimates <- tibble::tibble(
    indicator_id = "pobreza_nbi",
    estimate = 0.40
  )

  benchmarks <- tibble::tibble(
    benchmark_set = "synthetic_nbi",
    period = "synthetic",
    indicator_id = "pobreza_nbi",
    domain_type = "national",
    domain_value = "national",
    domain_label = "National",
    official_estimate = NA_real_,
    official_scale = NA_character_,
    source_file = "synthetic",
    source_table = "synthetic",
    source_note = "Synthetic benchmark for tests only.",
    source_status = "synthetic_test"
  )

  comparison <- enemdu_compare_official_nbi(
    estimates = estimates,
    benchmarks = benchmarks,
    period = "synthetic",
    benchmark_set = "synthetic_nbi"
  )

  expect_equal(comparison$comparison_status, "missing_official_benchmark")
  expect_true(is.na(comparison$official_estimate_proportion))
  expect_true(is.na(comparison$official_estimate_percent))
  expect_true(is.na(comparison$difference))
  expect_true(is.na(comparison$difference_pp))
})

test_that("NBI comparison evaluates tolerance in percentage points", {
  estimates <- tibble::tibble(
    indicator_id = "pobreza_nbi",
    estimate = 0.406
  )

  benchmarks <- tibble::tibble(
    benchmark_set = "synthetic_nbi",
    period = "synthetic",
    indicator_id = "pobreza_nbi",
    domain_type = "national",
    domain_value = "national",
    domain_label = "National",
    official_estimate = 40,
    official_scale = "percent",
    source_file = "synthetic",
    source_table = "synthetic",
    source_note = "Synthetic benchmark for tests only.",
    source_status = "synthetic_test"
  )

  comparison <- enemdu_compare_official_nbi(
    estimates = estimates,
    benchmarks = benchmarks,
    period = "synthetic",
    benchmark_set = "synthetic_nbi",
    tolerance_pp = 0.50
  )

  expect_equal(comparison$difference, 0.006, tolerance = 1e-10)
  expect_equal(comparison$difference_pp, 0.6, tolerance = 1e-10)
  expect_equal(comparison$comparison_status, "outside_tolerance")
})

test_that("NBI comparison supports domain values and tolerance status", {
  estimates <- tibble::tibble(
    indicator_id = c("pobreza_nbi", "pobreza_nbi"),
    area = c("1", "2"),
    estimate = c(0.401, 0.60)
  )

  benchmarks <- tibble::tibble(
    benchmark_set = "synthetic_nbi",
    period = "synthetic",
    indicator_id = c("pobreza_nbi", "pobreza_nbi"),
    domain_type = "area",
    domain_value = c("urban", "rural"),
    domain_label = c("Urban", "Rural"),
    official_estimate = c(0.40, 0.45),
    official_scale = "proportion",
    source_file = "synthetic",
    source_table = "synthetic",
    source_note = "Synthetic benchmark for tests only.",
    source_status = "synthetic_test"
  )

  comparison <- enemdu_compare_official_nbi(
    estimates = estimates,
    benchmarks = benchmarks,
    period = "synthetic",
    benchmark_set = "synthetic_nbi",
    domain_vars = "area",
    domain_map = c("1" = "urban", "2" = "rural"),
    tolerance_pp = 0.20
  )

  urban <- comparison[comparison$domain_value == "urban", , drop = FALSE]
  rural <- comparison[comparison$domain_value == "rural", , drop = FALSE]

  expect_equal(urban$comparison_status, "benchmark_comparison_within_tolerance")
  expect_equal(rural$comparison_status, "outside_tolerance")
  expect_error(
    enemdu_compare_official_nbi(
      estimates = estimates,
      benchmarks = benchmarks,
      period = "synthetic",
      benchmark_set = "synthetic_nbi",
      domain_vars = "area",
      domain_map = c("1" = "urban", "2" = "rural"),
      tolerance_pp = 0.20,
      strict = TRUE
    ),
    class = "enemdu_error_official_nbi_comparison_mismatch"
  )
})
