.ipm_benchmarks_repo_path <- function(...) {
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

.ipm_benchmarks_extdata_path <- function(file) {
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

if (!exists("enemdu_official_ipm_benchmarks")) {
  source(.ipm_benchmarks_repo_path("R", "utils-errors.R"), local = TRUE)
  source(.ipm_benchmarks_repo_path("R", "utils-metadata.R"), local = TRUE)
  .enemdu_extdata_path <- function(file) .ipm_benchmarks_extdata_path(file)
  source(.ipm_benchmarks_repo_path("R", "ipm_kpis.R"), local = TRUE)
  source(.ipm_benchmarks_repo_path("R", "ipm_benchmarks.R"), local = TRUE)
}

test_that("IPM benchmark functions are exported when package namespace is available", {
  if (!"enemduR" %in% loadedNamespaces()) {
    testthat::skip("Export checks require the package namespace.")
  }

  expect_true("enemdu_official_ipm_benchmarks" %in% getNamespaceExports("enemduR"))
  expect_true("enemdu_compare_official_ipm" %in% getNamespaceExports("enemduR"))
  expect_true(is.function(getExportedValue("enemduR", "enemdu_official_ipm_benchmarks")))
  expect_true(is.function(getExportedValue("enemduR", "enemdu_compare_official_ipm")))
})

test_that("official IPM benchmarks return 9 annual December 2025 rows", {
  benchmarks <- enemdu_official_ipm_benchmarks()

  expect_s3_class(benchmarks, "tbl_df")
  expect_equal(nrow(benchmarks), 9L)
  expect_setequal(benchmarks$indicator_id, c("tpm", "tpem", "ipm"))
  expect_setequal(benchmarks$domain_value, c("national", "urban", "rural"))
  expect_true(all(benchmarks$period == "2025-12"))
  expect_true(all(benchmarks$survey_type == "anual"))
})

test_that("official IPM benchmark values are stored as proportions", {
  benchmarks <- enemdu_official_ipm_benchmarks()

  national_tpm <- benchmarks[
    benchmarks$indicator_id == "tpm" &
      benchmarks$domain_value == "national",
    ,
    drop = FALSE
  ]
  rural_tpem <- benchmarks[
    benchmarks$indicator_id == "tpem" &
      benchmarks$domain_value == "rural",
    ,
    drop = FALSE
  ]
  urban_ipm <- benchmarks[
    benchmarks$indicator_id == "ipm" &
      benchmarks$domain_value == "urban",
    ,
    drop = FALSE
  ]

  expect_equal(national_tpm$official_estimate, 0.417)
  expect_equal(rural_tpem$official_estimate, 0.348)
  expect_equal(urban_ipm$official_estimate, 0.132)
  expect_equal(urban_ipm$official_reported_value, 13.2)
  expect_equal(urban_ipm$reported_unit, "points")
})

test_that("official IPM benchmarks do not claim institutional validation", {
  benchmarks <- enemdu_official_ipm_benchmarks()

  expect_equal(
    unique(benchmarks$official_validation_status),
    "published_benchmark_not_institutional_validation"
  )
})

test_that("IPM comparison detects exact matches", {
  benchmarks <- enemdu_official_ipm_benchmarks()
  estimates <- benchmarks[
    ,
    c("period", "survey_type", "indicator_id", "domain_type", "domain_value", "official_estimate"),
    drop = FALSE
  ]
  names(estimates)[names(estimates) == "official_estimate"] <- "estimate"

  comparison <- enemdu_compare_official_ipm(estimates, benchmarks)

  expect_true(all(comparison$comparison_status == "matched_reported_rounding"))
  expect_true(all(comparison$abs_difference_pp == 0))
  expect_equal(unique(comparison$official_validation_status), "not_officially_validated")
})

test_that("IPM comparison detects within-tolerance differences", {
  benchmarks <- enemdu_official_ipm_benchmarks()
  benchmark <- benchmarks[
    benchmarks$indicator_id == "tpm" &
      benchmarks$domain_value == "national",
    ,
    drop = FALSE
  ]
  estimates <- benchmark[
    ,
    c("period", "survey_type", "indicator_id", "domain_type", "domain_value"),
    drop = FALSE
  ]
  estimates$estimate <- 0.420

  comparison <- enemdu_compare_official_ipm(
    estimates = estimates,
    benchmarks = benchmark,
    tolerance_pp = 0.5
  )

  expect_equal(comparison$comparison_status, "benchmark_comparison_within_tolerance")
  expect_equal(comparison$difference_pp, 0.3, tolerance = 1e-12)
})

test_that("IPM comparison detects outside-tolerance differences", {
  benchmarks <- enemdu_official_ipm_benchmarks()
  benchmark <- benchmarks[
    benchmarks$indicator_id == "tpm" &
      benchmarks$domain_value == "national",
    ,
    drop = FALSE
  ]
  estimates <- benchmark[
    ,
    c("period", "survey_type", "indicator_id", "domain_type", "domain_value"),
    drop = FALSE
  ]
  estimates$estimate <- 0.430

  comparison <- enemdu_compare_official_ipm(
    estimates = estimates,
    benchmarks = benchmark,
    tolerance_pp = 0.5
  )

  expect_equal(comparison$comparison_status, "benchmark_comparison_outside_tolerance")
  expect_equal(comparison$difference_pp, 1.3, tolerance = 1e-12)
})

test_that("IPM comparison handles point scale consistently", {
  benchmarks <- enemdu_official_ipm_benchmarks()
  benchmark <- benchmarks[
    benchmarks$indicator_id == "ipm" &
      benchmarks$domain_value == "national",
    ,
    drop = FALSE
  ]
  estimates <- benchmark[
    ,
    c("period", "survey_type", "indicator_id", "domain_type", "domain_value"),
    drop = FALSE
  ]
  estimates$estimate <- 0.205

  comparison <- enemdu_compare_official_ipm(estimates, benchmark)

  expect_equal(comparison$estimate_percent, 20.5)
  expect_equal(comparison$official_estimate_percent, 20.5)
  expect_equal(comparison$difference_pp, 0)
})
