.poverty_reproducibility_test_data <- function(area = c("1", "1", "1", "1", "2", "2", "2", "2")) {
  tibble::tibble(
    idhogar = paste0("h", seq_along(area)),
    hsize = rep(1L, length(area)),
    upm = seq_along(area),
    estrato = rep(seq_len(length(area) / 2), each = 2L),
    fexp = rep(1, length(area)),
    area = area,
    ingtot_pc = c(30, 70, 120, 150, 20, 40, 80, 150)[seq_along(area)]
  )
}

test_that("poverty reproducibility preflight passes with complete variables", {
  data <- .poverty_reproducibility_test_data()

  preflight <- enemdu_validate_poverty_reproducibility_inputs(data)

  expect_s3_class(preflight, "enemdu_poverty_reproducibility_preflight")
  expect_true(isTRUE(attr(preflight, "preflight_passed")))
  expect_true(all(c(
    "variable",
    "role",
    "present",
    "class",
    "missing_n",
    "non_missing_n",
    "issue"
  ) %in% names(preflight)))
  expect_true(all(preflight$issue == "ok"))
})

test_that("poverty reproducibility preflight reports missing variables", {
  data <- .poverty_reproducibility_test_data()
  data$area <- NULL

  preflight <- enemdu_validate_poverty_reproducibility_inputs(data)

  area_row <- preflight[preflight$variable == "area", , drop = FALSE]

  expect_false(isTRUE(attr(preflight, "preflight_passed")))
  expect_false(area_row$present)
  expect_equal(area_row$issue, "missing_variable")
})

test_that("poverty reproducibility workflow returns expected class", {
  data <- .poverty_reproducibility_test_data()

  result <- enemdu_run_poverty_reproducibility(
    data = data,
    sample_n_min = 1
  )

  expect_s3_class(result, "enemdu_poverty_reproducibility_result")
  expect_s3_class(result, "enemdu_official_poverty_comparison")
  expect_true(all(c("national", "urban", "rural") %in% result$domain_value))
})

test_that("reproducibility runner honors custom survey design variables during preflight", {
  data <- tibble::tibble(
    idhogar = paste0("h", seq_len(8)),
    hsize = rep(1L, 8),
    custom_psu = seq_len(8),
    custom_strata = rep(c(1, 2), each = 4),
    custom_weight = rep(1, 8),
    custom_area = c("1", "1", "2", "2", "1", "1", "2", "2"),
    custom_income = c(80, 120, 40, 140, 80, 120, 40, 140)
  )

  result <- enemdu_run_poverty_reproducibility(
    data = data,
    income_var = "custom_income",
    area_var = "custom_area",
    ids = "custom_psu",
    strata = "custom_strata",
    weight = "custom_weight",
    run_preflight = TRUE,
    sample_n_min = 1
  )

  expect_s3_class(result, "enemdu_poverty_reproducibility_result")
  expect_true(all(result$official_validation_status == "not_officially_validated"))
  expect_true(all(result$domain_value %in% c("national", "urban", "rural")))
})

test_that("reproducibility runner honors custom survey design variables without preflight", {
  data <- tibble::tibble(
    idhogar = paste0("h", seq_len(8)),
    hsize = rep(1L, 8),
    custom_psu = seq_len(8),
    custom_strata = rep(c(1, 2), each = 4),
    custom_weight = rep(1, 8),
    custom_area = c("1", "1", "2", "2", "1", "1", "2", "2"),
    custom_income = c(80, 120, 40, 140, 80, 120, 40, 140)
  )

  result <- enemdu_run_poverty_reproducibility(
    data = data,
    income_var = "custom_income",
    area_var = "custom_area",
    ids = "custom_psu",
    strata = "custom_strata",
    weight = "custom_weight",
    run_preflight = FALSE,
    sample_n_min = 1
  )

  expect_s3_class(result, "enemdu_poverty_reproducibility_result")
  expect_true(all(result$official_validation_status == "not_officially_validated"))
  expect_true(all(result$domain_value %in% c("national", "urban", "rural")))
})

test_that("poverty reproducibility workflow does not claim official validation", {
  data <- .poverty_reproducibility_test_data()

  result <- enemdu_run_poverty_reproducibility(
    data = data,
    sample_n_min = 1
  )

  expect_equal(unique(result$official_validation_status), "not_officially_validated")
  expect_true(all(grepl("not an official validation claim", result$official_validation_note, fixed = TRUE)))
})

test_that("poverty reproducibility workflow maps area values 1 and 2", {
  data <- .poverty_reproducibility_test_data(area = c("1", "1", "1", "1", "2", "2", "2", "2"))

  result <- enemdu_run_poverty_reproducibility(
    data = data,
    sample_n_min = 1
  )

  area_rows <- result[result$domain_type == "area", , drop = FALSE]

  expect_true(all(c("urban", "rural") %in% area_rows$domain_value))
  expect_true(all(area_rows$reproducibility_scope == "urban_rural"))
})

test_that("poverty reproducibility workflow fails clearly when income is missing", {
  data <- .poverty_reproducibility_test_data()
  data$ingtot_pc <- NULL

  expect_error(
    enemdu_run_poverty_reproducibility(
      data = data,
      sample_n_min = 1
    ),
    class = "enemdu_error_poverty_reproducibility_preflight_failed"
  )
})

test_that("poverty reproducibility strict mode fails outside tolerance", {
  data <- .poverty_reproducibility_test_data()

  expect_error(
    enemdu_run_poverty_reproducibility(
      data = data,
      strict = TRUE,
      sample_n_min = 1
    ),
    class = "enemdu_error_official_poverty_comparison_mismatch"
  )
})

test_that("poverty reproducibility result stores policy attribute", {
  data <- .poverty_reproducibility_test_data()

  result <- enemdu_run_poverty_reproducibility(
    data = data,
    sample_n_min = 1
  )

  policy <- attr(result, "reproducibility_policy")

  expect_false(is.null(policy))
  expect_equal(policy$period, "2025-12")
  expect_match(policy$note, "does not imply official validation", fixed = TRUE)
})
