.nbi_repro_test_data <- function(area = c("1", "1", "2", "2", "1", "1", "2", "2")) {
  tibble::tibble(
    idhogar = paste0("h", seq_along(area)),
    hsize = rep(1L, length(area)),
    upm = seq_along(area),
    estrato = rep(c(1, 2), each = length(area) / 2),
    fexp = rep(1, length(area)),
    area = area,
    comp1 = c(0, 1, 1, 1, 0, 0, 1, 1)[seq_along(area)],
    comp2 = c(0, 0, 1, 1, 0, 0, 0, 1)[seq_along(area)],
    comp3 = c(0, 0, 0, 1, 0, 0, 0, 0)[seq_along(area)],
    comp4 = c(0, 0, 0, 1, 0, 0, 0, 0)[seq_along(area)],
    comp5 = c(0, 0, 0, 1, 0, 0, 0, 0)[seq_along(area)]
  )
}

test_that("NBI reproducibility preflight passes with complete variables", {
  data <- .nbi_repro_test_data()

  preflight <- enemdu_validate_nbi_reproducibility_inputs(data)

  expect_s3_class(preflight, "enemdu_nbi_reproducibility_preflight")
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

test_that("NBI reproducibility preflight accepts factor binary component labels", {
  data <- .nbi_repro_test_data()

  data$comp1 <- factor(as.character(data$comp1), levels = c("0", "1"))
  data$comp2 <- factor(as.character(data$comp2), levels = c("0", "1"))
  data$comp3 <- factor(as.character(data$comp3), levels = c("0", "1"))
  data$comp4 <- factor(as.character(data$comp4), levels = c("0", "1"))
  data$comp5 <- factor(as.character(data$comp5), levels = c("0", "1"))

  preflight <- enemdu_validate_nbi_reproducibility_inputs(data)

  expect_true(isTRUE(attr(preflight, "preflight_passed")))
  expect_true(all(preflight$issue == "ok"))

  result <- enemdu_run_nbi_reproducibility(
    data = data,
    sample_n_min = 1
  )

  expect_s3_class(result, "enemdu_nbi_reproducibility_result")
  expect_equal(result$validation$validation_status, "passed")
})

test_that("NBI reproducibility preflight rejects invalid factor component labels", {
  data <- .nbi_repro_test_data()
  data$comp1 <- factor(c("0", "1", "yes", "0", "0", "1", "0", "1"))

  preflight <- enemdu_validate_nbi_reproducibility_inputs(data)
  comp1_row <- preflight[preflight$variable == "comp1", , drop = FALSE]

  expect_false(isTRUE(attr(preflight, "preflight_passed")))
  expect_equal(comp1_row$issue, "non_binary_component")
})
test_that("NBI reproducibility preflight reports missing component variables", {
  data <- .nbi_repro_test_data()
  data$comp5 <- NULL

  preflight <- enemdu_validate_nbi_reproducibility_inputs(data)
  comp5_row <- preflight[preflight$variable == "comp5", , drop = FALSE]

  expect_false(isTRUE(attr(preflight, "preflight_passed")))
  expect_false(comp5_row$present)
  expect_equal(comp5_row$issue, "missing_variable")
})

test_that("NBI reproducibility runner returns structured result", {
  data <- .nbi_repro_test_data()

  result <- enemdu_run_nbi_reproducibility(
    data = data,
    sample_n_min = 1
  )

  expect_s3_class(result, "enemdu_nbi_reproducibility_result")
  expect_true(all(c(
    "preflight",
    "validation",
    "estimates",
    "benchmark_comparison",
    "official_validation_status"
  ) %in% names(result)))
  expect_equal(result$validation$validation_status, "passed")
  expect_true(all(c("pobreza_nbi", "pobreza_extrema_nbi") %in% result$estimates$indicator_id))
  expect_equal(result$official_validation_status, "not_officially_validated")
})

test_that("NBI reproducibility runner maps urban and rural area values", {
  data <- .nbi_repro_test_data(area = c("1", "1", "2", "2", "1", "1", "2", "2"))

  result <- enemdu_run_nbi_reproducibility(
    data = data,
    sample_n_min = 1
  )

  area_rows <- result$estimates[!is.na(result$estimates$.enemdu_area_domain), , drop = FALSE]

  expect_true(all(c("urban", "rural") %in% area_rows$.enemdu_area_domain))
})

test_that("NBI reproducibility runner fails clearly with missing components", {
  data <- .nbi_repro_test_data()
  data$comp1 <- NULL

  expect_error(
    enemdu_run_nbi_reproducibility(
      data = data,
      sample_n_min = 1
    ),
    class = "enemdu_error_nbi_reproducibility_preflight_failed"
  )
})

test_that("NBI reproducibility result stores policy attribute", {
  data <- .nbi_repro_test_data()

  result <- enemdu_run_nbi_reproducibility(
    data = data,
    sample_n_min = 1
  )

  policy <- attr(result, "reproducibility_policy")

  expect_false(is.null(policy))
  expect_equal(policy$component_vars, paste0("comp", 1:5))
  expect_match(policy$note, "Official validation requires", fixed = TRUE)
})
