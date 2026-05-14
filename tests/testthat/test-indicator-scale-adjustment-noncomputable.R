test_that("scale-adjusted indicator preserves noncomputable survey decision", {
  data <- tibble::tibble(
    idhogar = c("h1", "h1", "h2", "h2"),
    hsize = c(2L, 2L, 2L, 2L),
    upm = c(1, 2, 3, 4),
    estrato = c(1, 1, 2, 2),
    fexp = c(1, 1, 1, 1),
    ingtot_pc = c(NA_real_, NA_real_, NA_real_, NA_real_)
  )

  out <- enemdu_indicator_estimate(
    data = data,
    indicator_id = "ingreso_percapita_familiar",
    survey_type = "mensual",
    sample_n_min = 1
  )

  expect_s3_class(out, "enemdu_indicator_estimate")
  expect_true(out$household_scale_adjustment_applied_to_precision)
  expect_equal(out$decision, "no_recommended_inference")
  expect_equal(out$failed_reasons, "no_valid_observations")
  expect_equal(out$quality_flag, "not_evaluable")
  expect_equal(out$warning_flag, "inference_not_recommended")
  expect_false(out$decision == "precision_evaluation_error")
  expect_equal(out$representativity_flag, "design_domain_inference_not_recommended")
})

test_that("scale-adjusted grouped indicator preserves noncomputable subgroup decision", {
  data <- tibble::tibble(
    idhogar = c("h1", "h1", "h2", "h2"),
    hsize = c(2L, 2L, 2L, 2L),
    area = c(1, 1, 2, 2),
    upm = c(1, 2, 3, 4),
    estrato = c(1, 1, 2, 2),
    fexp = c(1, 1, 1, 1),
    ingtot_pc = c(NA_real_, NA_real_, 10, 20)
  )

  out <- enemdu_indicator_estimate(
    data = data,
    indicator_id = "ingreso_percapita_familiar",
    group_vars = "area",
    survey_type = "mensual",
    sample_n_min = 1
  )

  expect_s3_class(out, "enemdu_indicator_estimate")
  expect_true(out$household_scale_adjustment_applied_to_precision[1])
  expect_true("area" %in% names(out))

  area_1 <- out[out$area == 1, , drop = FALSE]

  expect_equal(nrow(area_1), 1)
  expect_equal(area_1$decision, "no_recommended_inference")
  expect_equal(area_1$failed_reasons, "no_valid_observations")
  expect_equal(area_1$quality_flag, "not_evaluable")
  expect_equal(area_1$warning_flag, "inference_not_recommended")
  expect_false(any(out$decision == "precision_evaluation_error"))
})
