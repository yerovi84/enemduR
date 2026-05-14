test_that("survey estimators return controlled rows when estimates are not computable", {
  data <- tibble::tibble(
    upm = c(1, 2, 3, 4),
    estrato = c(1, 1, 2, 2),
    fexp = c(1, 1, 1, 1),
    x = c(Inf, Inf, Inf, Inf)
  )

  out <- enemdu_survey_mean(
    data = data,
    value = "x",
    survey_type = "mensual",
    sample_n_min = 1
  )

  expect_s3_class(out, "enemdu_survey_estimate")
  expect_equal(out$decision, "no_recommended_inference")
  expect_equal(out$failed_reasons, "no_valid_observations")
  expect_true(is.na(out$estimate))
  expect_true(is.na(out$standard_error))
})

test_that("survey estimators do not send NA estimates to precision evaluation", {
  data <- tibble::tibble(
    upm = c(1, 2, 3, 4),
    estrato = c(1, 1, 2, 2),
    fexp = c(1, 1, 1, 1),
    x = c(NA_real_, NA_real_, NA_real_, NA_real_)
  )

  out <- enemdu_survey_mean(
    data = data,
    value = "x",
    survey_type = "mensual",
    sample_n_min = 1
  )

  expect_s3_class(out, "enemdu_survey_estimate")
  expect_equal(out$decision, "no_recommended_inference")
  expect_equal(out$failed_reasons, "no_valid_observations")
  expect_false(out$decision == "precision_evaluation_error")
})
