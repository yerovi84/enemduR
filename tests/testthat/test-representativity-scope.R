test_that("representativity scope returns monthly scope", {
  scope <- enemdu_representativity_scope("mensual", emit = FALSE)

  expect_s3_class(scope, "enemdu_representativity_scope")
  expect_equal(scope$survey_type, "mensual")
  expect_true(grepl("nacional", tolower(scope$representativity_message)))
  expect_true(grepl("urbano-rural", tolower(scope$representativity_message)))
})

test_that("representativity scope returns annual province scope", {
  scope <- enemdu_representativity_scope("anual", emit = FALSE)

  expect_s3_class(scope, "enemdu_representativity_scope")
  expect_equal(scope$survey_type, "anual")
  expect_true(grepl("24 provincias", tolower(scope$representativity_message)))
})

test_that("precision evaluation classifies low effective sample size as not recommended", {
  result <- enemdu_evaluate_precision(
    estimate = 0.25,
    standard_error = 0.02,
    effective_n = 20,
    degrees_freedom = 30,
    estimator_type = "proportion_0_1"
  )

  expect_s3_class(result, "enemdu_precision_decision")
  expect_equal(result$decision, "no_recommended_inference")
})

test_that("precision evaluation classifies reliable mean with low cv", {
  result <- enemdu_evaluate_precision(
    estimate = 100,
    standard_error = 5,
    effective_n = 100,
    degrees_freedom = 20,
    estimator_type = "mean"
  )

  expect_s3_class(result, "enemdu_precision_decision")
  expect_equal(result$decision, "reliable")
})
