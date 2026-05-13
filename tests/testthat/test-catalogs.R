test_that("variable catalog loads with expected columns", {
  catalog <- enemdu_variable_catalog()

  expect_true(nrow(catalog) > 0)

  expect_true(all(
    c(
      "variable",
      "canonical_name",
      "variable_group",
      "analysis_level",
      "type_expected",
      "description"
    ) %in% names(catalog)
  ))
})

test_that("indicator registry loads with expected columns", {
  registry <- enemdu_indicator_registry()

  expect_true(nrow(registry) > 0)

  expect_true(all(
    c(
      "indicator_id",
      "indicator_label",
      "analysis_level",
      "estimator_type",
      "required_vars",
      "scale_adjustment_required",
      "representativity_required"
    ) %in% names(registry)
  ))
})

test_that("validation registry loads with expected columns", {
  registry <- enemdu_validation_registry()

  expect_true(nrow(registry) > 0)

  expect_true(all(
    c(
      "check_id",
      "check_type",
      "severity",
      "required_vars",
      "rule_type",
      "message"
    ) %in% names(registry)
  ))
})

test_that("income component registry loads with expected columns", {
  registry <- enemdu_income_component_registry()

  expect_true(nrow(registry) > 0)

  expect_true(all(
    c(
      "stage",
      "output_variable",
      "input_variable",
      "operation",
      "nonresponse_codes",
      "description"
    ) %in% names(registry)
  ))
})

test_that("required variables can be resolved for an indicator", {
  vars <- enemdu_required_vars_for_indicator("ingreso_percapita_familiar")

  expect_true("idhogar" %in% vars)
  expect_true("ingtot" %in% vars)
  expect_true("hsize" %in% vars)
})
