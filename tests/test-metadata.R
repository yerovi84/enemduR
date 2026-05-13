test_that("supported survey types are stable", {
  types <- enemduR:::.enemdu_supported_survey_types()

  expect_identical(
    types,
    c("mensual", "trimestral", "anual")
  )
})

test_that("survey registry loads with expected columns", {
  registry <- enemduR:::.enemdu_survey_registry()

  expect_true(nrow(registry) >= 3)

  expect_true(all(
    c(
      "survey_type",
      "default_weight",
      "default_psu",
      "default_strata",
      "national",
      "urban_rural"
    ) %in% names(registry)
  ))
})

test_that("comparability registry loads with expected columns", {
  registry <- enemduR:::.enemdu_comparability_registry()

  expect_true(nrow(registry) >= 1)

  expect_true(all(
    c(
      "regimen_id",
      "survey_type_scope",
      "start_period",
      "end_period",
      "alert_level",
      "note"
    ) %in% names(registry)
  ))
})
