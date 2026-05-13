test_that("representativity report classifies design-domain reliable estimates", {
  estimate <- tibble::tibble(
    indicator_id = "test_indicator",
    survey_type = "anual",
    decision = "reliable",
    estimate = 10
  )

  out <- enemdu_representativity_report(
    estimate = estimate,
    survey_type = "anual",
    domain_level = "provincia_24"
  )

  expect_s3_class(out, "enemdu_integrated_representativity_report")
  expect_true(out$domain_is_design_domain)
  expect_equal(out$representativity_flag, "design_domain_reliable")
})

test_that("representativity report warns analysis domain even if precision is reliable", {
  estimate <- tibble::tibble(
    indicator_id = "test_indicator",
    survey_type = "mensual",
    decision = "reliable",
    estimate = 10
  )

  out <- enemdu_representativity_report(
    estimate = estimate,
    survey_type = "mensual",
    domain_level = "provincia_24"
  )

  expect_false(out$domain_is_design_domain)
  expect_equal(out$representativity_flag, "analysis_domain_reliable_but_not_design_domain")
  expect_true(grepl("no debe presentarse como dominio de diseño", out$representativity_note))
})

test_that("representativity report can infer domain from group vars", {
  estimate <- tibble::tibble(
    indicator_id = "test_indicator",
    survey_type = "trimestral",
    area = "urbano",
    decision = "reduced_precision",
    estimate = 10
  )

  out <- enemdu_representativity_report(
    estimate = estimate,
    group_vars = "area"
  )

  expect_true(out$domain_is_design_domain)
  expect_equal(out$representativity_flag, "design_domain_reduced_precision")
})

test_that("representativity report requires survey type if absent", {
  estimate <- tibble::tibble(
    indicator_id = "test_indicator",
    decision = "reliable",
    estimate = 10
  )

  expect_error(
    enemdu_representativity_report(
      estimate = estimate,
      domain_level = "nacional"
    ),
    class = "enemdu_error_missing_argument"
  )
})

test_that("representativity report keeps backward compatibility for numeric estimate", {
  out <- enemdu_representativity_report(
    estimate = 0.5,
    standard_error = 0.01,
    degrees_freedom = 20,
    survey_type = "anual",
    estimator_type = "proportion_0_1",
    effective_n = 100
  )

  expect_s3_class(out, "enemdu_representativity_report")
  expect_equal(out$survey_type, "anual")
})
