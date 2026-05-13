test_that("monthly ENEMDU validates national domain as design domain", {
  out <- enemdu_validate_domain_scope(
    survey_type = "mensual",
    domain_level = "nacional",
    emit = FALSE
  )

  expect_s3_class(out, "enemdu_domain_scope_report")
  expect_true(out$is_design_domain)
  expect_equal(out$scope_flag, "design_domain")
})

test_that("monthly ENEMDU validates province as analysis domain", {
  out <- enemdu_validate_domain_scope(
    survey_type = "mensual",
    domain_level = "provincia_24",
    emit = FALSE
  )

  expect_false(out$is_design_domain)
  expect_equal(out$scope_flag, "analysis_domain_requires_precision")
  expect_equal(out$severity, "warning")
})

test_that("annual ENEMDU validates province as design domain", {
  out <- enemdu_validate_domain_scope(
    survey_type = "anual",
    domain_level = "provincia_24",
    emit = FALSE
  )

  expect_true(out$is_design_domain)
  expect_equal(out$scope_flag, "design_domain")
})

test_that("strict domain validation errors outside design scope", {
  expect_error(
    enemdu_validate_domain_scope(
      survey_type = "mensual",
      domain_level = "provincia_24",
      strict = TRUE,
      emit = FALSE
    ),
    class = "enemdu_error_domain_out_of_scope"
  )
})

test_that("domain level can be inferred from group vars", {
  out <- enemdu_validate_domain_scope(
    survey_type = "trimestral",
    group_vars = c("area", "sexo"),
    emit = FALSE
  )

  expect_equal(nrow(out), 2)
  expect_true(out$is_design_domain[out$domain_var == "area"])
  expect_false(out$is_design_domain[out$domain_var == "sexo"])
})

test_that("unknown group var becomes sociodemographic analysis domain", {
  out <- enemdu_validate_domain_scope(
    survey_type = "anual",
    group_vars = "grupo_custom",
    emit = FALSE
  )

  expect_equal(out$domain_level, "subpoblacion_sociodemografica")
  expect_false(out$is_design_domain)
})
