test_that("labor indicator registry loads with expected structure", {
  registry <- enemdu_labor_indicator_registry()

  expect_equal(nrow(registry), 32)
  expect_equal(anyDuplicated(registry$indicator_id), 0L)

  expect_true(all(
    c(
      "indicator_id",
      "indicator_label",
      "indicator_group",
      "unit",
      "analysis_level",
      "estimator_type",
      "universe",
      "weight",
      "required_vars",
      "derived_vars",
      "numerator_var",
      "denominator_var",
      "value_var",
      "condact_codes",
      "secemp_codes",
      "output_scale",
      "domain_scope_policy",
      "design_domain_policy",
      "representativity_required",
      "scale_adjustment_required",
      "implementation_status",
      "source_status",
      "method_note"
    ) %in% names(registry)
  ))
})

test_that("labor indicator registry documents all implemented employment KPI ids", {
  registry <- enemdu_labor_indicator_registry()

  expected_ids <- c(
    "labor_desempleo_abierto_total",
    "labor_desempleo_oculto_total",
    "labor_desempleo_total",
    "labor_empleo_adecuado_total",
    "labor_empleo_no_clasificado_total",
    "labor_empleo_no_remunerado_total",
    "labor_empleo_total",
    "labor_otro_empleo_no_pleno_total",
    "labor_pea_total",
    "labor_pei_total",
    "labor_pet_total",
    "labor_sector_domestico_total",
    "labor_sector_formal_total",
    "labor_sector_informal_total",
    "labor_sector_no_clasificado_total",
    "labor_subempleo_ingresos_total",
    "labor_subempleo_tiempo_total",
    "labor_subempleo_total",
    "labor_tasa_desempleo",
    "labor_tasa_desempleo_abierto",
    "labor_tasa_desempleo_oculto",
    "labor_tasa_empleo_adecuado",
    "labor_tasa_empleo_no_clasificado",
    "labor_tasa_empleo_no_remunerado",
    "labor_tasa_ocupacion_bruta",
    "labor_tasa_ocupacion_global",
    "labor_tasa_otro_empleo_no_pleno",
    "labor_tasa_participacion_bruta",
    "labor_tasa_participacion_global",
    "labor_tasa_subempleo",
    "labor_tasa_subempleo_ingresos",
    "labor_tasa_subempleo_tiempo"
  )

  expect_setequal(registry$indicator_id, expected_ids)
})

test_that("labor indicator registry separates totals and rates", {
  registry <- enemdu_labor_indicator_registry()

  totals <- registry[registry$estimator_type == "total", , drop = FALSE]
  rates <- registry[registry$estimator_type == "proportion_0_1", , drop = FALSE]

  expect_equal(nrow(totals), 18)
  expect_equal(nrow(rates), 14)

  expect_true(all(totals$output_scale == "count"))
  expect_true(all(rates$output_scale == "proportion_0_1"))
  expect_true(all(!is.na(totals$value_var) & nzchar(totals$value_var)))
  expect_true(all(!is.na(rates$numerator_var) & nzchar(rates$numerator_var)))
  expect_true(all(!is.na(rates$denominator_var) & nzchar(rates$denominator_var)))
})

test_that("labor indicator registry records design-domain policy", {
  registry <- enemdu_labor_indicator_registry()

  expect_true(all(
    registry$domain_scope_policy == "observed_default_design_output_filter_available"
  ))

  expect_true(all(
    registry$design_domain_policy ==
      "domain_scope_design_filters_output_after_estimation_not_microdata"
  ))
})

test_that("sector labor indicators explicitly require secemp", {
  registry <- enemdu_labor_indicator_registry()
  sector_rows <- registry[grepl("^labor_sector_", registry$indicator_id), , drop = FALSE]

  expect_equal(nrow(sector_rows), 4)
  expect_true(all(grepl("secemp", sector_rows$required_vars, fixed = TRUE)))
  expect_setequal(as.character(stats::na.omit(sector_rows$secemp_codes)), c("1", "2", "3", "4"))
})
