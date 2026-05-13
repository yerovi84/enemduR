test_that("indicator estimate computes per-capita income mean from registry", {
  data <- tibble::tibble(
    idhogar = c("h1", "h1", "h2", "h2"),
    hsize = c(2L, 2L, 2L, 2L),
    upm = c(1, 2, 3, 4),
    estrato = c(1, 1, 2, 2),
    fexp = c(1, 1, 1, 1),
    ingtot_pc = c(10, 20, 30, 40)
  )

  out <- enemdu_indicator_estimate(
    data = data,
    indicator_id = "ingreso_percapita_familiar",
    survey_type = "anual",
    sample_n_min = 1
  )

  expect_s3_class(out, "enemdu_indicator_estimate")
  expect_equal(nrow(out), 1)
  expect_equal(out$indicator_id, "ingreso_percapita_familiar")
  expect_equal(out$estimate, 25)
  expect_true("standard_error" %in% names(out))
  expect_true("adjusted_unweighted_n" %in% names(out))
  expect_true(out$household_scale_adjustment_applied_to_precision)
  expect_true("representativity_flag" %in% names(out))
})

test_that("indicator estimate can be grouped", {
  data <- tibble::tibble(
    idhogar = c("h1", "h1", "h2", "h2"),
    hsize = c(2L, 2L, 2L, 2L),
    area = c("urbano", "urbano", "rural", "rural"),
    upm = c(1, 2, 3, 4),
    estrato = c(1, 1, 2, 2),
    fexp = c(1, 1, 1, 1),
    ingtot_pc = c(10, 20, 30, 40)
  )

  out <- enemdu_indicator_estimate(
    data = data,
    indicator_id = "ingreso_percapita_familiar",
    group_vars = "area",
    survey_type = "mensual",
    sample_n_min = 1
  )

  expect_equal(nrow(out), 2)
  expect_true("area" %in% names(out))
  expect_equal(out$estimate[out$area == "urbano"], 15)
  expect_equal(out$estimate[out$area == "rural"], 35)
  expect_true(all(out$domain_is_design_domain))
})

test_that("indicator estimate computes total persons through count variable", {
  data <- tibble::tibble(
    upm = c(1, 2, 3, 4),
    estrato = c(1, 1, 2, 2),
    fexp = c(10, 10, 20, 20)
  )

  out <- enemdu_indicator_estimate(
    data = data,
    indicator_id = "conteo_personas",
    survey_type = "anual",
    sample_n_min = 1
  )

  expect_equal(out$estimate, 60)
  expect_true(out$created_count_value)
})

test_that("indicator estimate computes poverty proportion when flags exist", {
  data <- tibble::tibble(
    idhogar = c("h1", "h1", "h2", "h2"),
    hsize = c(2L, 2L, 2L, 2L),
    upm = c(1, 2, 3, 4),
    estrato = c(1, 1, 2, 2),
    fexp = c(1, 1, 1, 1),
    pobre = c(1, 0, 1, 1)
  )

  out <- enemdu_indicator_estimate(
    data = data,
    indicator_id = "pobreza_ingresos",
    value = "pobre",
    survey_type = "anual",
    sample_n_min = 1
  )

  expect_equal(out$estimate, 0.75)
  expect_true(out$household_scale_adjustment_applied_to_precision)
})

test_that("indicator estimate fails for unsupported estimator type", {
  data <- tibble::tibble(
    upm = c(1, 2),
    estrato = c(1, 1),
    fexp = c(1, 1),
    quintil_ingreso_pc = c(1, 2)
  )

  expect_error(
    enemdu_indicator_estimate(
      data = data,
      indicator_id = "quintil_ingreso_pc",
      survey_type = "anual"
    ),
    class = "enemdu_error_unsupported_estimator_type"
  )
})

test_that("indicator estimate detects province outside monthly design scope", {
  data <- tibble::tibble(
    idhogar = c("h1", "h1", "h2", "h2"),
    hsize = c(2L, 2L, 2L, 2L),
    provincia = c("P1", "P1", "P2", "P2"),
    upm = c(1, 2, 3, 4),
    estrato = c(1, 1, 2, 2),
    fexp = c(1, 1, 1, 1),
    ingtot_pc = c(10, 20, 30, 40)
  )

  out <- enemdu_indicator_estimate(
    data = data,
    indicator_id = "ingreso_percapita_familiar",
    group_vars = "provincia",
    survey_type = "mensual",
    sample_n_min = 1
  )

  expect_false(out$domain_is_design_domain[1])
  expect_equal(out$domain_scope_flag[1], "analysis_domain_requires_precision")
  expect_true(grepl("no debe presentarse como dominio de diseño", out$representativity_note[1]))
})

test_that("indicator estimate accepts province as annual design scope", {
  data <- tibble::tibble(
    idhogar = c("h1", "h1", "h2", "h2"),
    hsize = c(2L, 2L, 2L, 2L),
    provincia = c("P1", "P1", "P2", "P2"),
    upm = c(1, 2, 3, 4),
    estrato = c(1, 1, 2, 2),
    fexp = c(1, 1, 1, 1),
    ingtot_pc = c(10, 20, 30, 40)
  )

  out <- enemdu_indicator_estimate(
    data = data,
    indicator_id = "ingreso_percapita_familiar",
    group_vars = "provincia",
    survey_type = "anual",
    sample_n_min = 1
  )

  expect_true(out$domain_is_design_domain[1])
  expect_equal(out$domain_scope_flag[1], "design_domain")
})

test_that("indicator estimate errors with strict domain outside scope", {
  data <- tibble::tibble(
    idhogar = c("h1", "h1", "h2", "h2"),
    hsize = c(2L, 2L, 2L, 2L),
    provincia = c("P1", "P1", "P2", "P2"),
    upm = c(1, 2, 3, 4),
    estrato = c(1, 1, 2, 2),
    fexp = c(1, 1, 1, 1),
    ingtot_pc = c(10, 20, 30, 40)
  )

  expect_error(
    enemdu_indicator_estimate(
      data = data,
      indicator_id = "ingreso_percapita_familiar",
      group_vars = "provincia",
      survey_type = "mensual",
      strict_domain = TRUE,
      sample_n_min = 1
    ),
    class = "enemdu_error_domain_out_of_scope"
  )
})

test_that("indicator estimate can skip integrated representativity", {
  data <- tibble::tibble(
    idhogar = c("h1", "h1", "h2", "h2"),
    hsize = c(2L, 2L, 2L, 2L),
    provincia = c("P1", "P1", "P2", "P2"),
    upm = c(1, 2, 3, 4),
    estrato = c(1, 1, 2, 2),
    fexp = c(1, 1, 1, 1),
    ingtot_pc = c(10, 20, 30, 40)
  )

  out <- enemdu_indicator_estimate(
    data = data,
    indicator_id = "ingreso_percapita_familiar",
    group_vars = "provincia",
    survey_type = "mensual",
    integrate_representativity = FALSE,
    sample_n_min = 1
  )

  expect_true("representativity_flag" %in% names(out))
  expect_true(all(is.na(out$representativity_flag)))
})

test_that("income KPI returns income estimates", {
  data <- tibble::tibble(
    idhogar = c("h1", "h1", "h2", "h2"),
    hsize = c(2L, 2L, 2L, 2L),
    upm = c(1, 2, 3, 4),
    estrato = c(1, 1, 2, 2),
    fexp = c(1, 1, 1, 1),
    ingtot_pc = c(10, 20, 30, 40),
    ingrl = c(5, 15, 25, 35)
  )

  out <- enemdu_kpi_income(
    data = data,
    survey_type = "anual",
    sample_n_min = 1
  )

  expect_true("ingreso_percapita_familiar" %in% out$indicator_id)
  expect_true("ingreso_laboral" %in% out$indicator_id)
  expect_true("representativity_flag" %in% names(out))
})

test_that("optional bonus KPI builds bonus variables if p78 exists", {
  data <- tibble::tibble(
    idhogar = c("h1", "h1", "h2", "h2"),
    upm = c(1, 2, 3, 4),
    estrato = c(1, 1, 2, 2),
    fexp = c(1, 1, 1, 1),
    p78 = c(10, 0, 20, NA_real_)
  )

  out <- enemdu_kpi_optional_bonuses(
    data = data,
    survey_type = "anual",
    sample_n_min = 1
  )

  expect_true("transferencias_bonos_total_enemdu" %in% out$indicator_id)
  expect_true("bono_jgl_receptores" %in% out$indicator_id)
  expect_true("bono_jgl_monto_promedio_receptor" %in% out$indicator_id)
  expect_true("representativity_flag" %in% names(out))
})

test_that("optional bonus KPI can be grouped", {
  data <- tibble::tibble(
    idhogar = c("h1", "h1", "h2", "h2"),
    area = c("urbano", "urbano", "rural", "rural"),
    upm = c(1, 2, 3, 4),
    estrato = c(1, 1, 2, 2),
    fexp = c(1, 1, 1, 1),
    p78 = c(10, 0, 20, 0)
  )

  out <- enemdu_kpi_optional_bonuses(
    data = data,
    group_vars = "area",
    survey_type = "mensual",
    sample_n_min = 1
  )

  expect_true("area" %in% names(out))
  expect_true(all(c("urbano", "rural") %in% out$area))
  expect_true(all(out$domain_is_design_domain))
})
