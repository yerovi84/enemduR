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
    sample_n_min = 1
  )

  expect_s3_class(out, "enemdu_indicator_estimate")
  expect_equal(nrow(out), 1)
  expect_equal(out$indicator_id, "ingreso_percapita_familiar")
  expect_equal(out$estimate, 25)
  expect_true("standard_error" %in% names(out))
  expect_true("adjusted_unweighted_n" %in% names(out))
  expect_true(out$household_scale_adjustment_applied_to_precision)
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
    sample_n_min = 1
  )

  expect_equal(nrow(out), 2)
  expect_true("area" %in% names(out))
  expect_equal(out$estimate[out$area == "urbano"], 15)
  expect_equal(out$estimate[out$area == "rural"], 35)
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
      indicator_id = "quintil_ingreso_pc"
    ),
    class = "enemdu_error_unsupported_estimator_type"
  )
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
    sample_n_min = 1
  )

  expect_true("ingreso_percapita_familiar" %in% out$indicator_id)
  expect_true("ingreso_laboral" %in% out$indicator_id)
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
    sample_n_min = 1
  )

  expect_true("transferencias_bonos_total_enemdu" %in% out$indicator_id)
  expect_true("bono_jgl_receptores" %in% out$indicator_id)
  expect_true("bono_jgl_monto_promedio_receptor" %in% out$indicator_id)
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
    sample_n_min = 1
  )

  expect_true("area" %in% names(out))
  expect_true(all(c("urbano", "rural") %in% out$area))
})
