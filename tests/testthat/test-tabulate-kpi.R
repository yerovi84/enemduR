test_that("enemdu_tabulate computes weighted count without groups", {
  data <- tibble::tibble(
    fexp = c(1, 2, 3)
  )

  out <- enemdu_tabulate(
    data = data,
    statistic = "count"
  )

  expect_s3_class(out, "enemdu_tabulation")
  expect_equal(nrow(out), 1)
  expect_equal(out$estimate, 6)
  expect_equal(out$unweighted_n, 3L)
})

test_that("enemdu_tabulate computes weighted count by group", {
  data <- tibble::tibble(
    area = c("urbano", "urbano", "rural"),
    fexp = c(1, 2, 3)
  )

  out <- enemdu_tabulate(
    data = data,
    group_vars = "area",
    statistic = "count"
  )

  expect_equal(nrow(out), 2)
  expect_true("area" %in% names(out))
  expect_equal(out$estimate[out$area == "urbano"], 3)
  expect_equal(out$estimate[out$area == "rural"], 3)
})

test_that("enemdu_tabulate computes weighted mean", {
  data <- tibble::tibble(
    area = c("urbano", "urbano", "rural"),
    ingtot_pc = c(100, 200, 50),
    fexp = c(1, 3, 2)
  )

  out <- enemdu_tabulate(
    data = data,
    group_vars = "area",
    value = "ingtot_pc",
    statistic = "mean"
  )

  expect_equal(out$estimate[out$area == "urbano"], 175)
  expect_equal(out$estimate[out$area == "rural"], 50)
})

test_that("enemdu_tabulate computes weighted sum", {
  data <- tibble::tibble(
    area = c("urbano", "urbano", "rural"),
    bonos_optional_total = c(10, 20, 5),
    fexp = c(1, 2, 3)
  )

  out <- enemdu_tabulate(
    data = data,
    group_vars = "area",
    value = "bonos_optional_total",
    statistic = "sum"
  )

  expect_equal(out$estimate[out$area == "urbano"], 50)
  expect_equal(out$estimate[out$area == "rural"], 15)
})

test_that("enemdu_tabulate computes weighted proportion", {
  data <- tibble::tibble(
    area = c("urbano", "urbano", "rural", "rural"),
    pobre = c(1, 0, 1, 1),
    fexp = c(1, 1, 1, 3)
  )

  out <- enemdu_tabulate(
    data = data,
    group_vars = "area",
    value = "pobre",
    statistic = "proportion"
  )

  expect_equal(out$estimate[out$area == "urbano"], 0.5)
  expect_equal(out$estimate[out$area == "rural"], 1)
})

test_that("enemdu_tabulate_two_way creates row and column groups", {
  data <- tibble::tibble(
    area = c("urbano", "urbano", "rural", "rural"),
    sexo = c(1, 2, 1, 2),
    fexp = c(1, 2, 3, 4)
  )

  out <- enemdu_tabulate_two_way(
    data = data,
    row_var = "area",
    col_var = "sexo",
    statistic = "count"
  )

  expect_equal(nrow(out), 4)
  expect_true(all(c("area", "sexo") %in% names(out)))
  expect_equal(out$estimate[out$area == "rural" & out$sexo == 2], 4)
})

test_that("enemdu_check_quality flags low sample size", {
  data <- tibble::tibble(
    estimate = c(1, 2),
    unweighted_n = c(20L, 100L)
  )

  out <- enemdu_check_quality(data)

  expect_equal(out$quality_flag[1], "low_sample_size")
  expect_equal(out$quality_flag[2], "sample_size_ok_precision_not_evaluated")
})

test_that("enemdu_kpi_general computes basic KPIs", {
  data <- tibble::tibble(
    idhogar = c("h1", "h1", "h2"),
    fexp = c(10, 10, 20),
    ingtot_pc = c(100, 100, 50)
  )

  out <- enemdu_kpi_general(data)

  expect_s3_class(out, "enemdu_kpi_table")
  expect_true("conteo_personas" %in% out$indicator_id)
  expect_true("conteo_hogares" %in% out$indicator_id)
  expect_true("ingreso_percapita_familiar_promedio" %in% out$indicator_id)

  persons <- out$estimate[out$indicator_id == "conteo_personas"]
  expect_equal(persons, 40)

  households <- out$estimate[out$indicator_id == "conteo_hogares"]
  expect_equal(households, 30)
})

test_that("enemdu_kpi_general includes optional bonus KPIs when available", {
  data <- tibble::tibble(
    idhogar = c("h1", "h2"),
    fexp = c(10, 20),
    ingtot_pc = c(100, 50),
    bonos_optional_total = c(5, 10),
    bonos_optional_recibe = c(1, 1)
  )

  out <- enemdu_kpi_general(data)

  expect_true("transferencias_bonos_total_enemdu" %in% out$indicator_id)
  expect_true("bono_jgl_receptores" %in% out$indicator_id)

  total_bonus <- out$estimate[out$indicator_id == "transferencias_bonos_total_enemdu"]
  expect_equal(total_bonus, 250)
})

test_that("enemdu_kpi_households computes household KPIs", {
  data <- tibble::tibble(
    idhogar = c("h1", "h1", "h2"),
    fexp = c(10, 10, 20),
    ingtot = c(100, 100, 80),
    ingtot_pc = c(50, 50, 80)
  )

  out <- enemdu_kpi_households(data)

  expect_s3_class(out, "enemdu_kpi_table")
  expect_true("hogares_muestrales" %in% out$indicator_id)
  expect_true("hogares_estimados" %in% out$indicator_id)
  expect_true("tamano_promedio_hogar_muestral" %in% out$indicator_id)
})
