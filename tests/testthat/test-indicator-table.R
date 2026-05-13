test_that("enemdu_indicator_table estimates one indicator without groups", {
  data <- tibble::tibble(
    idhogar = c("h1", "h1", "h2", "h2"),
    hsize = c(2L, 2L, 2L, 2L),
    upm = c(1, 2, 3, 4),
    estrato = c(1, 1, 2, 2),
    fexp = c(1, 1, 1, 1),
    ingtot_pc = c(10, 20, 30, 40)
  )

  out <- enemdu_indicator_table(
    data = data,
    indicator_id = "ingreso_percapita_familiar",
    survey_type = "anual",
    sample_n_min = 1
  )

  expect_s3_class(out, "enemdu_indicator_table")
  expect_equal(nrow(out), 1)
  expect_equal(out$table_status, "estimated")
  expect_equal(out$indicator_id, "ingreso_percapita_familiar")
  expect_equal(out$estimate, 25)
  expect_true("representativity_flag" %in% names(out))
  expect_true("table_id" %in% names(out))
})

test_that("enemdu_indicator_table estimates multiple indicators", {
  data <- tibble::tibble(
    idhogar = c("h1", "h1", "h2", "h2"),
    hsize = c(2L, 2L, 2L, 2L),
    upm = c(1, 2, 3, 4),
    estrato = c(1, 1, 2, 2),
    fexp = c(1, 1, 1, 1),
    ingtot_pc = c(10, 20, 30, 40),
    pobre = c(1, 0, 1, 1)
  )

  out <- enemdu_indicator_table(
    data = data,
    indicator_id = c("ingreso_percapita_familiar", "pobreza_ingresos"),
    survey_type = "anual",
    sample_n_min = 1
  )

  expect_s3_class(out, "enemdu_indicator_table")
  expect_equal(nrow(out), 2)
  expect_true(all(c("ingreso_percapita_familiar", "pobreza_ingresos") %in% out$indicator_id))
  expect_true(all(out$table_status == "estimated"))
  expect_equal(out$estimate[out$indicator_id == "ingreso_percapita_familiar"], 25)
  expect_equal(out$estimate[out$indicator_id == "pobreza_ingresos"], 0.75)
})

test_that("enemdu_indicator_table preserves grouped domain metadata", {
  data <- tibble::tibble(
    idhogar = c("h1", "h1", "h2", "h2"),
    hsize = c(2L, 2L, 2L, 2L),
    area = c("urbano", "urbano", "rural", "rural"),
    upm = c(1, 2, 3, 4),
    estrato = c(1, 1, 2, 2),
    fexp = c(1, 1, 1, 1),
    ingtot_pc = c(10, 20, 30, 40)
  )

  out <- enemdu_indicator_table(
    data = data,
    indicator_id = "ingreso_percapita_familiar",
    group_vars = "area",
    survey_type = "mensual",
    sample_n_min = 1
  )

  expect_equal(nrow(out), 2)
  expect_true("area" %in% names(out))
  expect_true(all(c("urbano", "rural") %in% out$area))
  expect_equal(out$group_vars[[1]], "area")
  expect_true(all(out$domain_is_design_domain))
  expect_equal(unique(out$domain_scope_flag), "design_domain")
})

test_that("enemdu_indicator_table marks analysis domains without hiding precision metadata", {
  data <- tibble::tibble(
    idhogar = c("h1", "h1", "h2", "h2"),
    hsize = c(2L, 2L, 2L, 2L),
    provincia = c("P1", "P1", "P2", "P2"),
    upm = c(1, 2, 3, 4),
    estrato = c(1, 1, 2, 2),
    fexp = c(1, 1, 1, 1),
    ingtot_pc = c(10, 20, 30, 40)
  )

  out <- enemdu_indicator_table(
    data = data,
    indicator_id = "ingreso_percapita_familiar",
    group_vars = "provincia",
    survey_type = "mensual",
    sample_n_min = 1
  )

  expect_equal(nrow(out), 2)
  expect_false(out$domain_is_design_domain[[1]])
  expect_equal(out$domain_scope_flag[[1]], "analysis_domain_requires_precision")
  expect_true("representativity_flag" %in% names(out))
})

test_that("enemdu_indicator_table controls unsupported estimator types", {
  data <- tibble::tibble(
    upm = c(1, 2, 3, 4),
    estrato = c(1, 1, 2, 2),
    fexp = c(1, 1, 1, 1),
    quintil_ingreso_pc = c(1, 2, 3, 4)
  )

  out <- enemdu_indicator_table(
    data = data,
    indicator_id = "quintil_ingreso_pc",
    survey_type = "anual",
    unsupported = "row"
  )

  expect_s3_class(out, "enemdu_indicator_table")
  expect_equal(nrow(out), 1)
  expect_equal(out$table_status, "unsupported_estimator_type")
  expect_equal(out$decision, "not_estimated")
  expect_true(is.na(out$estimate))
})

test_that("enemdu_indicator_table can skip unsupported indicators", {
  data <- tibble::tibble(
    idhogar = c("h1", "h1", "h2", "h2"),
    hsize = c(2L, 2L, 2L, 2L),
    upm = c(1, 2, 3, 4),
    estrato = c(1, 1, 2, 2),
    fexp = c(1, 1, 1, 1),
    ingtot_pc = c(10, 20, 30, 40),
    quintil_ingreso_pc = c(1, 2, 3, 4)
  )

  expect_warning(
    out <- enemdu_indicator_table(
      data = data,
      indicator_id = c("ingreso_percapita_familiar", "quintil_ingreso_pc"),
      survey_type = "anual",
      unsupported = "skip",
      sample_n_min = 1
    ),
    class = "enemdu_warning_unsupported_indicator_skipped"
  )

  expect_equal(nrow(out), 1)
  expect_equal(out$indicator_id, "ingreso_percapita_familiar")
  expect_equal(out$table_status, "estimated")
})

test_that("enemdu_survey_tabulate is a compatibility wrapper", {
  data <- tibble::tibble(
    idhogar = c("h1", "h1", "h2", "h2"),
    hsize = c(2L, 2L, 2L, 2L),
    upm = c(1, 2, 3, 4),
    estrato = c(1, 1, 2, 2),
    fexp = c(1, 1, 1, 1),
    ingtot_pc = c(10, 20, 30, 40)
  )

  out <- enemdu_survey_tabulate(
    data = data,
    indicator_id = "ingreso_percapita_familiar",
    survey_type = "anual",
    sample_n_min = 1
  )

  expect_s3_class(out, "enemdu_indicator_table")
  expect_equal(out$table_status, "estimated")
  expect_equal(out$estimate, 25)
})
