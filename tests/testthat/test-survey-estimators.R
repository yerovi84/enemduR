test_that("enemdu_survey_mean computes a design-based mean", {
  data <- tibble::tibble(
    upm = c(1, 2, 3, 4),
    estrato = c(1, 1, 2, 2),
    fexp = c(1, 1, 1, 1),
    ingtot_pc = c(10, 20, 30, 40)
  )

  out <- enemdu_survey_mean(
    data = data,
    value = "ingtot_pc",
    sample_n_min = 1
  )

  expect_s3_class(out, "enemdu_survey_estimate")
  expect_equal(nrow(out), 1)
  expect_equal(out$estimate, 25)
  expect_true("standard_error" %in% names(out))
  expect_true("cv" %in% names(out))
  expect_true("degrees_freedom" %in% names(out))
  expect_true("decision" %in% names(out))
})

test_that("enemdu_survey_total computes a design-based total", {
  data <- tibble::tibble(
    upm = c(1, 2, 3, 4),
    estrato = c(1, 1, 2, 2),
    fexp = c(1, 1, 1, 1),
    bonos_optional_total = c(5, 10, 15, 20)
  )

  out <- enemdu_survey_total(
    data = data,
    value = "bonos_optional_total",
    sample_n_min = 1
  )

  expect_s3_class(out, "enemdu_survey_estimate")
  expect_equal(nrow(out), 1)
  expect_equal(out$estimate, 50)
})

test_that("enemdu_survey_proportion computes a design-based proportion", {
  data <- tibble::tibble(
    upm = c(1, 2, 3, 4),
    estrato = c(1, 1, 2, 2),
    fexp = c(1, 1, 1, 1),
    pobre = c(1, 0, 1, 1)
  )

  out <- enemdu_survey_proportion(
    data = data,
    value = "pobre",
    sample_n_min = 1
  )

  expect_s3_class(out, "enemdu_survey_estimate")
  expect_equal(nrow(out), 1)
  expect_equal(out$estimate, 0.75)
})

test_that("enemdu_survey_proportion fails for non-binary values", {
  data <- tibble::tibble(
    upm = c(1, 2, 3, 4),
    estrato = c(1, 1, 2, 2),
    fexp = c(1, 1, 1, 1),
    pobre = c(1, 0, 2, 1)
  )

  expect_error(
    enemdu_survey_proportion(
      data = data,
      value = "pobre"
    ),
    class = "enemdu_error_non_binary_proportion"
  )
})

test_that("survey estimates can be grouped", {
  data <- tibble::tibble(
    upm = c(1, 2, 3, 4),
    estrato = c(1, 1, 2, 2),
    fexp = c(1, 1, 1, 1),
    area = c("urbano", "urbano", "rural", "rural"),
    ingtot_pc = c(10, 20, 30, 40)
  )

  out <- enemdu_survey_mean(
    data = data,
    value = "ingtot_pc",
    group_vars = "area",
    sample_n_min = 1
  )

  expect_s3_class(out, "enemdu_survey_estimate")
  expect_equal(nrow(out), 2)
  expect_true("area" %in% names(out))
  expect_equal(out$estimate[out$area == "urbano"], 15)
  expect_equal(out$estimate[out$area == "rural"], 35)
})

test_that("survey estimates include survey type scope when available", {
  data <- tibble::tibble(
    upm = c(1, 2, 3, 4),
    estrato = c(1, 1, 2, 2),
    fexp = c(1, 1, 1, 1),
    ingtot_pc = c(10, 20, 30, 40)
  )

  attr(data, "survey_type") <- "anual"

  out <- enemdu_survey_mean(
    data = data,
    value = "ingtot_pc",
    sample_n_min = 1
  )

  expect_equal(out$survey_type, "anual")
  expect_true(grepl("24 provincias", out$design_domains))
})

test_that("survey estimators handle missing values by excluding them", {
  data <- tibble::tibble(
    upm = c(1, 2, 3, 4),
    estrato = c(1, 1, 2, 2),
    fexp = c(1, 1, 1, 1),
    ingtot_pc = c(10, NA_real_, 30, 40)
  )

  out <- enemdu_survey_mean(
    data = data,
    value = "ingtot_pc",
    sample_n_min = 1
  )

  expect_equal(out$unweighted_n, 3L)
  expect_equal(out$weighted_n, 3)
  expect_equal(out$estimate, mean(c(10, 30, 40)))
})

test_that("survey estimators fail clearly when design variables are missing", {
  data <- tibble::tibble(
    fexp = c(1, 1, 1),
    ingtot_pc = c(10, 20, 30)
  )

  expect_error(
    enemdu_survey_mean(
      data = data,
      value = "ingtot_pc"
    ),
    class = "enemdu_error_missing_vars"
  )
})
