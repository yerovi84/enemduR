test_that("optional bonus registry loads with expected columns", {
  registry <- enemdu_optional_bonus_registry()

  expect_true(nrow(registry) >= 2)

  expect_true(all(
    c(
      "variable",
      "condition_variable",
      "condition_yes_values",
      "condition_no_values",
      "output_variable",
      "recipient_variable",
      "bonus_label",
      "default_income_inclusion",
      "optional_income_inclusion",
      "scenario_income_inclusion",
      "missing_codes"
    ) %in% names(registry)
  ))

  expect_true(all(c("p76", "p78") %in% registry$variable))
  expect_true(all(c("p75", "p77") %in% registry$condition_variable))
})

test_that("social bonus variables are validated from receipt questions", {
  data <- tibble::tibble(
    idhogar = c("h1", "h1", "h2", "h2"),
    hsize = c(2L, 2L, 2L, 2L),
    ingrltot = c(100, 50, 80, 60),
    p75 = c(1, 2, 1, NA_real_),
    p76 = c(55, 999999, NA_real_, 40),
    p77 = c(2, 1, 1, 2),
    p78 = c(30, 45, NA_real_, 10)
  )

  out <- enemdu_build_optional_bonuses(data)

  expect_true(all(c(
    "bono_desarrollo_humano",
    "bono_desarrollo_humano_recibe",
    "bono_discapacidad",
    "bono_discapacidad_recibe",
    "bonos_sociales_total",
    "bonos_sociales_recibe",
    "bonos_scenario_add_total"
  ) %in% names(out)))

  expect_equal(out$bono_desarrollo_humano, c(55, 0, NA_real_, NA_real_))
  expect_equal(out$bono_desarrollo_humano_recibe, c(1L, 0L, 1L, NA_integer_))

  expect_equal(out$bono_discapacidad, c(0, 45, NA_real_, 0))
  expect_equal(out$bono_discapacidad_recibe, c(0L, 1L, 1L, 0L))

  expect_equal(out$bonos_sociales_total, c(55, 45, NA_real_, 0))
  expect_equal(out$bonos_sociales_recibe, c(1L, 1L, 1L, 0L))

  expect_equal(out$bonos_scenario_add_total, c(0, 45, NA_real_, 0))

  expect_true(out$bono_desarrollo_humano_amount_without_receipt_flag[4])
  expect_true(out$bono_discapacidad_amount_without_receipt_flag[4])
})

test_that("social bonus keeps raw source and audit flags", {
  data <- tibble::tibble(
    idhogar = c("h1", "h2"),
    hsize = c(1L, 1L),
    ingrltot = c(100, 100),
    p75 = c(1, 2),
    p76 = c(999999, 40),
    p77 = c(1, 2),
    p78 = c(20, 30)
  )

  out <- enemdu_build_optional_bonuses(
    data,
    keep_raw = TRUE,
    create_flags = TRUE
  )

  expect_true("bono_desarrollo_humano_raw" %in% names(out))
  expect_true("bono_desarrollo_humano_missing_flag" %in% names(out))
  expect_true(out$bono_desarrollo_humano_missing_flag[1])
  expect_equal(out$bono_desarrollo_humano_raw[1], 999999)

  expect_true("bono_discapacidad_amount_without_receipt_flag" %in% names(out))
  expect_true(out$bono_discapacidad_amount_without_receipt_flag[2])
})

test_that("scenario income adds only bonuses not included in base income", {
  data <- tibble::tibble(
    idhogar = c("h1", "h1", "h2"),
    hsize = c(2L, 2L, 1L),
    ingrltot = c(100, 50, 80),
    p75 = c(1, 1, 2),
    p76 = c(55, 55, NA_real_),
    p77 = c(1, 2, 1),
    p78 = c(30, NA_real_, 20)
  )

  out <- enemdu_build_optional_bonuses(data)

  expect_true("ingrltot_plus_optional_bonos" %in% names(out))
  expect_true("ingtot_plus_optional_bonos" %in% names(out))
  expect_true("ingtot_pc_plus_optional_bonos" %in% names(out))

  expect_equal(out$bonos_sociales_total, c(85, 55, 20))
  expect_equal(out$bonos_scenario_add_total, c(30, 0, 20))

  expect_equal(out$ingrltot_plus_optional_bonos, c(130, 50, 100))
  expect_equal(out$ingtot_plus_optional_bonos[1], 180)
  expect_equal(out$ingtot_pc_plus_optional_bonos[1], 90)
  expect_equal(out$ingtot_pc_plus_optional_bonos[3], 100)
})

test_that("scenario income becomes missing when added bonus amount is unknown for a receiver", {
  data <- tibble::tibble(
    idhogar = c("h1", "h2"),
    hsize = c(1L, 1L),
    ingrltot = c(100, 80),
    p75 = c(2, 2),
    p76 = c(NA_real_, NA_real_),
    p77 = c(1, 2),
    p78 = c(NA_real_, 30)
  )

  out <- enemdu_build_optional_bonuses(data)

  expect_true(is.na(out$bonos_scenario_add_total[1]))
  expect_true(is.na(out$ingrltot_plus_optional_bonos[1]))

  expect_equal(out$bonos_scenario_add_total[2], 0)
  expect_equal(out$ingrltot_plus_optional_bonos[2], 80)
})

test_that("optional bonus scenario can be used by poverty flags through income_var", {
  data <- tibble::tibble(
    idhogar = c("h1", "h1", "h2"),
    hsize = c(2L, 2L, 1L),
    ingrltot = c(40, 40, 80),
    p75 = c(2, 2, 2),
    p76 = c(NA_real_, NA_real_, NA_real_),
    p77 = c(1, 2, 1),
    p78 = c(20, NA_real_, 30)
  )

  out <- enemdu_build_optional_bonuses(data)

  poverty <- enemdu_build_poverty_flags(
    data = out,
    period = "2024-12",
    income_var = "ingtot_pc_plus_optional_bonos",
    mode = "manual",
    poverty_line = 100,
    extreme_poverty_line = 50,
    line_source = "Unit test manual line"
  )

  expect_true("pobre" %in% names(poverty))
  expect_true("expobre" %in% names(poverty))
})

test_that("social bonus function does not modify base income variables", {
  data <- tibble::tibble(
    idhogar = c("h1", "h1"),
    hsize = c(2L, 2L),
    ingrltot = c(100, 50),
    p75 = c(1, 1),
    p76 = c(55, 55),
    p77 = c(1, 1),
    p78 = c(30, 20)
  )

  out <- enemdu_build_optional_bonuses(data)

  expect_equal(out$ingrltot, c(100, 50))
  expect_equal(out$ingrltot_plus_optional_bonos, c(130, 70))
})

test_that("legacy discapacidad aliases are created for compatibility", {
  data <- tibble::tibble(
    idhogar = c("h1", "h2"),
    hsize = c(1L, 1L),
    ingrltot = c(100, 100),
    p75 = c(2, 2),
    p76 = c(NA_real_, NA_real_),
    p77 = c(1, 2),
    p78 = c(20, 30)
  )

  out <- enemdu_build_optional_bonuses(data)

  expect_true("bono_jgl" %in% names(out))
  expect_true("bono_jgl_recibe" %in% names(out))
  expect_equal(out$bono_jgl, out$bono_discapacidad)
  expect_equal(out$bono_jgl_recibe, out$bono_discapacidad_recibe)
})

test_that("social bonus KPI estimates all validated bonus indicators", {
  data <- tibble::tibble(
    idhogar = c("h1", "h1", "h2", "h2"),
    upm = c(1, 2, 3, 4),
    estrato = c(1, 1, 2, 2),
    fexp = c(1, 1, 1, 1),
    p75 = c(1, 2, 1, 2),
    p76 = c(55, NA_real_, 55, NA_real_),
    p77 = c(1, 2, 1, 2),
    p78 = c(20, NA_real_, 30, NA_real_)
  )

  out <- enemdu_kpi_social_bonuses(
    data = data,
    survey_type = "anual",
    sample_n_min = 1
  )

  expect_true("transferencias_bonos_total_enemdu" %in% out$indicator_id)
  expect_true("bono_desarrollo_humano_receptores" %in% out$indicator_id)
  expect_true("bono_discapacidad_receptores" %in% out$indicator_id)
  expect_true("bonos_scenario_add_total_enemdu" %in% out$indicator_id)
  expect_true("representativity_flag" %in% names(out))
})
