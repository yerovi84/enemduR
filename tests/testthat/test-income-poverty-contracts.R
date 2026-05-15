.contract_valid_poverty_registry <- function() {
  tibble::tibble(
    period = c("2024-12", "2024-12"),
    period_type = c("monthly", "monthly"),
    line_type = c("poverty", "extreme_poverty"),
    line_value = c(100, 50),
    currency = c("USD", "USD"),
    ipc_value = c(NA_real_, NA_real_),
    base_line_value = c(NA_real_, 31.92),
    base_period = c("ECV_2006", "ECV_2006"),
    update_method = c("official_line", "official_line"),
    source_status = c("official", "official"),
    source_note = c("Synthetic contract-test source", "Synthetic contract-test source"),
    valid_from = c("2024-12", "2024-12"),
    valid_to = c("2024-12", "2024-12"),
    notes = c("Contract-test row", "Contract-test row")
  )
}

test_that("income derivation creates current income variables without poverty classification", {
  data <- tibble::tibble(
    idhogar = c("h1", "h1", "h2"),
    p63 = c(100, 50, 80)
  )

  expect_warning(
    out <- enemdu_build_variables(data),
    class = "enemdu_warning_absent_income_vars"
  )

  expect_true(all(c(
    "hsize",
    "ingr",
    "ingrls",
    "ingrl",
    "ingrltot",
    "ingtot",
    "ingtot_pc"
  ) %in% names(out)))

  expect_equal(out$hsize, c(2L, 2L, 1L))
  expect_equal(out$ingtot_pc[out$idhogar == "h1"], c(75, 75))
  expect_equal(out$ingtot_pc[out$idhogar == "h2"], 80)

  expect_false(any(c("pobre", "expobre", "linea_pobreza", "linea_pobreza_extrema") %in% names(out)))

  metadata <- attr(out, "income_derivation")
  expect_true(is.list(metadata))
  expect_true("p64b" %in% metadata$absent_income_vars)
  expect_true(grepl("Poverty flags are intentionally not derived", metadata$note))
})

test_that("quintiles use current weighted behavior and do not require poverty lines", {
  data <- tibble::tibble(
    ingtot_pc = c(10, 20, 30, 40, 50),
    fexp = c(1, 1, 1, 1, 6)
  )

  out <- enemdu_build_quintiles(data)

  expect_equal(out$quintil_ingreso_pc, c(1L, 2L, 2L, 3L, 4L))
  expect_false(any(c("pobre", "expobre", "linea_pobreza", "linea_pobreza_extrema") %in% names(out)))

  metadata <- attr(out, "quintile_derivation")
  expect_equal(metadata$income_var, "ingtot_pc")
  expect_equal(metadata$weight, "fexp")
  expect_true(metadata$use_weights)
})

test_that("household profile preserves household-level interpretation without extra adjustment", {
  data <- tibble::tibble(
    idhogar = c("h1", "h1", "h2"),
    fexp = c(10, 20, 30),
    ingtot = c(200, 200, 90),
    ingtot_pc = c(100, 100, 90)
  )

  profile <- enemdu_build_household_profile(data)

  expect_s3_class(profile, "enemdu_household_profile")
  expect_equal(nrow(profile), 2)

  h1 <- profile[profile$household_id == "h1", , drop = FALSE]
  h2 <- profile[profile$household_id == "h2", , drop = FALSE]

  expect_equal(h1$hsize, 2L)
  expect_equal(h1$fexp_first, 10)
  expect_equal(h1$ingtot, 200)
  expect_equal(h1$ingtot_pc, 100)
  expect_equal(h2$hsize, 1L)
  expect_equal(h2$ingtot_pc, 90)
})

test_that("poverty-line contracts require auditable operational metadata", {
  registry <- enemdu_poverty_line_registry()

  expect_true(all(c(
    "period",
    "period_type",
    "line_type",
    "line_value",
    "currency",
    "source_status",
    "source_note"
  ) %in% names(registry)))

  default_validation <- enemdu_validate_poverty_lines(registry, require_valid = TRUE)
  expect_s3_class(default_validation, "enemdu_poverty_line_validation")
  expect_true(any(default_validation$status == "fail"))

  invalid_registry <- tibble::tibble(
    period = "2024-12",
    period_type = "monthly",
    line_type = "poverty",
    line_value = 100,
    currency = "USD",
    ipc_value = NA_real_,
    base_line_value = NA_real_,
    base_period = "ECV_2006",
    update_method = "official_line",
    source_status = "pending_review",
    source_note = "",
    valid_from = "2024-12",
    valid_to = "2024-12",
    notes = "Invalid contract-test row"
  )

  invalid_validation <- enemdu_validate_poverty_lines(invalid_registry, require_valid = TRUE)
  expect_equal(invalid_validation$status, "fail")
  expect_true(grepl("source_note", invalid_validation$message))
  expect_true(grepl("pending_review", invalid_validation$message))

  valid_line <- enemdu_get_poverty_line(
    period = "2024-12",
    line_type = "poverty",
    registry = .contract_valid_poverty_registry()
  )

  expect_s3_class(valid_line, "enemdu_poverty_line")
  expect_equal(valid_line$line_value, 100)
  expect_equal(valid_line$source_status, "official")
})

test_that("poverty flags are computed only from explicit poverty-line inputs", {
  data <- tibble::tibble(
    ingtot_pc = c(40, 75, 125, NA_real_, 0)
  )

  expect_error(
    enemdu_build_poverty_flags(
      data = data,
      period = "2024-12",
      mode = "strict"
    ),
    class = "enemdu_error_missing_poverty_line"
  )

  diagnostic <- enemdu_build_poverty_flags(data, mode = "diagnostic_only")
  expect_false(any(c("pobre", "expobre") %in% names(diagnostic)))
  expect_true(is.data.frame(attr(diagnostic, "poverty_input_report")))

  out <- enemdu_build_poverty_flags(
    data = data,
    period = "2024-12",
    mode = "manual",
    poverty_line = 100,
    extreme_poverty_line = 50,
    line_source = "Synthetic contract-test poverty lines"
  )

  expect_equal(out$pobre, c(1L, 1L, 0L, NA_integer_, NA_integer_))
  expect_equal(out$expobre, c(1L, 0L, 0L, NA_integer_, NA_integer_))
  expect_true(all(c("linea_pobreza", "linea_pobreza_extrema") %in% names(out)))
  expect_equal(unique(out$linea_pobreza), 100)
  expect_equal(unique(out$linea_pobreza_extrema), 50)
})

test_that("optional bonus scenario excludes p76 from scenario additions and allows p78 by contract", {
  registry <- enemdu_optional_bonus_registry()

  p76_rule <- registry[registry$variable == "p76", , drop = FALSE]
  p78_rule <- registry[registry$variable == "p78", , drop = FALSE]

  expect_true(p76_rule$default_income_inclusion)
  expect_false(p76_rule$scenario_income_inclusion)
  expect_false(p78_rule$default_income_inclusion)
  expect_true(p78_rule$scenario_income_inclusion)

  data <- tibble::tibble(
    idhogar = c("h1", "h1", "h2"),
    hsize = c(2L, 2L, 1L),
    ingrltot = c(155, 100, 80),
    p75 = c(1, 2, 2),
    p76 = c(55, NA_real_, NA_real_),
    p77 = c(1, 2, 1),
    p78 = c(30, NA_real_, 20)
  )

  out <- enemdu_build_optional_bonuses(data)

  expect_equal(out$bono_desarrollo_humano, c(55, 0, 0))
  expect_equal(out$bono_discapacidad, c(30, 0, 20))
  expect_equal(out$bonos_sociales_total, c(85, 0, 20))
  expect_equal(out$bonos_scenario_add_total, c(30, 0, 20))

  expect_equal(out$ingrltot, data$ingrltot)
  expect_equal(out$ingrltot_plus_optional_bonos, c(185, 100, 100))
  expect_false(out$ingrltot_plus_optional_bonos[1] == data$ingrltot[1] + out$bonos_sociales_total[1])
})

test_that("income and social-bonus KPI helpers use existing documented indicators only", {
  income_data <- tibble::tibble(
    idhogar = c("h1", "h1", "h2", "h2"),
    hsize = c(2L, 2L, 2L, 2L),
    upm = c(1, 2, 3, 4),
    estrato = c(1, 1, 2, 2),
    fexp = c(1, 1, 1, 1),
    ingtot_pc = c(10, 20, 30, 40),
    ingrl = c(5, 15, 25, 35)
  )

  income_kpis <- enemdu_kpi_income(
    data = income_data,
    survey_type = "anual",
    sample_n_min = 1
  )

  expect_true(all(c("ingreso_percapita_familiar", "ingreso_laboral") %in% income_kpis$indicator_id))
  expect_false(any(grepl("pobreza", income_kpis$indicator_id)))

  bonus_data <- tibble::tibble(
    idhogar = c("h1", "h1", "h2", "h2"),
    upm = c(1, 2, 3, 4),
    estrato = c(1, 1, 2, 2),
    fexp = c(1, 1, 1, 1),
    p75 = c(1, 2, 1, 2),
    p76 = c(55, NA_real_, 55, NA_real_),
    p77 = c(1, 2, 1, 2),
    p78 = c(20, NA_real_, 30, NA_real_)
  )

  bonus_kpis <- enemdu_kpi_social_bonuses(
    data = bonus_data,
    survey_type = "anual",
    sample_n_min = 1
  )

  expect_true(all(c(
    "transferencias_bonos_total_enemdu",
    "bono_desarrollo_humano_receptores",
    "bono_discapacidad_receptores",
    "bonos_scenario_add_total_enemdu"
  ) %in% bonus_kpis$indicator_id))
  expect_true("representativity_flag" %in% names(bonus_kpis))
})

test_that("registries encode current income and poverty contracts without global sentinel recoding", {
  income_registry <- enemdu_income_component_registry()
  expect_true(all(c("stage", "output_variable", "input_variable", "operation") %in% names(income_registry)))
  expect_true(all(c("household_income", "percapita_income") %in% income_registry$stage))

  poverty_registry <- enemdu_poverty_line_registry()
  expect_true(all(c("poverty", "extreme_poverty") %in% poverty_registry$line_type))

  bonus_registry <- enemdu_optional_bonus_registry()
  expect_true(all(c("default_income_inclusion", "scenario_income_inclusion") %in% names(bonus_registry)))

  missing_registry <- enemdu_missing_code_registry()
  p78_rows <- missing_registry[missing_registry$variable == "p78", , drop = FALSE]
  expect_true(nrow(p78_rows) > 0)
  expect_true(all(p78_rows$action == "diagnostic_only"))

  indicator_registry <- enemdu_indicator_registry()
  contract_indicators <- c(
    "ingreso_percapita_familiar",
    "pobreza_ingresos",
    "pobreza_extrema_ingresos",
    "quintil_ingreso_pc",
    "ingreso_percapita_familiar_plus_optional_bonos",
    "pobreza_ingresos_plus_optional_bonos",
    "pobreza_extrema_ingresos_plus_optional_bonos"
  )
  rows <- indicator_registry[indicator_registry$indicator_id %in% contract_indicators, , drop = FALSE]

  expect_equal(nrow(rows), length(contract_indicators))
  expect_true(all(rows$scale_adjustment_required == "TRUE"))
  expect_equal(rows$estimator_type[rows$indicator_id == "quintil_ingreso_pc"], "other")
})
