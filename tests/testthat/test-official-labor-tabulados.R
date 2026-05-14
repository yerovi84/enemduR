test_that("official monthly labor tabulations are parsed into long format", {
  tmp <- tempfile("official_labor_monthly_")
  dir.create(tmp)

  writeLines(
    c(
      ";;;;;",
      "Encuesta;Periodo;Indicadores;Nacional;Area;",
      ";;;Total;Urbana;Rural",
      "ENEMDU;dic-07;Poblacion en Edad de Trabajar (PET);9.309.490;6.343.085;2.966.404",
      "ENEMDU;dic-07;Poblacion Economicamente Activa;6.336.029;4.227.702;2.108.328"
    ),
    file.path(tmp, "1. Poblaciones.csv"),
    useBytes = TRUE
  )

  writeLines(
    c(
      ";;;;;",
      "Encuesta;Periodo;Indicadores;Nacional;Area;",
      ";;;Total;Urbana;Rural",
      "ENEMDU;dic-07;Empleo Global (%);95,0;93,9;97,2",
      "ENEMDU;dic-07;Participacion Global (%);68,1;66,7;71,1"
    ),
    file.path(tmp, "2. Tasas.csv"),
    useBytes = TRUE
  )

  out <- enemdu_read_official_labor_tabulados(
    path = tmp,
    survey_type = "mensual"
  )

  expect_s3_class(out, "enemdu_official_labor_tabulados")
  expect_true("labor_pet_total" %in% out$indicator_id)
  expect_true("labor_tasa_ocupacion_global" %in% out$indicator_id)

  pet <- out[
    out$indicator_id == "labor_pet_total" &
      out$domain_group == "Nacional" &
      out$domain_label == "Total",
    ,
    drop = FALSE
  ]

  empleo_global <- out[
    out$indicator_id == "labor_tasa_ocupacion_global" &
      out$domain_group == "Nacional" &
      out$domain_label == "Total",
    ,
    drop = FALSE
  ]

  expect_equal(pet$official_value_package_scale, 9309490)
  expect_equal(empleo_global$official_value_package_scale, 0.95)
})

test_that("official annual labor estimator tabulations are parsed", {
  tmp <- tempfile("official_labor_annual_")
  dir.create(tmp)

  labor_dir <- file.path(
    tmp,
    "00 ENEMDU_Anual_2025_Tabulados_Mercado_Laboral_CSV"
  )
  dir.create(labor_dir)

  writeLines(
    c(
      "1. Estimadores de indicadores laborales;;;;;",
      "Anual 2018 - 2025;;;;;",
      "Periodo;Indicador;Estimador;Nacional;Area;",
      ";;;Total;Urbano;Rural",
      "2025;Empleo Global;Indicador;96,5%;95,1%;98,0%",
      "2025;Empleo Global;Error estandar;0,2%;0,3%;0,4%"
    ),
    file.path(labor_dir, "1. Estimadores.csv"),
    useBytes = TRUE
  )

  out <- enemdu_read_official_labor_tabulados(
    path = tmp,
    survey_type = "anual"
  )

  indicator_row <- out[
    out$indicator_id == "labor_tasa_ocupacion_global" &
      out$official_measure == "Indicador" &
      out$domain_group == "Nacional" &
      out$domain_label == "Total",
    ,
    drop = FALSE
  ]

  expect_equal(nrow(indicator_row), 1)
  expect_equal(indicator_row$official_value_package_scale, 0.965)
})

test_that("labor estimates can be compared against official national tabulations", {
  official <- tibble::tibble(
    indicator_id = c(
      "labor_pet_total",
      "labor_tasa_ocupacion_global"
    ),
    period = c("dic-07", "dic-07"),
    domain_group = c("Nacional", "Nacional"),
    domain_label = c("Total", "Total"),
    official_indicator_label = c(
      "Poblacion en Edad de Trabajar (PET)",
      "Empleo Global (%)"
    ),
    official_measure = c("Indicador", "Indicador"),
    official_value_raw = c("9.309.490", "95,0"),
    official_value = c(9309490, 95),
    official_value_package_scale = c(9309490, 0.95),
    package_scale = c("count", "proportion_0_1")
  )

  estimates <- tibble::tibble(
    indicator_id = c(
      "labor_pet_total",
      "labor_tasa_ocupacion_global"
    ),
    estimate = c(9309490, 0.9501)
  )

  comparison <- enemdu_compare_labor_tabulados(
    estimates = estimates,
    official = official,
    official_period = "dic-07",
    tolerance_count = 1,
    tolerance_rate = 0.0005
  )

  expect_s3_class(comparison, "enemdu_labor_tabulados_comparison")
  expect_true(all(comparison$within_tolerance))
  expect_true(all(comparison$comparison_status == "match"))
})

test_that("labor comparison detects values outside tolerance", {
  official <- tibble::tibble(
    indicator_id = "labor_tasa_ocupacion_global",
    period = "dic-07",
    domain_group = "Nacional",
    domain_label = "Total",
    official_indicator_label = "Empleo Global (%)",
    official_measure = "Indicador",
    official_value_raw = "95,0",
    official_value = 95,
    official_value_package_scale = 0.95,
    package_scale = "proportion_0_1"
  )

  estimates <- tibble::tibble(
    indicator_id = "labor_tasa_ocupacion_global",
    estimate = 0.90
  )

  comparison <- enemdu_compare_labor_tabulados(
    estimates = estimates,
    official = official,
    official_period = "dic-07",
    tolerance_rate = 0.0005
  )

  expect_false(comparison$within_tolerance)
  expect_equal(comparison$comparison_status, "outside_tolerance")

  expect_error(
    enemdu_compare_labor_tabulados(
      estimates = estimates,
      official = official,
      official_period = "dic-07",
      tolerance_rate = 0.0005,
      strict = TRUE
    ),
    class = "enemdu_error_labor_tabulados_mismatch"
  )
})
