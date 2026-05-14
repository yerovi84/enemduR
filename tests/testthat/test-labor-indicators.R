test_that("enemdu_build_labor_flags builds official condact flags", {
  data <- tibble::tibble(
    p03 = c(10, 16, 20, 21, 22, 30, 35, 40, 45, 50),
    condact = 0:9,
    secemp = c(NA, 1, 2, 3, 4, 1, 2, NA, NA, NA)
  )

  out <- enemdu_build_labor_flags(data)

  expect_equal(out$labor_menor_15, c(1L, rep(0L, 9)))
  expect_equal(out$labor_pet, c(0L, rep(1L, 9)))
  expect_equal(out$labor_pea, c(0L, rep(1L, 8), 0L))
  expect_equal(out$labor_pei, c(rep(0L, 9), 1L))
  expect_equal(out$labor_empleo, c(0L, rep(1L, 6), 0L, 0L, 0L))
  expect_equal(out$labor_subempleo, c(0L, 0L, 1L, 1L, rep(0L, 6)))
  expect_equal(out$labor_subempleo_tiempo, c(0L, 0L, 1L, rep(0L, 7)))
  expect_equal(out$labor_subempleo_ingresos, c(0L, 0L, 0L, 1L, rep(0L, 6)))
  expect_equal(out$labor_desempleo, c(rep(0L, 7), 1L, 1L, 0L))
  expect_equal(out$labor_sector_formal, c(NA, 1L, 0L, 0L, 0L, 1L, 0L, NA, NA, NA))
  expect_equal(out$labor_sector_informal, c(NA, 0L, 1L, 0L, 0L, 0L, 1L, NA, NA, NA))
})

test_that("enemdu_build_labor_flags rejects invalid condact codes in strict mode", {
  data <- tibble::tibble(
    p03 = c(20, 30),
    condact = c(1, 99)
  )

  expect_error(
    enemdu_build_labor_flags(data, strict = TRUE),
    class = "enemdu_error_invalid_labor_code"
  )

  expect_s3_class(
    enemdu_build_labor_flags(data, strict = FALSE),
    "tbl_df"
  )
})

test_that("enemdu_kpi_employment estimates labor totals and rates with computable synthetic design", {
  data <- tibble::tibble(
    p03 = rep(20, 20),
    condact = c(
      1, 2, 7, 9,
      1, 3, 4, 8,
      5, 6, 2, 9,
      3, 4, 5, 7,
      6, 1, 8, 9
    ),
    secemp = c(
      1, 2, NA, NA,
      1, 2, 3, NA,
      4, 1, 2, NA,
      2, 3, 4, NA,
      1, 2, NA, NA
    ),
    upm = seq_len(20),
    estrato = rep(1:5, each = 4),
    fexp = rep(1, 20)
  )

  out <- enemdu_kpi_employment(
    data = data,
    survey_type = "mensual",
    sample_n_min = 1
  )

  expect_s3_class(out, "enemdu_employment_kpi")
  expect_true("labor_pet_total" %in% out$indicator_id)
  expect_true("labor_pea_total" %in% out$indicator_id)
  expect_true("labor_tasa_ocupacion_global" %in% out$indicator_id)
  expect_true("labor_tasa_empleo_adecuado" %in% out$indicator_id)
  expect_true("standard_error" %in% names(out))

  pet <- out$estimate[out$indicator_id == "labor_pet_total"]
  pea <- out$estimate[out$indicator_id == "labor_pea_total"]
  employment_total <- out$estimate[out$indicator_id == "labor_empleo_total"]
  adequate_total <- out$estimate[out$indicator_id == "labor_empleo_adecuado_total"]
  employment_rate <- out$estimate[out$indicator_id == "labor_tasa_ocupacion_global"]
  adequate_rate <- out$estimate[out$indicator_id == "labor_tasa_empleo_adecuado"]

  expect_equal(as.numeric(pet), 20)
  expect_equal(as.numeric(pea), 17)
  expect_equal(as.numeric(employment_total), 13)
  expect_equal(as.numeric(adequate_total), 3)
  expect_equal(as.numeric(employment_rate), 13 / 17, tolerance = 1e-8)
  expect_equal(as.numeric(adequate_rate), 3 / 17, tolerance = 1e-8)
})

test_that("enemdu_kpi_employment can group estimates with computable synthetic domains", {
  data <- tibble::tibble(
    area = c(
      "urbano", "urbano", "urbano", "urbano",
      "rural", "rural", "rural", "rural"
    ),
    p03 = rep(20, 8),
    condact = c(1, 2, 7, 9, 1, 8, 9, 9),
    upm = seq_len(8),
    estrato = c(1, 1, 2, 2, 3, 3, 4, 4),
    fexp = rep(1, 8)
  )

  out <- enemdu_kpi_employment(
    data = data,
    group_vars = "area",
    survey_type = "mensual",
    include_rates = FALSE,
    sample_n_min = 1
  )

  expect_true("area" %in% names(out))

  pea_urbano <- out$estimate[out$indicator_id == "labor_pea_total" & out$area == "urbano"]
  pea_rural <- out$estimate[out$indicator_id == "labor_pea_total" & out$area == "rural"]

  expect_equal(as.numeric(pea_urbano), 3)
  expect_equal(as.numeric(pea_rural), 2)
})
