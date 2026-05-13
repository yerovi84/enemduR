test_that("optional bonus registry loads with expected columns", {
  registry <- enemdu_optional_bonus_registry()

  expect_true(nrow(registry) >= 1)

  expect_true(all(
    c(
      "variable",
      "output_variable",
      "recipient_variable",
      "bonus_label",
      "default_income_inclusion",
      "optional_income_inclusion",
      "missing_codes"
    ) %in% names(registry)
  ))
})

test_that("optional bonus variables are built from p78", {
  data <- tibble::tibble(
    idhogar = c("h1", "h1", "h2"),
    hsize = c(2L, 2L, 1L),
    ingrltot = c(100, 50, 80),
    p78 = c(30, NA_real_, 999999)
  )

  out <- enemdu_build_optional_bonuses(data)

  expect_true("bono_jgl" %in% names(out))
  expect_true("bono_jgl_recibe" %in% names(out))
  expect_true("bonos_optional_total" %in% names(out))
  expect_true("bonos_optional_recibe" %in% names(out))

  expect_equal(out$bono_jgl, c(30, 0, NA_real_))
  expect_equal(out$bono_jgl_recibe, c(1L, 0L, NA_integer_))
  expect_equal(out$bonos_optional_total, c(30, 0, NA_real_))
})

test_that("optional bonus keeps raw source and missing flags", {
  data <- tibble::tibble(
    idhogar = c("h1", "h2"),
    hsize = c(1L, 1L),
    ingrltot = c(100, 100),
    p78 = c(999999, 20)
  )

  out <- enemdu_build_optional_bonuses(
    data,
    keep_raw = TRUE,
    create_flags = TRUE
  )

  expect_true("bono_jgl_raw" %in% names(out))
  expect_true("bono_jgl_missing_flag" %in% names(out))
  expect_true(out$bono_jgl_missing_flag[1])
  expect_equal(out$bono_jgl_raw[1], 999999)
})

test_that("optional bonus scenario income is built", {
  data <- tibble::tibble(
    idhogar = c("h1", "h1", "h2"),
    hsize = c(2L, 2L, 1L),
    ingrltot = c(100, 50, 80),
    p78 = c(30, NA_real_, 20)
  )

  out <- enemdu_build_optional_bonuses(data)

  expect_true("ingrltot_plus_optional_bonos" %in% names(out))
  expect_true("ingtot_plus_optional_bonos" %in% names(out))
  expect_true("ingtot_pc_plus_optional_bonos" %in% names(out))

  expect_equal(out$ingrltot_plus_optional_bonos, c(130, 50, 100))
  expect_equal(out$ingtot_plus_optional_bonos[1], 180)
  expect_equal(out$ingtot_pc_plus_optional_bonos[1], 90)
  expect_equal(out$ingtot_pc_plus_optional_bonos[3], 100)
})

test_that("optional bonus scenario can be used by poverty flags through income_var", {
  data <- tibble::tibble(
    idhogar = c("h1", "h1", "h2"),
    hsize = c(2L, 2L, 1L),
    ingrltot = c(40, 40, 80),
    p78 = c(20, 0, 30)
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

test_that("optional bonus function does not modify base income variables", {
  data <- tibble::tibble(
    idhogar = c("h1", "h1"),
    hsize = c(2L, 2L),
    ingrltot = c(100, 50),
    p78 = c(30, 20)
  )

  out <- enemdu_build_optional_bonuses(data)

  expect_equal(out$ingrltot, c(100, 50))
  expect_equal(out$ingrltot_plus_optional_bonos, c(130, 70))
})
