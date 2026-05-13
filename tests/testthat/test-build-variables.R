test_that("build_variables constructs hsize and income variables", {
  data <- tibble::tibble(
    idhogar = c("h1", "h1", "h2"),
    p63 = c(100, NA_real_, 50),
    p64b = c(20, NA_real_, 10),
    p65 = c(10, NA_real_, 5),
    p66 = c(NA_real_, 80, NA_real_),
    p67 = c(NA_real_, 5, NA_real_),
    p68b = c(NA_real_, NA_real_, NA_real_),
    p69 = c(10, NA_real_, NA_real_),
    p70b = c(NA_real_, NA_real_, NA_real_),
    p71a = c(2, 2, 2),
    p71b = c(NA_real_, NA_real_, NA_real_),
    p72a = c(2, 2, 2),
    p72b = c(NA_real_, NA_real_, NA_real_),
    p73a = c(2, 2, 2),
    p73b = c(NA_real_, NA_real_, NA_real_),
    p74a = c(2, 2, 2),
    p74b = c(NA_real_, NA_real_, NA_real_),
    p75 = c(2, 2, 2),
    p76 = c(NA_real_, NA_real_, NA_real_),
    p78 = c(NA_real_, NA_real_, NA_real_),
    fexp = c(1, 1, 1)
  )

  out <- enemdu_build_variables(data)

  expect_true(all(
    c("hsize", "ingr", "ingrls", "ingrl", "ingrltot", "ingtot", "ingtot_pc") %in% names(out)
  ))

  expect_equal(out$hsize, c(2L, 2L, 1L))

  expect_equal(out$ingr[1], 110)
  expect_equal(out$ingrls[1], 10)
  expect_equal(out$ingrl[1], 120)

  expect_equal(out$ingr[2], 85)
  expect_true(is.na(out$ingrls[2]))
  expect_equal(out$ingrl[2], 85)

  expect_equal(out$ingtot[1], out$ingtot[2])
  expect_equal(out$ingtot_pc[1], out$ingtot[1] / 2)
  expect_equal(out$ingtot_pc[2], out$ingtot[2] / 2)
})

test_that("build_variables normalizes income sentinel codes before deriving", {
  data <- tibble::tibble(
    idhogar = c("h1", "h1"),
    p63 = c(999999, 100),
    p64b = c(NA_real_, NA_real_),
    p65 = c(NA_real_, NA_real_),
    p66 = c(NA_real_, NA_real_),
    p67 = c(NA_real_, NA_real_),
    p68b = c(NA_real_, NA_real_),
    p69 = c(NA_real_, NA_real_),
    p70b = c(NA_real_, NA_real_),
    p71a = c(2, 2),
    p71b = c(NA_real_, NA_real_),
    p72a = c(2, 2),
    p72b = c(NA_real_, NA_real_),
    p73a = c(2, 2),
    p73b = c(NA_real_, NA_real_),
    p74a = c(2, 2),
    p74b = c(NA_real_, NA_real_),
    p75 = c(2, 2),
    p76 = c(NA_real_, NA_real_),
    p78 = c(NA_real_, NA_real_)
  )

  out <- enemdu_build_variables(data)

  expect_true(is.na(out$p63[1]))
  expect_true("p63_raw" %in% names(out))
  expect_equal(out$p63_raw[1], 999999)
  expect_true("p63_missing_flag" %in% names(out))

  expect_equal(out$ingrltot[2], 100)
  expect_equal(out$ingtot[1], 100)
  expect_equal(out$ingtot_pc[1], 50)
})

test_that("build_variables can use id_hogar alias", {
  data <- tibble::tibble(
    id_hogar = c("a", "a", "b"),
    p63 = c(10, 20, 30)
  )

  out <- enemdu_build_variables(
    data,
    missing_vars_absent = "warn_as_na"
  )

  expect_true("hsize" %in% names(out))
  expect_equal(out$hsize, c(2L, 2L, 1L))
})

test_that("build_variables backs up existing output variables by default", {
  data <- tibble::tibble(
    idhogar = c("h1", "h1"),
    ingrl = c(999, 999),
    p63 = c(100, 200)
  )

  out <- enemdu_build_variables(data)

  expect_true("ingrl_source" %in% names(out))
  expect_equal(out$ingrl_source, c(999, 999))
  expect_false(all(out$ingrl == c(999, 999)))
})

test_that("build_variables errors when existing output variables are protected", {
  data <- tibble::tibble(
    idhogar = c("h1", "h1"),
    ingrl = c(999, 999),
    p63 = c(100, 200)
  )

  expect_error(
    enemdu_build_variables(
      data,
      existing = "error"
    ),
    class = "enemdu_error_existing_output_vars"
  )
})

test_that("build_quintiles creates a quintile variable", {
  data <- tibble::tibble(
    ingtot_pc = c(10, 20, 30, 40, 50),
    fexp = c(1, 1, 1, 1, 1)
  )

  out <- enemdu_build_quintiles(data)

  expect_true("quintil_ingreso_pc" %in% names(out))
  expect_true(all(out$quintil_ingreso_pc %in% 1:5))
})

test_that("build_quintiles leaves invalid income as missing", {
  data <- tibble::tibble(
    ingtot_pc = c(10, NA_real_, 0, 40, 50),
    fexp = c(1, 1, 1, 1, 1)
  )

  out <- enemdu_build_quintiles(data)

  expect_true(is.na(out$quintil_ingreso_pc[2]))
  expect_true(is.na(out$quintil_ingreso_pc[3]))
})

test_that("build_household_profile creates one row per household", {
  data <- tibble::tibble(
    idhogar = c("h1", "h1", "h2"),
    fexp = c(1.5, 1.5, 2.0),
    ingtot = c(100, 100, 80),
    ingtot_pc = c(50, 50, 80)
  )

  profile <- enemdu_build_household_profile(data)

  expect_s3_class(profile, "enemdu_household_profile")
  expect_equal(nrow(profile), 2)
  expect_true(all(c("household_id", "hsize", "fexp_first", "ingtot", "ingtot_pc") %in% names(profile)))
  expect_equal(profile$hsize[profile$household_id == "h1"], 2L)
})
