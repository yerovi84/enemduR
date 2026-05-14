.test_bonus_pair_row <- function(out, pair_id) {
  out[!is.na(out$pair_id) & out$pair_id == pair_id, , drop = FALSE]
}

test_that("required microdata variables include survey-type domain variables", {
  mensual <- enemdu_required_microdata_variables(
    survey_type = "mensual",
    include_income_core = FALSE,
    include_social_bonuses = FALSE
  )

  trimestral <- enemdu_required_microdata_variables(
    survey_type = "trimestral",
    include_income_core = FALSE,
    include_social_bonuses = FALSE
  )

  anual <- enemdu_required_microdata_variables(
    survey_type = "anual",
    include_income_core = FALSE,
    include_social_bonuses = FALSE
  )

  expect_true("area" %in% mensual)
  expect_false("ciudad" %in% mensual)
  expect_true(all(c("area", "ciudad") %in% trimestral))
  expect_true(all(c("area", "ciudad", "prov") %in% anual))
})

test_that("microdata validation passes with complete current bonus pairs", {
  dictionary <- tibble::tibble(
    variable = c(
      "area", "ciudad", "prov", "id_hogar", "upm", "estrato", "fexp",
      "p63", "p64b", "p65", "p66", "p67", "p68b", "p69", "p70b",
      "p71a", "p71b", "p72a", "p72b", "p73a", "p73b", "p74a", "p74b",
      "p75", "p76", "p77", "p78"
    ),
    description = NA_character_
  )

  data <- tibble::tibble(
    area = c(1, 2),
    ciudad = c(170150, 90150),
    prov = c(17, 9),
    id_hogar = c("h1", "h2"),
    upm = c(1, 2),
    estrato = c(1, 1),
    fexp = c(1, 1),
    p63 = c(100, 200),
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
    p75 = c(1, 2),
    p76 = c(55, NA_real_),
    p77 = c(1, 2),
    p78 = c(20, NA_real_)
  )

  out <- enemdu_validate_microdata_against_dictionary(
    data = data,
    dictionary = dictionary,
    survey_type = "anual",
    emit = FALSE
  )

  pair_rows <- out[!is.na(out$check_type) & out$check_type == "social_bonus_pair", , drop = FALSE]

  expect_s3_class(out, "enemdu_microdata_dictionary_validation")
  expect_false(any(out$severity == "error"))
  expect_true(all(c("bono_desarrollo_humano", "bono_discapacidad") %in% out$pair_id))
  expect_true(all(pair_rows$validation_status == "pair_complete"))
})

test_that("microdata validation detects missing required design variable", {
  dictionary <- tibble::tibble(
    variable = c("area", "id_hogar", "upm", "estrato", "fexp", "p75", "p76", "p77", "p78"),
    description = NA_character_
  )

  data <- tibble::tibble(
    area = c(1, 2),
    id_hogar = c("h1", "h2"),
    upm = c(1, 2),
    estrato = c(1, 1),
    p75 = c(1, 2),
    p76 = c(55, NA_real_),
    p77 = c(1, 2),
    p78 = c(20, NA_real_)
  )

  out <- enemdu_validate_microdata_against_dictionary(
    data = data,
    dictionary = dictionary,
    survey_type = "mensual",
    include_income_core = FALSE,
    emit = FALSE
  )

  row <- out[out$variable == "fexp", , drop = FALSE]

  expect_equal(nrow(row), 1)
  expect_equal(row$validation_status, "missing_required_variable")
  expect_equal(row$severity, "error")
})

test_that("microdata validation detects incomplete social bonus pair", {
  dictionary <- tibble::tibble(
    variable = c("area", "id_hogar", "upm", "estrato", "fexp", "p75", "p76", "p78"),
    description = NA_character_
  )

  data <- tibble::tibble(
    area = c(1, 2),
    id_hogar = c("h1", "h2"),
    upm = c(1, 2),
    estrato = c(1, 1),
    fexp = c(1, 1),
    p75 = c(1, 2),
    p76 = c(55, NA_real_),
    p78 = c(20, NA_real_)
  )

  out <- enemdu_validate_microdata_against_dictionary(
    data = data,
    dictionary = dictionary,
    survey_type = "mensual",
    include_income_core = FALSE,
    emit = FALSE
  )

  row <- .test_bonus_pair_row(out, "bono_discapacidad")

  expect_equal(nrow(row), 1)
  expect_equal(row$validation_status, "missing_condition_variable")
  expect_equal(row$condition_variable, "p77")
  expect_equal(row$amount_variable, "p78")
  expect_equal(row$severity, "error")
})

test_that("microdata file validation reads dta files for structural validation", {
  skip_if_not_installed("haven")

  dictionary <- tibble::tibble(
    variable = c("area", "id_hogar", "upm", "estrato", "fexp", "p75", "p76", "p77", "p78"),
    description = NA_character_
  )

  data <- tibble::tibble(
    area = c(1, 2),
    id_hogar = c("h1", "h2"),
    upm = c(1, 2),
    estrato = c(1, 1),
    fexp = c(1, 1),
    p75 = c(1, 2),
    p76 = c(55, NA_real_),
    p77 = c(1, 2),
    p78 = c(20, NA_real_)
  )

  path <- tempfile(fileext = ".dta")
  haven::write_dta(data, path)

  out <- enemdu_validate_microdata_file_against_dictionary(
    path = path,
    dictionary = dictionary,
    survey_type = "mensual",
    include_income_core = FALSE,
    emit = FALSE
  )

  expect_false(any(out$severity == "error"))
  expect_true("p75/p76" %in% out$variable)
  expect_true("p77/p78" %in% out$variable)
})
