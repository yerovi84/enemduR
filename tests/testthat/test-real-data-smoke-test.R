.test_smoke_dictionary <- function() {
  tibble::tibble(
    variable = c(
      "area", "id_hogar", "upm", "estrato", "fexp",
      "p63", "p64b", "p65", "p66", "p67", "p68b", "p69", "p70b",
      "p71a", "p71b", "p72a", "p72b", "p73a", "p73b", "p74a", "p74b",
      "p75", "p76", "p77", "p78"
    ),
    description = NA_character_
  )
}

.test_smoke_data <- function() {
  tibble::tibble(
    area = c(1, 1, 2, 2),
    id_hogar = c("h1", "h1", "h2", "h2"),
    upm = c(1, 2, 3, 4),
    estrato = c(1, 1, 2, 2),
    fexp = c(1, 1, 1, 1),
    p63 = c(100, 50, 80, 70),
    p64b = c(NA_real_, NA_real_, NA_real_, NA_real_),
    p65 = c(NA_real_, NA_real_, NA_real_, NA_real_),
    p66 = c(NA_real_, NA_real_, NA_real_, NA_real_),
    p67 = c(NA_real_, NA_real_, NA_real_, NA_real_),
    p68b = c(NA_real_, NA_real_, NA_real_, NA_real_),
    p69 = c(NA_real_, NA_real_, NA_real_, NA_real_),
    p70b = c(NA_real_, NA_real_, NA_real_, NA_real_),
    p71a = c(2, 2, 2, 2),
    p71b = c(NA_real_, NA_real_, NA_real_, NA_real_),
    p72a = c(2, 2, 2, 2),
    p72b = c(NA_real_, NA_real_, NA_real_, NA_real_),
    p73a = c(2, 2, 2, 2),
    p73b = c(NA_real_, NA_real_, NA_real_, NA_real_),
    p74a = c(2, 2, 2, 2),
    p74b = c(NA_real_, NA_real_, NA_real_, NA_real_),
    p75 = c(1, 2, 1, 2),
    p76 = c(55, NA_real_, 55, NA_real_),
    p77 = c(1, 2, 1, 2),
    p78 = c(20, NA_real_, 30, NA_real_)
  )
}

test_that("smoke test runs the minimal functional pipeline", {
  out <- enemdu_smoke_test_microdata(
    data = .test_smoke_data(),
    dictionary = .test_smoke_dictionary(),
    survey_type = "mensual",
    domain_group_vars = "area",
    sample_n_min = 1,
    emit = FALSE
  )

  expect_s3_class(out, "enemdu_smoke_test_result")
  expect_s3_class(out$summary, "tbl_df")
  expect_true(all(c(
    "microdata_dictionary_validation",
    "build_variables",
    "build_social_bonuses",
    "core_indicator_table",
    "domain_indicator_table",
    "social_bonus_kpis"
  ) %in% out$summary$step))

  expect_false(any(out$summary$status == "error"))
  expect_s3_class(out$core_indicators, "enemdu_indicator_table")
  expect_s3_class(out$domain_indicators, "enemdu_indicator_table")
  expect_true("transferencias_bonos_total_enemdu" %in% out$social_bonus_indicators$indicator_id)
})

test_that("smoke test can run without dictionary validation", {
  out <- enemdu_smoke_test_microdata(
    data = .test_smoke_data(),
    survey_type = "mensual",
    include_validation = FALSE,
    sample_n_min = 1,
    emit = FALSE
  )

  expect_s3_class(out, "enemdu_smoke_test_result")
  expect_false("microdata_dictionary_validation" %in% out$summary$step)
  expect_false(any(out$summary$status == "error"))
})

test_that("smoke test file interface reads SAV files", {
  skip_if_not_installed("haven")

  path <- tempfile(fileext = ".sav")
  haven::write_sav(.test_smoke_data(), path)

  out <- enemdu_smoke_test_microdata_file(
    path = path,
    dictionary = .test_smoke_dictionary(),
    survey_type = "mensual",
    n_max = 4,
    sample_n_min = 1,
    emit = FALSE
  )

  expect_s3_class(out, "enemdu_smoke_test_result")
  expect_false(any(out$summary$status == "error"))
  expect_true("build_variables" %in% out$summary$step)
})

test_that("smoke test reports validation errors as smoke errors", {
  dictionary <- .test_smoke_dictionary()
  data <- .test_smoke_data()
  data$fexp <- NULL

  out <- enemdu_smoke_test_microdata(
    data = data,
    dictionary = dictionary,
    survey_type = "mensual",
    include_build_variables = FALSE,
    include_social_bonuses = FALSE,
    include_core_indicators = FALSE,
    sample_n_min = 1,
    emit = FALSE
  )

  row <- out$summary[out$summary$step == "microdata_dictionary_validation", , drop = FALSE]

  expect_equal(nrow(row), 1)
  expect_equal(row$status, "error")
})
