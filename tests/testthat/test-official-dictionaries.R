test_that("official dictionary core registry loads with required columns", {
  registry <- enemdu_official_dictionary_core_registry()

  expect_s3_class(registry, "tbl_df")
  expect_gt(nrow(registry), 0)

  expect_true(all(c(
    "survey_type",
    "period",
    "dictionary_frequency",
    "dictionary_file_scope",
    "dictionary_file",
    "official_variable",
    "official_description",
    "package_expected_variable",
    "role",
    "required_core",
    "domain_level",
    "source_status",
    "notes"
  ) %in% names(registry)))

  expect_true(all(registry$survey_type %in% c("mensual", "trimestral", "anual")))
})

test_that("official dictionary validation covers id_hogar through canonical alias", {
  out <- enemdu_validate_official_dictionary_core(
    survey_type = "mensual",
    emit = FALSE
  )

  row <- out[out$official_variable == "id_hogar", , drop = FALSE]

  expect_equal(nrow(row), 1)
  expect_equal(row$package_variable, "idhogar")
  expect_equal(row$catalog_match_type, "alias")
  expect_equal(row$catalog_status, "covered_by_variable_catalog")
  expect_equal(row$validation_flag, "pass")
})

test_that("official dictionary validation keeps design variables covered", {
  out <- enemdu_validate_official_dictionary_core(
    survey_type = "trimestral",
    emit = FALSE
  )

  design_rows <- out[out$official_variable %in% c("upm", "estrato", "fexp"), , drop = FALSE]

  expect_equal(nrow(design_rows), 3)
  expect_true(all(design_rows$catalog_status == "covered_by_variable_catalog"))
  expect_true(all(design_rows$validation_flag == "pass"))
})

test_that("official dictionary validation recognizes annual province domain variable", {
  out <- enemdu_validate_official_dictionary_core(
    survey_type = "anual",
    emit = FALSE
  )

  row <- out[out$official_variable == "prov", , drop = FALSE]

  expect_equal(nrow(row), 1)
  expect_equal(row$domain_level, "provincia_24")
  expect_equal(row$domain_status, "covered_by_domain_registry")
  expect_equal(row$severity, "warning")
  expect_equal(row$validation_flag, "warning_domain_variable_not_in_variable_catalog")
})

test_that("official dictionary validation flags p78 bonus label mismatch for review", {
  out <- enemdu_validate_official_dictionary_core(
    emit = FALSE
  )

  p78_rows <- out[out$official_variable == "p78", , drop = FALSE]

  expect_gt(nrow(p78_rows), 0)
  expect_true(all(p78_rows$bonus_label_status == "possible_label_mismatch"))
  expect_true(all(p78_rows$validation_flag == "warning_optional_bonus_label_mismatch"))
  expect_true(all(p78_rows$severity == "warning"))
})

test_that("official dictionary validation has no error-level issues in current core registry", {
  out <- enemdu_validate_official_dictionary_core(
    emit = FALSE
  )

  expect_false(any(out$severity == "error"))
})
