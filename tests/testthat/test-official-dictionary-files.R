test_that("official dictionary table parser extracts variable rows", {
  raw <- tibble::tibble(
    col1 = c(
      "Institución",
      "Identificador",
      "Nombre",
      "Descripción",
      "Nombre del campo",
      "area",
      "ciudad",
      "p75",
      "p76"
    ),
    col2 = c(
      "Instituto Nacional de Estadistica y Censos",
      "INEC_PDA_2026",
      "enemdu_persona_2026_03",
      "Diccionario sintético",
      "Descripción del campo",
      "Area",
      "Ciudad",
      "Recibió el BONO DE DESARROLLO HUMANO",
      "Monto que recibió por el BONO DE DESARROLLO HUMANO"
    )
  )

  out <- .enemdu_tidy_official_dictionary_table(
    raw = raw,
    source_file = "Diccionario de Datos_persona_2026_03.ODS",
    source_sheet = "1",
    survey_type = "mensual",
    period = "2026-03",
    dictionary_frequency = "Mensual",
    dictionary_file_scope = "persona"
  )

  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 4)
  expect_equal(out$variable, c("area", "ciudad", "p75", "p76"))
  expect_equal(out$description[out$variable == "p76"], "Monto que recibió por el BONO DE DESARROLLO HUMANO")
  expect_equal(unique(out$survey_type), "mensual")
  expect_equal(unique(out$dictionary_file_scope), "persona")
})

test_that("dictionary reader backend dispatches by extension", {
  expect_equal(
    .enemdu_dictionary_reader_backend("Diccionario de Datos_persona_2026_03.ODS"),
    "readODS"
  )

  expect_equal(
    .enemdu_dictionary_reader_backend("Diccionario de Datos_persona_2026_I_trimestre.ods"),
    "readODS"
  )

  expect_equal(
    .enemdu_dictionary_reader_backend("Diccionario de Datos_persona_anual_2025.xlsx"),
    "readxl"
  )

  expect_equal(
    .enemdu_dictionary_file_extension("Diccionario de Datos_persona_2026_03.ODS"),
    "ods"
  )
})

test_that("data validation detects required variables missing from data", {
  dictionary <- tibble::tibble(
    variable = c("area", "ciudad", "upm", "estrato", "fexp", "p75", "p76"),
    description = c("Area", "Ciudad", "UPM", "Estrato", "Factor", "Recibe BDH", "Monto BDH")
  )

  data <- tibble::tibble(
    area = c(1, 2),
    ciudad = c(170150, 90150),
    upm = c(1, 2),
    estrato = c(1, 1),
    p75 = c(1, 2)
  )

  out <- enemdu_validate_data_against_official_dictionary(
    data = data,
    dictionary = dictionary,
    required_vars = c("area", "ciudad", "upm", "estrato", "fexp")
  )

  row <- out[out$variable == "fexp", , drop = FALSE]

  expect_equal(nrow(row), 1)
  expect_equal(row$validation_status, "missing_required_variable")
  expect_equal(row$severity, "error")
})

test_that("data validation can flag extra variables when extras are not allowed", {
  dictionary <- tibble::tibble(
    variable = c("area", "ciudad"),
    description = c("Area", "Ciudad")
  )

  data <- tibble::tibble(
    area = c(1, 2),
    ciudad = c(170150, 90150),
    variable_extra = c(1, 1)
  )

  out <- enemdu_validate_data_against_official_dictionary(
    data = data,
    dictionary = dictionary,
    allow_extra = FALSE
  )

  row <- out[out$variable == "variable_extra", , drop = FALSE]

  expect_equal(nrow(row), 1)
  expect_equal(row$validation_status, "extra_data_variable_not_in_dictionary")
  expect_equal(row$severity, "warning")
})

test_that("catalog validation recognizes official dictionary aliases", {
  dictionary <- tibble::tibble(
    variable = c("id_hogar", "area", "ciudad", "upm", "estrato", "fexp"),
    description = c("Identificador hogar", "Area", "Ciudad", "UPM", "Estrato", "Factor")
  )

  catalog <- tibble::tibble(
    variable = c("idhogar", "area", "ciudad", "upm", "estrato", "fexp"),
    aliases = c("id_hogar", "urbano_rural", "", "", "", ""),
    required_core = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE)
  )

  out <- enemdu_validate_catalog_against_official_dictionary(
    dictionary = dictionary,
    variable_catalog = catalog,
    required_core_only = TRUE
  )

  row <- out[out$variable == "idhogar", , drop = FALSE]

  expect_equal(nrow(row), 1)
  expect_equal(row$matched_dictionary_variable, "id_hogar")
  expect_equal(row$matched_by, "alias")
  expect_equal(row$validation_status, "covered_by_official_dictionary")
  expect_equal(row$severity, "ok")
})

test_that("dictionary file name inference works for current official naming patterns", {
  expect_equal(
    .enemdu_infer_survey_type_from_dictionary_file("Diccionario de Datos_persona_2026_03.ODS"),
    "mensual"
  )

  expect_equal(
    .enemdu_infer_period_from_dictionary_file("Diccionario de Datos_persona_2026_03.ODS"),
    "2026-03"
  )

  expect_equal(
    .enemdu_infer_survey_type_from_dictionary_file("Diccionario de Datos_persona_2026_I_trimestre.ods"),
    "trimestral"
  )

  expect_equal(
    .enemdu_infer_period_from_dictionary_file("Diccionario de Datos_persona_2026_I_trimestre.ods"),
    "2026-I"
  )

  expect_equal(
    .enemdu_infer_survey_type_from_dictionary_file("Diccionario de Datos_persona_anual_2025.xlsx"),
    "anual"
  )

  expect_equal(
    .enemdu_infer_period_from_dictionary_file("Diccionario de Datos_persona_anual_2025.xlsx"),
    "2025"
  )
})
