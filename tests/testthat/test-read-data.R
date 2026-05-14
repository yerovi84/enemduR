test_that("enemdu_read_data reads SAV files as the operational primary format", {
  path <- tempfile(fileext = ".sav")

  haven::write_sav(
    tibble::tibble(
      P03 = c(15, 30),
      UPM = c(1, 1),
      ESTRATO = c(10, 10),
      FEXP = c(100, 120)
    ),
    path
  )

  out <- enemdu_read_data(
    path = path,
    survey_type = "mensual",
    inform_scope = FALSE
  )

  expect_s3_class(out, "enemdu_tbl")
  expect_equal(attr(out, "survey_type"), "mensual")
  expect_equal(attr(out, "input_format"), "sav")
  expect_equal(attr(out, "design_vars"), c("upm", "estrato", "fexp"))
  expect_true(all(c("p03", "upm", "estrato", "fexp") %in% names(out)))
})

test_that("enemdu_read_data keeps DTA support for interoperability", {
  path <- tempfile(fileext = ".dta")

  haven::write_dta(
    tibble::tibble(
      P03 = c(15, 30),
      UPM = c(1, 1),
      ESTRATO = c(10, 10),
      FEXP = c(100, 120)
    ),
    path
  )

  out <- enemdu_read_data(
    path = path,
    survey_type = "trimestral",
    inform_scope = FALSE
  )

  expect_s3_class(out, "enemdu_tbl")
  expect_equal(attr(out, "survey_type"), "trimestral")
  expect_equal(attr(out, "input_format"), "dta")
  expect_true(all(c("p03", "upm", "estrato", "fexp") %in% names(out)))
})

test_that("enemdu_read_data reads comma-separated CSV files", {
  path <- tempfile(fileext = ".csv")

  writeLines(
    c(
      "P03,UPM,ESTRATO,FEXP",
      "15,1,10,100",
      "30,1,10,120"
    ),
    path,
    useBytes = TRUE
  )

  out <- enemdu_read_data(
    path = path,
    survey_type = "anual",
    inform_scope = FALSE
  )

  expect_s3_class(out, "enemdu_tbl")
  expect_equal(attr(out, "survey_type"), "anual")
  expect_equal(attr(out, "input_format"), "csv")
  expect_true(all(c("p03", "upm", "estrato", "fexp") %in% names(out)))
})

test_that("enemdu_read_data reads semicolon-separated CSV files", {
  path <- tempfile(fileext = ".csv")

  writeLines(
    c(
      "P03;UPM;ESTRATO;FEXP",
      "15;1;10;100",
      "30;1;10;120"
    ),
    path,
    useBytes = TRUE
  )

  out <- enemdu_read_data(
    path = path,
    survey_type = "mensual",
    inform_scope = FALSE
  )

  expect_s3_class(out, "enemdu_tbl")
  expect_equal(attr(out, "input_format"), "csv")
  expect_true(all(c("p03", "upm", "estrato", "fexp") %in% names(out)))
})

test_that("enemdu_read_data rejects unsupported formats with an explicit message", {
  path <- tempfile(fileext = ".xlsx")
  writeLines("not a supported microdata format", path)

  expect_error(
    enemdu_read_data(
      path = path,
      survey_type = "mensual",
      inform_scope = FALSE
    ),
    class = "enemdu_error_invalid_file_format"
  )
})
