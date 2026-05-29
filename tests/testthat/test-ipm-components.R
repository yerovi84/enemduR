.ipm_components_repo_path <- function(...) {
  repo_path <- file.path(...)
  if (file.exists(repo_path)) {
    return(repo_path)
  }

  test_path <- file.path("..", "..", ...)
  if (file.exists(test_path)) {
    return(test_path)
  }

  repo_path
}

.ipm_components_extdata_path <- function(file) {
  repo_path <- file.path("inst", "extdata", file)
  if (file.exists(repo_path)) {
    return(repo_path)
  }

  test_path <- file.path("..", "..", "inst", "extdata", file)
  if (file.exists(test_path)) {
    return(test_path)
  }

  installed_path <- system.file("extdata", file, package = "enemduR")
  if (nzchar(installed_path)) {
    return(installed_path)
  }

  repo_path
}

if (!exists("enemdu_build_ipm_components")) {
  source(.ipm_components_repo_path("R", "utils-errors.R"), local = TRUE)
  source(.ipm_components_repo_path("R", "utils-metadata.R"), local = TRUE)
  .enemdu_read_csv_registry <- function(file) {
    readr::read_csv(
      .ipm_components_extdata_path(file),
      show_col_types = FALSE,
      progress = FALSE
    )
  }
  source(.ipm_components_repo_path("R", "nbi_sources.R"), local = TRUE)
  source(.ipm_components_repo_path("R", "labor_indicators.R"), local = TRUE)
  source(.ipm_components_repo_path("R", "ipm_sources.R"), local = TRUE)
  source(.ipm_components_repo_path("R", "ipm_flags.R"), local = TRUE)
  source(.ipm_components_repo_path("R", "ipm_components_household.R"), local = TRUE)
  source(.ipm_components_repo_path("R", "ipm_components.R"), local = TRUE)
}

.ipm_components_registry <- function() {
  registry <- read.csv(
    .ipm_components_extdata_path("ipm_component_registry.csv"),
    stringsAsFactors = FALSE,
    check.names = FALSE,
    fileEncoding = "UTF-8"
  )
  registry[order(registry$indicator_order), , drop = FALSE]
}

.ipm_component_names <- function() {
  .ipm_components_registry()$expected_component_name
}

.ipm_component_name <- function(indicator_id) {
  registry <- .ipm_components_registry()
  registry$expected_component_name[match(indicator_id, registry$indicator_id)]
}

.ipm_component_test_data <- function() {
  tibble::tibble(
    id_hogar = c("h1", "h1", "h2", "h2", "h3", "h3", "h3", "h4", "h4", "h4", "h4"),
    p01 = c(1, 2, 1, 2, 1, 2, 3, 1, 2, 3, 4),
    row_id = seq_len(11),
    vi10 = c(1, 1, 2, 2, 1, 1, 1, 1, 1, 1, 1),
    vi07 = c(1, 1, 1, 1, 0, 0, 0, 1, 1, 1, 1),
    expobre = c(0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0),
    nbi = 0L,
    pobre = 0L,
    labor_empleo = 1L
  )
}

.ipm_household_source_test_data <- function() {
  tibble::tibble(
    id_hogar = c("h1", "h2", "h3", "h4"),
    vi03a = c(1, 1, 1, 1),
    vi03b = c(1, 1, 1, 1),
    vi04a = c(1, 1, 1, 1),
    vi04b = c(1, 1, 1, 1),
    vi05a = c(1, 1, 1, 1),
    vi05b = c(1, 1, 1, 1),
    vi07 = c(1, 1, 0, 1),
    vi09 = c(1, 1, 1, 1),
    vi10 = c(1, 2, 1, 1),
    vi13 = c(1, 1, 1, 1)
  )
}

.ipm_precomputed_component_data <- function() {
  component_names <- .ipm_component_names()
  out <- tibble::tibble(
    id_hogar = c("h1", "h2"),
    p01 = c(1, 1),
    row_id = c(1, 2)
  )

  for (component in component_names) {
    out[[component]] <- 0L
  }

  out[[.ipm_component_name("ipm_i08_sin_agua_red_publica")]][2] <- 1L
  out
}

.ipm_operational_component_data <- function() {
  tibble::tibble(
    id_hogar = c("h1", "h1", "h2", "h2", "h3", "h3", "h4"),
    p01 = c(1, 2, 1, 2, 1, 2, 1),
    row_id = seq_len(7),
    upm = seq_len(7),
    estrato = c(1, 1, 2, 2, 3, 3, 4),
    fexp = rep(1, 7),
    p03 = c(10, 30, 12, 35, 40, 50, 25),
    p07 = c(1, 2, 2, 2, 2, 2, 2),
    p09 = c(1, 1, 3, 1, 1, 1, 1),
    p10a = c(5, 7, 5, 4, 6, 6, 5),
    p10b = c(5, 3, 7, 6, 6, 6, 9),
    condact = c(0, 1, 0, 7, 1, 9, 4),
    empleo = c(0, 1, 0, 1, 1, 0, 1),
    desempleo = c(0, 0, 0, 1, 0, 0, 0),
    p20 = c(2, 1, 2, 1, 1, 2, 1),
    p21 = c(12, 12, 12, 12, 12, 12, 12),
    p22 = c(2, 2, 2, 2, 2, 2, 2),
    p24 = c(NA, 40, NA, 20, 40, NA, 20),
    ingrl = c(NA, 600, NA, 300, 600, NA, 600),
    p61b1 = c(6, 1, 6, 5, 1, 6, 5),
    p72a = c(2, 2, 2, 2, 2, 2, 2),
    p75 = c(2, 2, 2, 2, 2, 2, 2),
    p77 = c(2, 2, 2, 2, 2, 2, 2),
    area = c(1, 1, 1, 1, 2, 2, 2),
    vi03a = c(1, 1, 1, 1, 1, 1, 1),
    vi03b = c(1, 1, 3, 3, 1, 1, 1),
    vi04a = c(1, 1, 1, 1, 1, 1, 1),
    vi04b = c(1, 1, 1, 1, 1, 1, 3),
    vi05a = c(1, 1, 1, 1, 1, 1, 1),
    vi05b = c(1, 1, 1, 1, 1, 1, 1),
    vi10 = c(1, 1, 2, 2, 1, 1, 1),
    vi07 = c(2, 2, 1, 1, 0, 0, 1),
    vi09 = c(1, 1, 2, 2, 2, 2, 3),
    vi13 = c(2, 2, 3, 3, 1, 1, 4),
    epobreza = c(0, 0, 1, 1, 0, 0, 0),
    nbi = 0L,
    pobre = 0L,
    labor_empleo = 1L
  )
}

.ipm_build_complete_components <- function(data = .ipm_operational_component_data(),
                                           strict = FALSE,
                                           overwrite = TRUE,
                                           ...) {
  enemdu_build_ipm_components(
    data,
    strict = strict,
    overwrite = overwrite,
    higher_education_economic_reason_codes = 3,
    ...
  )
}

.ipm_build_component_policy_data <- function(data, strict = FALSE, ...) {
  enemdu_build_ipm_components(
    data,
    strict = strict,
    overwrite = TRUE,
    higher_education_economic_reason_codes = 3,
    ...
  )
}

test_that("IPM component builder exists and is exported", {
  expect_true(exists("enemdu_build_ipm_components"))
  expect_true("enemdu_build_ipm_components" %in% getNamespaceExports("enemduR"))
  expect_true(is.function(getExportedValue("enemduR", "enemdu_build_ipm_components")))
})

test_that("IPM component builder preserves row count and order", {
  data <- .ipm_component_test_data()
  out <- enemdu_build_ipm_components(data, strict = FALSE)

  expect_equal(nrow(out), nrow(data))
  expect_equal(out$row_id, data$row_id)
  expect_equal(out$id_hogar, data$id_hogar)
})

test_that("IPM component builder uses names expected by the registry", {
  out <- enemdu_build_ipm_components(.ipm_component_test_data(), strict = FALSE)

  expect_true(all(.ipm_component_names() %in% names(out)))
})

test_that("IPM component builder preserves precomputed components when overwrite is false", {
  data <- .ipm_component_test_data()
  water_component <- .ipm_component_name("ipm_i08_sin_agua_red_publica")
  data[[water_component]] <- 1L

  out <- enemdu_build_ipm_components(data, strict = FALSE, overwrite = FALSE)

  expect_equal(out[[water_component]][out$id_hogar == "h1"], c(1, 1))
})

test_that("IPM component builder replaces implemented components when overwrite is true", {
  data <- .ipm_component_test_data()
  water_component <- .ipm_component_name("ipm_i08_sin_agua_red_publica")
  data[[water_component]] <- 1L

  out <- enemdu_build_ipm_components(data, strict = FALSE, overwrite = TRUE)

  expect_equal(out[[water_component]][out$id_hogar == "h1"], c(0, 0))
})

test_that("IPM component builder constructs water and overcrowding components", {
  out <- enemdu_build_ipm_components(.ipm_component_test_data(), strict = FALSE)
  water_component <- .ipm_component_name("ipm_i08_sin_agua_red_publica")
  overcrowding_component <- .ipm_component_name("ipm_i09_hacinamiento")

  expect_equal(out[[water_component]][out$id_hogar == "h1"], c(0L, 0L))
  expect_equal(out[[water_component]][out$id_hogar == "h2"], c(1L, 1L))
  expect_equal(out[[overcrowding_component]][out$id_hogar == "h4"], rep(1L, 4))
  expect_equal(out[[overcrowding_component]][out$id_hogar == "h1"], c(0L, 0L))
})

test_that("IPM component builder constructs extreme poverty from expobre", {
  out <- enemdu_build_ipm_components(.ipm_component_test_data(), strict = FALSE)
  extreme_component <- .ipm_component_name("ipm_i07_pobreza_extrema_ingresos")

  expect_equal(out[[extreme_component]], .ipm_component_test_data()$expobre)
})

test_that("IPM component builder accepts epobreza alias for extreme poverty", {
  data <- .ipm_operational_component_data()
  out <- enemdu_build_ipm_components(data, strict = FALSE)
  extreme_component <- .ipm_component_name("ipm_i07_pobreza_extrema_ingresos")

  expect_equal(out[[extreme_component]], data$epobreza)
})

test_that("IPM extreme poverty fallback fills only missing binary flags", {
  component <- .ipm_component_name("ipm_i07_pobreza_extrema_ingresos")
  data <- tibble::tibble(
    id_hogar = paste0("h", 1:4),
    p01 = 1L,
    expobre = c(1L, NA_integer_, NA_integer_, NA_integer_),
    ingpc = c(1000, 40, 60, NA)
  )

  out <- .ipm_build_component_policy_data(
    data,
    extreme_poverty_income_var = "ingpc",
    extreme_poverty_line = 52.07
  )
  expect_equal(out[[component]], c(1, 1, 0, NA))
  diagnostics <- attr(out, "ipm_component_diagnostics")
  extreme_diagnostics <- diagnostics$variables_used$extreme_income_poverty
  expect_equal(extreme_diagnostics$fallback_filled_n, 2L)
  expect_equal(extreme_diagnostics$fallback_missing_income_n, 1L)
  expect_equal(extreme_diagnostics$still_missing_n, 1L)

  out_without_fallback <- .ipm_build_component_policy_data(data)
  expect_equal(out_without_fallback[[component]], c(1, NA, NA, NA))
})

test_that("IPM component builder aborts when expobre and other components are pending in strict mode", {
  data <- .ipm_component_test_data()
  data$expobre <- NULL

  expect_error(
    enemdu_build_ipm_components(data, strict = TRUE),
    class = "enemdu_error_pending_ipm_components"
  )
})

test_that("IPM component builder derives water from vi10", {
  out <- enemdu_build_ipm_components(.ipm_operational_component_data(), strict = FALSE)
  water_component <- .ipm_component_name("ipm_i08_sin_agua_red_publica")

  expect_equal(out[[water_component]][out$id_hogar == "h1"], c(0L, 0L))
  expect_equal(out[[water_component]][out$id_hogar == "h2"], c(1L, 1L))
})

test_that("IPM component builder derives sanitation from area and vi09", {
  out <- enemdu_build_ipm_components(.ipm_operational_component_data(), strict = FALSE)
  sanitation_component <- .ipm_component_name("ipm_i11_sin_saneamiento_excretas")

  expect_equal(out[[sanitation_component]][out$id_hogar == "h1"], c(0L, 0L))
  expect_equal(out[[sanitation_component]][out$id_hogar == "h2"], c(1L, 1L))
  expect_equal(out[[sanitation_component]][out$id_hogar == "h3"], c(0L, 0L))
  expect_equal(out[[sanitation_component]][out$id_hogar == "h4"], 1L)
})

test_that("IPM sanitation component rejects invalid source codes", {
  component <- .ipm_component_name("ipm_i11_sin_saneamiento_excretas")
  data <- .ipm_operational_component_data()
  data$area <- 1
  data$vi09 <- c(rep(1, nrow(data) - 1), 999)

  expect_error(
    enemdu_build_ipm_components(
      data,
      strict = TRUE,
      overwrite = TRUE,
      sanitation_valid_codes = 1:5
    ),
    class = "enemdu_error_invalid_ipm_source_codes"
  )

  out <- enemdu_build_ipm_components(
    data,
    strict = FALSE,
    overwrite = TRUE,
    sanitation_valid_codes = 1:5
  )

  expect_true(is.na(out[[component]][nrow(out)]))
})

test_that("IPM component builder derives garbage collection from vi13", {
  out <- enemdu_build_ipm_components(.ipm_operational_component_data(), strict = FALSE)
  garbage_component <- .ipm_component_name("ipm_i12_sin_recoleccion_basura")

  expect_equal(out[[garbage_component]][out$id_hogar == "h1"], c(0L, 0L))
  expect_equal(out[[garbage_component]][out$id_hogar == "h3"], c(1L, 1L))
  expect_equal(out[[garbage_component]][out$id_hogar == "h2"], c(1L, 1L))
  expect_equal(out[[garbage_component]][out$id_hogar == "h4"], 1L)
})

test_that("IPM garbage component rejects invalid source codes", {
  component <- .ipm_component_name("ipm_i12_sin_recoleccion_basura")
  data <- .ipm_operational_component_data()
  data$vi13 <- c(rep(2, nrow(data) - 1), 999)

  expect_error(
    enemdu_build_ipm_components(
      data,
      strict = TRUE,
      overwrite = TRUE,
      garbage_valid_codes = 1:4
    ),
    class = "enemdu_error_invalid_ipm_source_codes"
  )

  out <- enemdu_build_ipm_components(
    data,
    strict = FALSE,
    overwrite = TRUE,
    garbage_valid_codes = 1:4
  )

  expect_true(is.na(out[[component]][nrow(out)]))
})

test_that("IPM component builder derives school attendance from p03 and p07", {
  out <- enemdu_build_ipm_components(.ipm_operational_component_data(), strict = FALSE)
  attendance_component <- .ipm_component_name("ipm_i01_inasistencia_basica_bach")

  expect_equal(out[[attendance_component]][out$id_hogar == "h1"], c(0L, 0L))
  expect_equal(out[[attendance_component]][out$id_hogar == "h2"], c(1L, 1L))
  expect_equal(out[[attendance_component]][out$id_hogar == "h3"], c(0L, 0L))
  expect_equal(out[[attendance_component]][out$id_hogar == "h4"], 0L)
})

test_that("IPM component builder derives incomplete education from p03, p07, p10a, and p10b", {
  out <- enemdu_build_ipm_components(.ipm_operational_component_data(), strict = FALSE)
  incomplete_component <- .ipm_component_name("ipm_i03_logro_educativo_incompleto")

  expect_equal(out[[incomplete_component]][out$id_hogar == "h1"], c(0L, 0L))
  expect_equal(out[[incomplete_component]][out$id_hogar == "h2"], c(1L, 1L))
  expect_equal(out[[incomplete_component]][out$id_hogar == "h3"], c(0L, 0L))
  expect_equal(out[[incomplete_component]][out$id_hogar == "h4"], 1L)
})

test_that("IPM component builder derives inadequate employment from consolidated labor flags", {
  out <- enemdu_build_ipm_components(.ipm_operational_component_data(), strict = FALSE)
  labor_component <- .ipm_component_name("ipm_i05_desempleo_empleo_inadecuado")

  expect_equal(out[[labor_component]][out$id_hogar == "h1"], c(0L, 0L))
  expect_equal(out[[labor_component]][out$id_hogar == "h2"], c(1L, 1L))
  expect_equal(out[[labor_component]][out$id_hogar == "h3"], c(0L, 0L))
  expect_equal(out[[labor_component]][out$id_hogar == "h4"], 1L)
})

test_that("IPM higher education access identifies economic exclusion", {
  component <- .ipm_component_name("ipm_i02_no_acceso_superior_economico")
  data <- .ipm_operational_component_data()
  data$p03[7] <- 25
  data$p07[7] <- 2
  data$p09[7] <- 3
  data$p10a[7] <- 7
  data$p10b[7] <- 3

  out <- .ipm_build_complete_components(data)

  expect_equal(out[[component]][out$id_hogar == "h4"], 1L)

  data$p03[7] <- 30
  out <- .ipm_build_complete_components(data)

  expect_equal(out[[component]][out$id_hogar == "h4"], 0L)
})

test_that("IPM higher education access handles missing and non-economic reasons", {
  component <- .ipm_component_name("ipm_i02_no_acceso_superior_economico")
  data <- .ipm_operational_component_data()
  data$p03[7] <- 25
  data$p07[7] <- 2
  data$p10a[7] <- 7
  data$p10b[7] <- 3
  data$p09[7] <- NA

  expect_error(
    .ipm_build_complete_components(data, strict = TRUE),
    class = "enemdu_error_missing_ipm_component_derivation"
  )

  out <- .ipm_build_complete_components(data, strict = FALSE)
  expect_true(is.na(out[[component]][out$id_hogar == "h4"]))

  data$p09[7] <- 1
  out <- .ipm_build_complete_components(data, strict = FALSE)
  expect_equal(out[[component]][out$id_hogar == "h4"], 0L)
})

test_that("IPM higher education access repeats household aggregation", {
  component <- .ipm_component_name("ipm_i02_no_acceso_superior_economico")
  data <- .ipm_operational_component_data()
  data$p03[2] <- 25
  data$p07[2] <- 2
  data$p09[2] <- 3
  data$p10a[2] <- 7
  data$p10b[2] <- 3

  out <- .ipm_build_complete_components(data)

  expect_equal(out[[component]][out$id_hogar == "h1"], c(1L, 1L))
})

test_that("IPM child and adolescent employment applies age-specific rules", {
  component <- .ipm_component_name("ipm_i04_empleo_infantil_adolescente")
  data <- .ipm_operational_component_data()

  data$p03[1] <- 10
  data$empleo[1] <- 1
  out <- .ipm_build_complete_components(data)
  expect_equal(out[[component]][out$id_hogar == "h1"], c(1L, 1L))

  data <- .ipm_operational_component_data()
  data$p03[7] <- 16
  data$empleo[7] <- 1
  data$p07[7] <- 2
  data$p24[7] <- 20
  data$ingrl[7] <- 600
  out <- .ipm_build_complete_components(data)
  expect_equal(out[[component]][out$id_hogar == "h4"], 1L)

  data$p07[7] <- 1
  data$p24[7] <- 31
  out <- .ipm_build_complete_components(data)
  expect_equal(out[[component]][out$id_hogar == "h4"], 1L)

  data$p24[7] <- 20
  data$ingrl[7] <- 100
  out <- .ipm_build_complete_components(data)
  expect_equal(out[[component]][out$id_hogar == "h4"], 1L)

  data$ingrl[7] <- 470
  out <- .ipm_build_complete_components(data)
  expect_equal(out[[component]][out$id_hogar == "h4"], 0L)
})

test_that("IPM child and adolescent employment applies NA policy by applicability", {
  component <- .ipm_component_name("ipm_i04_empleo_infantil_adolescente")
  data <- tibble::tibble(
    id_hogar = c("no_applicable", "no_applicable", "non_working", "working_child",
                 "not_attending_adolescent", "adult_missing"),
    p01 = c(1, 2, 1, 1, 1, 1),
    p03 = c(30, 70, 10, 10, 16, 30),
    p07 = c(NA, NA, NA, NA, 2, NA),
    empleo = c(NA, NA, 0, 1, 1, NA),
    p24 = c(NA, NA, NA, NA, NA, 999),
    ingrl = c(NA, NA, NA, NA, NA, 999999),
    expobre = 0L
  )

  out <- .ipm_build_component_policy_data(data)

  expect_equal(out[[component]][out$id_hogar == "no_applicable"], c(0L, 0L))
  expect_equal(out[[component]][out$id_hogar == "non_working"], 0L)
  expect_equal(out[[component]][out$id_hogar == "working_child"], 1L)
  expect_equal(out[[component]][out$id_hogar == "not_attending_adolescent"], 1L)
  expect_equal(out[[component]][out$id_hogar == "adult_missing"], 0L)

  diagnostics <- attr(out, "ipm_component_diagnostics")
  child_diagnostics <- diagnostics$variables_used$child_adolescent_employment
  expect_equal(child_diagnostics$applicable_persons_n, 3L)
  expect_equal(child_diagnostics$working_adolescents_unknown_hours_n, 1L)
  expect_equal(child_diagnostics$working_adolescents_unknown_income_n, 1L)
  expect_equal(child_diagnostics$household_na_n, 0L)
})

test_that("IPM child and adolescent employment treats ENEMDU employment NA by policy", {
  component <- .ipm_component_name("ipm_i04_empleo_infantil_adolescente")
  data <- tibble::tibble(
    id_hogar = c("child_na", "adolescent_na", "adult_missing", "not_attending_sentinel"),
    p01 = 1L,
    p03 = c(10, 16, 30, 16),
    p07 = c(NA, 1, NA, 2),
    empleo = c(NA, NA, NA, 1),
    p24 = c(NA, NA, 999, 999),
    ingrl = c(NA, NA, 999999, 999999),
    expobre = 0L
  )

  out <- .ipm_build_component_policy_data(data)

  expect_equal(out[[component]][out$id_hogar == "child_na"], 0L)
  expect_equal(out[[component]][out$id_hogar == "adolescent_na"], 0L)
  expect_equal(out[[component]][out$id_hogar == "adult_missing"], 0L)
  expect_equal(out[[component]][out$id_hogar == "not_attending_sentinel"], 1L)

  diagnostics <- attr(out, "ipm_component_diagnostics")
  child_diagnostics <- diagnostics$variables_used$child_adolescent_employment
  expect_true(child_diagnostics$employment_na_as_not_employed)
  expect_equal(child_diagnostics$employment_na_treated_as_not_employed_n, 2L)
  expect_equal(child_diagnostics$remaining_unknown_employment_cases_n, 0L)
  expect_equal(child_diagnostics$working_adolescents_unknown_hours_n, 1L)
  expect_equal(child_diagnostics$working_adolescents_unknown_income_n, 1L)

  out_unknown <- .ipm_build_component_policy_data(
    data,
    employment_na_as_not_employed = FALSE
  )
  expect_true(is.na(out_unknown[[component]][out_unknown$id_hogar == "child_na"]))
  expect_true(is.na(out_unknown[[component]][out_unknown$id_hogar == "adolescent_na"]))
  expect_equal(out_unknown[[component]][out_unknown$id_hogar == "adult_missing"], 0L)
})

test_that("IPM child and adolescent employment leaves undecidable adolescents as NA", {
  component <- .ipm_component_name("ipm_i04_empleo_infantil_adolescente")
  data <- tibble::tibble(
    id_hogar = "h1",
    p01 = 1L,
    p03 = 16,
    p07 = 1,
    empleo = 1,
    p24 = 999,
    ingrl = 600,
    vi10 = 1,
    vi07 = 1,
    expobre = 0L
  )

  out <- .ipm_build_component_policy_data(data, strict = FALSE)
  expect_true(is.na(out[[component]]))

  expect_error(
    .ipm_build_component_policy_data(data, strict = TRUE),
    class = "enemdu_error_invalid_ipm_source_values"
  )
})

test_that("IPM child and adolescent employment rejects hour and income sentinels", {
  component <- .ipm_component_name("ipm_i04_empleo_infantil_adolescente")
  data <- .ipm_operational_component_data()
  data$p03[7] <- 16
  data$empleo[7] <- 1
  data$p07[7] <- 1
  data$p24[7] <- 999
  data$ingrl[7] <- 600

  expect_error(
    .ipm_build_complete_components(data, strict = TRUE),
    class = "enemdu_error_invalid_ipm_source_values"
  )

  out <- .ipm_build_complete_components(data, strict = FALSE)
  expect_true(is.na(out[[component]][out$id_hogar == "h4"]))

  data$p24[7] <- 20
  data$ingrl[7] <- 999999

  expect_error(
    .ipm_build_complete_components(data, strict = TRUE),
    class = "enemdu_error_invalid_ipm_source_values"
  )

  out <- .ipm_build_complete_components(data, strict = FALSE)
  expect_true(is.na(out[[component]][out$id_hogar == "h4"]))
})

test_that("IPM pension contribution applies contribution and older-person exceptions", {
  component <- .ipm_component_name("ipm_i06_no_contribucion_pensiones")
  data <- .ipm_operational_component_data()

  data$p03[7] <- 25
  data$empleo[7] <- 1
  data$p61b1[7] <- 1
  out <- .ipm_build_complete_components(data)
  expect_equal(out[[component]][out$id_hogar == "h4"], 0L)

  data$p61b1[7] <- 5
  out <- .ipm_build_complete_components(data)
  expect_equal(out[[component]][out$id_hogar == "h4"], 1L)

  data$p03[7] <- 65
  data$empleo[7] <- 0
  data$p72a[7] <- 1
  out <- .ipm_build_complete_components(data)
  expect_equal(out[[component]][out$id_hogar == "h4"], 0L)

  data$desempleo[7] <- 0
  data$p72a[7] <- 2
  data$p75[7] <- 2
  data$p77[7] <- 2
  out <- .ipm_build_complete_components(data)
  expect_equal(out[[component]][out$id_hogar == "h4"], 1L)

  data$p77[7] <- 1
  out <- .ipm_build_complete_components(data)
  expect_equal(out[[component]][out$id_hogar == "h4"], 0L)
})

test_that("IPM pension contribution applies NA policy by applicable case", {
  component <- .ipm_component_name("ipm_i06_no_contribucion_pensiones")
  data <- tibble::tibble(
    id_hogar = c("under_15", "non_employed_15_64", "contributes",
                 "no_contribution", "older_benefit", "older_all_no"),
    p01 = 1L,
    p03 = c(14, 30, 30, 30, 65, 65),
    empleo = c(NA, 0, 1, 1, 0, 0),
    p61b1 = c(NA, NA, 1, 5, NA, NA),
    p72a = c(NA, NA, NA, NA, 1, 2),
    p75 = c(NA, NA, NA, NA, NA, 2),
    p77 = c(NA, NA, NA, NA, NA, 2),
    expobre = 0L
  )

  out <- .ipm_build_component_policy_data(data)

  expect_equal(out[[component]][out$id_hogar == "under_15"], 0L)
  expect_equal(out[[component]][out$id_hogar == "non_employed_15_64"], 0L)
  expect_equal(out[[component]][out$id_hogar == "contributes"], 0L)
  expect_equal(out[[component]][out$id_hogar == "no_contribution"], 1L)
  expect_equal(out[[component]][out$id_hogar == "older_benefit"], 0L)
  expect_equal(out[[component]][out$id_hogar == "older_all_no"], 1L)

  diagnostics <- attr(out, "ipm_component_diagnostics")
  pension_diagnostics <- diagnostics$variables_used$pension_contribution
  expect_equal(pension_diagnostics$occupied_15_plus_evaluated_n, 2L)
  expect_equal(pension_diagnostics$occupied_15_plus_unknown_contribution_n, 0L)
  expect_equal(pension_diagnostics$older_non_employed_evaluated_n, 2L)
  expect_equal(pension_diagnostics$older_non_employed_unknown_benefit_status_n, 0L)
  expect_equal(pension_diagnostics$household_na_n, 0L)
})

test_that("IPM pension contribution treats ENEMDU employment NA by policy", {
  component <- .ipm_component_name("ipm_i06_no_contribucion_pensiones")
  data <- tibble::tibble(
    id_hogar = c("under_15", "na_15_64", "older_all_no", "older_benefit",
                 "occupied_contributes", "occupied_no_contribution"),
    p01 = 1L,
    p03 = c(14, 30, 65, 65, 30, 30),
    empleo = c(NA, NA, NA, NA, 1, 1),
    p61b1 = c(NA, NA, NA, NA, 1, 5),
    p72a = c(NA, NA, 2, 1, NA, NA),
    p75 = c(NA, NA, 2, NA, NA, NA),
    p77 = c(NA, NA, 2, NA, NA, NA),
    expobre = 0L
  )

  out <- .ipm_build_component_policy_data(data)

  expect_equal(out[[component]][out$id_hogar == "under_15"], 0L)
  expect_equal(out[[component]][out$id_hogar == "na_15_64"], 0L)
  expect_equal(out[[component]][out$id_hogar == "older_all_no"], 1L)
  expect_equal(out[[component]][out$id_hogar == "older_benefit"], 0L)
  expect_equal(out[[component]][out$id_hogar == "occupied_contributes"], 0L)
  expect_equal(out[[component]][out$id_hogar == "occupied_no_contribution"], 1L)

  diagnostics <- attr(out, "ipm_component_diagnostics")
  pension_diagnostics <- diagnostics$variables_used$pension_contribution
  expect_true(pension_diagnostics$employment_na_as_not_employed)
  expect_equal(pension_diagnostics$employment_na_treated_as_not_employed_15_plus_n, 3L)
  expect_equal(pension_diagnostics$remaining_unknown_employment_cases_n, 0L)
  expect_equal(pension_diagnostics$occupied_15_plus_evaluated_n, 2L)
  expect_equal(pension_diagnostics$older_non_employed_evaluated_n, 2L)

  out_unknown <- .ipm_build_component_policy_data(
    data,
    employment_na_as_not_employed = FALSE
  )
  expect_equal(out_unknown[[component]][out_unknown$id_hogar == "under_15"], 0L)
  expect_true(is.na(out_unknown[[component]][out_unknown$id_hogar == "na_15_64"]))
})

test_that("IPM pension contribution treats unknown contribution as non-evaluable", {
  component <- .ipm_component_name("ipm_i06_no_contribucion_pensiones")
  data <- .ipm_operational_component_data()
  data$p03[7] <- 25
  data$empleo[7] <- 1
  data$p61b1[7] <- 6

  expect_error(
    .ipm_build_complete_components(data, strict = TRUE),
    class = "enemdu_error_missing_ipm_component_derivation"
  )

  out <- .ipm_build_complete_components(data, strict = FALSE)
  expect_true(is.na(out[[component]][out$id_hogar == "h4"]))
})

test_that("IPM pension contribution leaves unknown older benefit status as non-evaluable", {
  component <- .ipm_component_name("ipm_i06_no_contribucion_pensiones")
  data <- tibble::tibble(
    id_hogar = "h1",
    p01 = 1L,
    p03 = 65,
    empleo = 0,
    p72a = NA,
    p75 = 2,
    p77 = 2,
    vi10 = 1,
    vi07 = 1,
    expobre = 0L
  )

  out <- .ipm_build_component_policy_data(data, strict = FALSE)
  expect_true(is.na(out[[component]]))

  expect_error(
    .ipm_build_component_policy_data(data, strict = TRUE),
    class = "enemdu_error_missing_ipm_component_derivation"
  )
})

test_that("IPM pension contribution component does not require unemployment diagnostics", {
  component <- .ipm_component_name("ipm_i06_no_contribucion_pensiones")

  data <- tibble::tibble(
    id_hogar = c("h1", "h2"),
    p01 = c(1, 1),
    p03 = c(30, 30),
    empleo = c(1L, 1L),
    p61b1 = c(1L, 5L),
    p72a = c(2L, 2L),
    p75 = c(2L, 2L),
    p77 = c(2L, 2L)
  )

  out <- enemdu_build_ipm_components(
    data,
    household_id = "id_hogar",
    person_id = "p01",
    strict = FALSE,
    overwrite = TRUE,
    unemployment_var = "desempleo"
  )

  expect_true(component %in% names(out))
  expect_equal(out[[component]][out$id_hogar == "h1"], 0L)
  expect_equal(out[[component]][out$id_hogar == "h2"], 1L)

  diagnostics <- attr(out, "ipm_component_diagnostics")
  expect_false("desempleo" %in% diagnostics$variables_used$pension_contribution$source_vars)
  expect_false(isTRUE(diagnostics$variables_used$pension_contribution$unemployment_var_available))
})

test_that("IPM housing deficit uses state and configured material rules", {
  component <- .ipm_component_name("ipm_i10_deficit_habitacional")
  data <- .ipm_operational_component_data()

  out <- .ipm_build_complete_components(data)
  expect_equal(out[[component]][out$id_hogar == "h2"], c(1L, 1L))
  expect_equal(out[[component]][out$id_hogar == "h1"], c(0L, 0L))

  data$vi03b <- 1
  data$vi04b <- 1
  data$vi05b <- 3
  data$vi04a[7] <- 1
  data$vi04b[7] <- 3
  out <- .ipm_build_complete_components(
    data,
    housing_material_valid_codes = c(1, 9),
    deficit_floor_material_codes = 9
  )

  expect_equal(out[[component]][out$id_hogar == "h4"], 1L)
})

test_that("IPM housing deficit rejects invalid state codes", {
  component <- .ipm_component_name("ipm_i10_deficit_habitacional")
  data <- .ipm_operational_component_data()
  data$vi03b <- 1
  data$vi04b <- 1
  data$vi05b <- 1
  data$vi05b[7] <- 999

  expect_error(
    .ipm_build_complete_components(data, strict = TRUE),
    class = "enemdu_error_invalid_ipm_source_codes"
  )

  out <- .ipm_build_complete_components(data, strict = FALSE)
  expect_true(is.na(out[[component]][out$id_hogar == "h4"]))
})

test_that("IPM housing deficit diagnostics state when material sets are not configured", {
  data <- .ipm_build_complete_components()
  diagnostics <- attr(data, "ipm_component_diagnostics")
  housing_rule <- diagnostics$variables_used$housing_deficit$rule

  expect_equal(housing_rule, "official_techo_pared_piso_tipviv_classification")
})

test_that("IPM inadequate employment component uses the official 18+ universe", {
  output_component <- "ipm_i05_desempleo_empleo_inadecuado_flag"

  data <- tibble::tibble(
    id_hogar = c("h1", "h2"),
    p01 = c(1, 1),
    p03 = c(17, 18),
    labor_desempleo = c(1L, 1L),
    labor_subempleo = c(0L, 0L),
    labor_otro_empleo_no_pleno = c(0L, 0L),
    labor_empleo_no_remunerado = c(0L, 0L),
    labor_empleo_no_clasificado = c(0L, 0L)
  )

  out <- enemdu_build_ipm_components(
    data,
    household_id = "id_hogar",
    person_id = "p01",
    strict = FALSE,
    overwrite = TRUE,
    labor_inadequate_flags = c(
      "labor_desempleo",
      "labor_subempleo",
      "labor_otro_empleo_no_pleno",
      "labor_empleo_no_remunerado",
      "labor_empleo_no_clasificado"
    )
  )

  expect_true(output_component %in% names(out))

  expect_equal(
    out[[output_component]][out$id_hogar == "h1"],
    0L
  )

  expect_equal(
    out[[output_component]][out$id_hogar == "h2"],
    1L
  )
})

test_that("IPM component builder does not create score, flags, or aggregate columns", {
  out <- enemdu_build_ipm_components(.ipm_component_test_data(), strict = FALSE)

  expect_false(any(c("ipm_score", "tpm", "tpem", "A", "ipm") %in% names(out)))
})

test_that("IPM component builder returns pending components as NA when strict is false", {
  out <- enemdu_build_ipm_components(.ipm_component_test_data(), strict = FALSE)
  diagnostics <- attr(out, "ipm_component_diagnostics")

  pending_components <- diagnostics$components_pending

  expect_true(length(pending_components) > 0)
  expect_true(all(vapply(out[pending_components], function(x) all(is.na(x)), logical(1))))
  expect_true(all(vapply(out[pending_components], typeof, character(1)) == "integer"))
})

test_that("IPM component builder completes all components with full 2025 sources", {
  out <- .ipm_build_complete_components(.ipm_operational_component_data(), strict = TRUE)
  diagnostics <- attr(out, "ipm_component_diagnostics")

  expect_equal(diagnostics$components_pending, character())
  expect_true(all(.ipm_component_names() %in% names(out)))
  expect_false(any(is.na(unlist(out[.ipm_component_names()]))))
})

test_that("IPM component builder output from all derived components feeds flags", {
  out <- .ipm_build_complete_components(.ipm_operational_component_data(), strict = TRUE)
  flagged <- enemdu_build_ipm_flags(out, strict = TRUE, overwrite = TRUE)

  expect_true(all(c("ipm_score", "tpm", "tpem") %in% names(flagged)))
  expect_false(any(is.na(flagged$ipm_score)))
})

test_that("IPM component builder aborts in strict mode when not all components are available", {
  expect_error(
    enemdu_build_ipm_components(.ipm_component_test_data(), strict = TRUE),
    class = "enemdu_error_pending_ipm_components"
  )
})

test_that("IPM component builder accepts all 12 precomputed components", {
  out <- enemdu_build_ipm_components(.ipm_precomputed_component_data(), strict = TRUE)
  diagnostics <- attr(out, "ipm_component_diagnostics")

  expect_true(all(.ipm_component_names() %in% names(out)))
  expect_equal(diagnostics$components_pending, character())
  expect_setequal(diagnostics$components_precomputed, .ipm_component_names())
})

test_that("IPM component builder output can feed IPM flags when all components are available", {
  out <- enemdu_build_ipm_components(.ipm_precomputed_component_data(), strict = TRUE)
  flagged <- enemdu_build_ipm_flags(out)

  expect_true(all(c("ipm_score", "tpm", "tpem") %in% names(flagged)))
  expect_equal(flagged$ipm_score[1], 0)
  expect_equal(flagged$ipm_score[2], 0.125)
})

test_that("IPM component builder output can feed IPM flags when pending components are precomputed", {
  data <- .ipm_operational_component_data()
  pending_ids <- c(
    "ipm_i02_no_acceso_superior_economico",
    "ipm_i04_empleo_infantil_adolescente",
    "ipm_i06_no_contribucion_pensiones",
    "ipm_i10_deficit_habitacional"
  )

  for (indicator_id in pending_ids) {
    data[[.ipm_component_name(indicator_id)]] <- 0L
  }

  out <- enemdu_build_ipm_components(data, strict = TRUE)
  diagnostics <- attr(out, "ipm_component_diagnostics")
  flagged <- enemdu_build_ipm_flags(out)

  expect_equal(diagnostics$components_pending, character())
  expect_true(all(c("ipm_score", "tpm", "tpem") %in% names(flagged)))
  expect_false(any(is.na(flagged$ipm_score)))
})

test_that("IPM pension component preserves older employed pension exception", {
  component <- .ipm_component_name("ipm_i06_no_contribucion_pensiones")

  data <- tibble::tibble(
    id_hogar = c("h1", "h2", "h3"),
    p01 = c(1, 1, 1),
    p03 = c(65, 65, 65),
    empleo = c(1L, 1L, 1L),
    p61b1 = c(5L, 5L, 5L),
    p72a = c(1L, 2L, NA_real_),
    p75 = c(2L, 2L, 2L),
    p77 = c(2L, 2L, 2L)
  )

  out <- enemdu_build_ipm_components(
    data,
    household_id = "id_hogar",
    person_id = "p01",
    strict = FALSE,
    overwrite = TRUE,
    employment_na_as_not_employed = TRUE
  )

  expect_equal(out[[component]][out$id_hogar == "h1"], 0L)
  expect_equal(out[[component]][out$id_hogar == "h2"], 1L)
  expect_true(is.na(out[[component]][out$id_hogar == "h3"]))
})

test_that("IPM component builder can prepare source data from household_data", {
  data <- .ipm_component_test_data()
  data$vi10 <- NULL
  data$vi07 <- NULL
  household <- .ipm_household_source_test_data()

  out <- enemdu_build_ipm_components(
    data = data,
    household_data = household,
    strict = FALSE
  )
  diagnostics <- attr(out, "ipm_component_diagnostics")
  water_component <- .ipm_component_name("ipm_i08_sin_agua_red_publica")

  expect_true(diagnostics$source_join_applied)
  expect_equal(out[[water_component]][out$id_hogar == "h2"], c(1L, 1L))
})

test_that("IPM component builder preserves unrelated NBI, poverty, and labor outputs", {
  data <- .ipm_component_test_data()
  out <- enemdu_build_ipm_components(data, strict = FALSE)

  expect_equal(out$nbi, data$nbi)
  expect_equal(out$pobre, data$pobre)
  expect_equal(out$labor_empleo, data$labor_empleo)
})

test_that("IPM inadequate employment derivation ignores irrelevant sector variables", {
  component <- .ipm_component_name("ipm_i05_desempleo_empleo_inadecuado")

  data <- tibble::tibble(
    id_hogar = c("h1", "h2"),
    p01 = c(1, 1),
    p03 = c(18, 18),
    condact = c(1, 7),
    p20 = c(2, 1),
    p21 = c(12, 12),
    p22 = c(2, 2),
    secemp = c("invalid_sector", "also_invalid")
  )

  expect_no_error({
    result <- .enemdu_build_ipm_labor_inadequate_component(
      data = data,
      component_var = component,
      household_id = "id_hogar",
      age_var = "p03",
      condact_var = "condact",
      labor_inadequate_flags = c(
        "labor_desempleo",
        "labor_subempleo",
        "labor_otro_empleo_no_pleno",
        "labor_empleo_no_remunerado",
        "labor_empleo_no_clasificado"
      ),
      overwrite = TRUE,
      strict = TRUE
    )
  })

  expect_true(component %in% names(result$data))
  expect_false("secemp" %in% result$variables_used$labor_inadequate_employment$source_vars)
})

test_that("IPM i01 uses official basic and bachillerato attendance syntax", {
  component <- .ipm_component_name("ipm_i01_inasistencia_basica_bach")
  data <- tibble::tibble(
    id_hogar = paste0("h", 1:4),
    p01 = 1L,
    p03 = c(5, 15, 10, 17),
    p07 = c(1, 1, 2, 1),
    p10a = c(4, 7, 5, 6),
    p10b = c(1, 1, 3, 4)
  )

  out <- enemdu_build_ipm_components(data, strict = FALSE, overwrite = TRUE)

  expect_equal(out[[component]], c(0L, 0L, 1L, 0L))
  diagnostics <- attr(out, "ipm_component_diagnostics")
  expect_equal(diagnostics$variables_used$school_attendance$rule_status, "official_syntax_rule")
})

test_that("IPM i02 uses official p09 == 3 economic reason syntax", {
  component <- .ipm_component_name("ipm_i02_no_acceso_superior_economico")
  data <- tibble::tibble(
    id_hogar = paste0("h", 1:3),
    p01 = 1L,
    p03 = c(25, 25, 25),
    p07 = c(2, 2, 2),
    p09 = c(3, 2, NA),
    p10a = c(7, 7, 7),
    p10b = c(3, 3, 3)
  )

  out <- enemdu_build_ipm_components(data, strict = FALSE, overwrite = TRUE)

  expect_equal(out[[component]][1:2], c(1L, 0L))
  expect_true(is.na(out[[component]][3]))
  diagnostics <- attr(out, "ipm_component_diagnostics")
  expect_equal(
    diagnostics$variables_used$higher_education_access$economic_reason_codes,
    3
  )
})

test_that("IPM i03 uses official schooling formula and reports zero conversions", {
  component <- .ipm_component_name("ipm_i03_logro_educativo_incompleto")
  data <- tibble::tibble(
    id_hogar = paste0("h", 1:4),
    p01 = 1L,
    p03 = c(30, 30, 30, 30),
    p07 = c(2, 2, 1, 2),
    p10a = c(4, 7, 4, 99),
    p10b = c(8, 0, 8, 0)
  )

  out <- enemdu_build_ipm_components(data, strict = FALSE, overwrite = TRUE)

  expect_equal(out[[component]][1:3], c(1L, 0L, 0L))
  expect_true(is.na(out[[component]][4]))
  diagnostics <- attr(out, "ipm_component_diagnostics")
  expect_equal(
    diagnostics$variables_used$incomplete_education$schooling_unmatched_converted_to_zero_n,
    0L
  )
  expect_equal(
    diagnostics$variables_used$incomplete_education$schooling_unmatched_observed_n,
    1L
  )
})

test_that("IPM i03 keeps missing schooling inputs unknown and preserves true zero", {
  component <- .ipm_component_name("ipm_i03_logro_educativo_incompleto")
  data <- tibble::tibble(
    id_hogar = paste0("h", 1:3),
    p01 = 1L,
    p03 = c(30, 30, 30),
    p07 = c(2, 2, 2),
    p10a = c(NA_real_, 4, 1),
    p10b = c(0, NA_real_, 0)
  )

  out <- enemdu_build_ipm_components(data, strict = FALSE, overwrite = TRUE)
  diagnostics <- attr(out, "ipm_component_diagnostics")
  by_component <- diagnostics$critical_missing_by_component
  i03 <- by_component[
    by_component$indicator_id == "ipm_i03_logro_educativo_incompleto",
    ,
    drop = FALSE
  ]

  expect_true(is.na(out[[component]][1]))
  expect_true(is.na(out[[component]][2]))
  expect_equal(out[[component]][3], 1L)
  expect_equal(i03$critical_missing_person_rows, 2L)
  expect_equal(
    diagnostics$variables_used$incomplete_education$schooling_missing_required_grade_n,
    1L
  )
})

test_that("IPM i04 uses official condactn, hours, and p51 syntax", {
  component <- .ipm_component_name("ipm_i04_empleo_infantil_adolescente")
  data <- tibble::tibble(
    id_hogar = paste0("h", 1:6),
    p01 = 1L,
    p03 = c(10, 10, 16, 16, 16, 16),
    p07 = c(1, 1, 1, 2, 1, 1),
    empleo = 1L,
    p20 = c(1, 2, 2, 1, 2, 1),
    p21 = c(12, 12, 12, 12, 12, 12),
    p22 = c(2, 2, 2, 2, 1, 2),
    p24 = c(10, 10, 10, 20, NA, 20),
    pea = 1L,
    condactn = c(1, 1, 2, 1, 1, 1),
    p51a = c(NA, NA, NA, NA, 20, NA),
    p51b = c(NA, NA, NA, NA, 20, NA)
  )

  out <- enemdu_build_ipm_components(data, strict = FALSE, overwrite = TRUE)

  expect_equal(out[[component]], c(1L, 0L, 1L, 1L, 1L, 0L))
  diagnostics <- attr(out, "ipm_component_diagnostics")
  expect_equal(
    diagnostics$variables_used$child_adolescent_employment$rule_status,
    "official_syntax_rule"
  )
})

test_that("IPM i04 preserves all-missing p51 hours as critical missing", {
  component <- .ipm_component_name("ipm_i04_empleo_infantil_adolescente")
  data <- tibble::tibble(
    id_hogar = paste0("h", 1:3),
    p01 = 1L,
    p03 = c(16, 16, 16),
    p07 = c(1, 1, 1),
    empleo = 1L,
    p20 = c(2, 2, 2),
    p21 = c(12, 12, 12),
    p22 = c(1, 1, 1),
    p24 = NA_real_,
    pea = 1L,
    condactn = 1L,
    p51a = c(NA_real_, 999, 20),
    p51b = c(NA_real_, 999, NA_real_)
  )

  out <- enemdu_build_ipm_components(data, strict = FALSE, overwrite = TRUE)
  diagnostics <- attr(out, "ipm_component_diagnostics")
  by_component <- diagnostics$critical_missing_by_component
  i04 <- by_component[
    by_component$indicator_id == "ipm_i04_empleo_infantil_adolescente",
    ,
    drop = FALSE
  ]

  expect_true(is.na(out[[component]][1]))
  expect_true(is.na(out[[component]][2]))
  expect_equal(out[[component]][3], 0L)
  expect_equal(i04$critical_missing_person_rows, 2L)
  expect_equal(
    diagnostics$variables_used$child_adolescent_employment$working_adolescents_unknown_hours_n,
    2L
  )
})

test_that("IPM i04 does not overwrite decided adolescent deprivation with missing inputs", {
  component <- .ipm_component_name("ipm_i04_empleo_infantil_adolescente")
  data <- tibble::tibble(
    id_hogar = paste0("h", 1:3),
    p01 = 1L,
    p03 = c(16, 16, 16),
    p07 = c(2, NA_real_, 1),
    empleo = 1L,
    p20 = 1L,
    p21 = 12L,
    p22 = 2L,
    p24 = c(NA_real_, 31, NA_real_),
    pea = 1L,
    condactn = 1L
  )

  out <- enemdu_build_ipm_components(data, strict = FALSE, overwrite = TRUE)
  diagnostics <- attr(out, "ipm_component_diagnostics")
  by_component <- diagnostics$critical_missing_by_component
  i04 <- by_component[
    by_component$indicator_id == "ipm_i04_empleo_infantil_adolescente",
    ,
    drop = FALSE
  ]

  expect_equal(out[[component]][1:2], c(1L, 1L))
  expect_true(is.na(out[[component]][3]))
  expect_equal(i04$critical_missing_person_rows, 1L)
})

test_that("IPM i05 uses official condactn in 2:8 syntax", {
  component <- .ipm_component_name("ipm_i05_desempleo_empleo_inadecuado")
  data <- tibble::tibble(
    id_hogar = paste0("h", 1:5),
    p01 = 1L,
    p03 = c(18, 18, 17, 18, 18),
    condactn = c(2, 1, 7, NA, 1),
    p20 = c(1, 1, 1, 1, NA),
    p21 = c(12, 12, 12, 12, NA),
    p22 = c(2, 2, 2, 2, NA),
    p32 = c(1, 1, 1, 1, NA),
    p34 = c(1, 1, 1, 1, NA),
    p35 = c(1, 1, 1, 1, NA)
  )

  out <- enemdu_build_ipm_components(data, strict = FALSE, overwrite = TRUE)
  diagnostics <- attr(out, "ipm_component_diagnostics")

  expect_equal(out[[component]][1:3], c(1L, 0L, 0L))
  expect_true(is.na(out[[component]][4]))
  expect_true(is.na(out[[component]][5]))
  expect_equal(
    diagnostics$variables_used$labor_inadequate_employment$rule_status,
    "official_syntax_rule"
  )
})

test_that("IPM i05 falls back when compact condact lacks raw labor block", {
  component <- .ipm_component_name("ipm_i05_desempleo_empleo_inadecuado")
  data <- tibble::tibble(
    id_hogar = paste0("h", 1:3),
    p01 = 1L,
    p03 = c(18, 18, 17),
    condact = c(1, 7, 7)
  )

  out <- enemdu_build_ipm_components(data, strict = FALSE, overwrite = TRUE)
  diagnostics <- attr(out, "ipm_component_diagnostics")
  labor_diagnostics <- diagnostics$variables_used$labor_inadequate_employment

  expect_equal(out[[component]], c(0L, 1L, 0L))
  expect_equal(labor_diagnostics$rule_status, "proxy_fallback_not_official_syntax")
  expect_equal(labor_diagnostics$fallback_reason, "raw_labor_block_vars_missing")
  expect_equal(
    labor_diagnostics$missing_official_source_vars,
    c("p20", "p21", "p22", "p32", "p34", "p35")
  )
})

test_that("IPM i05 uses official branch when compact condact has raw labor block", {
  component <- .ipm_component_name("ipm_i05_desempleo_empleo_inadecuado")
  data <- tibble::tibble(
    id_hogar = paste0("h", 1:2),
    p01 = 1L,
    p03 = c(18, 18),
    condact = c(2, 1),
    p20 = c(1, 1),
    p21 = c(12, 12),
    p22 = c(2, 2),
    p32 = c(1, 1),
    p34 = c(1, 1),
    p35 = c(1, 1)
  )

  out <- enemdu_build_ipm_components(data, strict = FALSE, overwrite = TRUE)
  diagnostics <- attr(out, "ipm_component_diagnostics")

  expect_equal(out[[component]], c(1L, 0L))
  expect_equal(
    diagnostics$variables_used$labor_inadequate_employment$rule_status,
    "official_syntax_rule"
  )
})

test_that("IPM i06 uses official p05a/p05b and benefit exceptions", {
  component <- .ipm_component_name("ipm_i06_no_contribucion_pensiones")
  data <- tibble::tibble(
    id_hogar = paste0("h", 1:7),
    p01 = 1L,
    p03 = c(30, 30, 65, 65, 65, 30, 30),
    empleo = c(1, 1, 1, 0, 0, 1, 1),
    p05a = c(5, 1, 5, 5, 5, 5, NA),
    p05b = c(5, 1, 5, 5, 5, 5, 5),
    p72a = c(2, 2, 1, 2, 2, 2, 2),
    p75 = c(2, 2, 2, 2, 1, 2, 2),
    p77 = c(2, 2, 2, 2, 2, 1, 2),
    desem = c(0, 0, 0, 1, 1, 0, 0),
    pei = c(0, 0, 0, 0, 0, 0, 0),
    pet = 1L
  )

  out <- enemdu_build_ipm_components(data, strict = FALSE, overwrite = TRUE)

  expect_equal(out[[component]][1:6], c(1L, 0L, 0L, 1L, 0L, 0L))
  expect_true(is.na(out[[component]][7]))
})

test_that("IPM i06 official missingness is branch-specific", {
  component <- .ipm_component_name("ipm_i06_no_contribucion_pensiones")
  data <- tibble::tibble(
    id_hogar = paste0("h", 1:5),
    p01 = 1L,
    p03 = c(30, 30, 65, 65, 65),
    empleo = 1L,
    p05a = c(1, 5, 5, 5, 5),
    p05b = c(1, 5, 5, 5, 5),
    p72a = c(NA_real_, NA_real_, NA_real_, 1, 2),
    p75 = c(NA_real_, NA_real_, 2, NA_real_, 2),
    p77 = c(NA_real_, NA_real_, 2, NA_real_, 2),
    desem = 0L,
    pei = 0L,
    pet = 1L
  )

  out <- enemdu_build_ipm_components(data, strict = FALSE, overwrite = TRUE)
  diagnostics <- attr(out, "ipm_component_diagnostics")
  by_component <- diagnostics$critical_missing_by_component
  i06 <- by_component[
    by_component$indicator_id == "ipm_i06_no_contribucion_pensiones",
    ,
    drop = FALSE
  ]

  expect_equal(out[[component]][1:2], c(0L, 1L))
  expect_true(is.na(out[[component]][3]))
  expect_equal(out[[component]][4:5], c(0L, 1L))
  expect_equal(i06$critical_missing_person_rows, 1L)
  expect_equal(
    diagnostics$variables_used$pension_contribution$rule_status,
    "official_syntax_rule"
  )
})

test_that("IPM i09 recodes zero bedrooms to one under official policy", {
  component <- .ipm_component_name("ipm_i09_hacinamiento")
  data <- tibble::tibble(
    id_hogar = "h1",
    p01 = 1:3,
    vi10 = 1,
    vi07 = 0
  )

  out <- enemdu_build_ipm_components(data, strict = FALSE, overwrite = TRUE)
  expect_equal(out[[component]], c(0L, 0L, 0L))

  out_deprived <- enemdu_build_ipm_components(
    data,
    strict = FALSE,
    overwrite = TRUE,
    bedrooms_zero_policy = "deprived"
  )
  expect_equal(out_deprived[[component]], c(1L, 1L, 1L))
})

test_that("IPM i10 uses official techo pared piso tipviv syntax", {
  component <- .ipm_component_name("ipm_i10_deficit_habitacional")
  data <- tibble::tibble(
    id_hogar = paste0("h", 1:3),
    p01 = 1L,
    vi03a = c(1, 1, 2),
    vi03b = c(1, 1, 3),
    vi04a = c(1, 1, 1),
    vi04b = c(1, 3, 1),
    vi05a = c(1, 1, 2),
    vi05b = c(1, 3, 3)
  )

  out <- enemdu_build_ipm_components(data, strict = FALSE, overwrite = TRUE)

  expect_equal(out[[component]], c(0L, 1L, 1L))
})

test_that("IPM i12 treats only vi13 == 2 as non-deprived", {
  component <- .ipm_component_name("ipm_i12_sin_recoleccion_basura")
  data <- tibble::tibble(
    id_hogar = paste0("h", 1:6),
    p01 = 1L,
    vi13 = c(1, 2, 3, 4, 5, NA)
  )

  out <- enemdu_build_ipm_components(data, strict = FALSE, overwrite = TRUE)

  expect_equal(out[[component]][1:5], c(1L, 0L, 1L, 1L, 1L))
  expect_true(is.na(out[[component]][6]))
})

test_that("IPM component diagnostics expose missing-critical households", {
  data <- tibble::tibble(
    id_hogar = c("h1", "h1", "h2"),
    p01 = c(1, 2, 1),
    p03 = c(10, 30, 10),
    p07 = c(NA, 2, 1),
    p10a = c(5, 7, 5),
    p10b = c(3, 3, 5)
  )

  out <- enemdu_build_ipm_components(data, strict = FALSE, overwrite = TRUE)
  diagnostics <- attr(out, "ipm_component_diagnostics")
  by_component <- diagnostics$critical_missing_by_component
  i01 <- by_component[
    by_component$indicator_id == "ipm_i01_inasistencia_basica_bach",
    ,
    drop = FALSE
  ]

  expect_true(all(c(
    "ipm_missing_critical_flag",
    "ipm_missing_critical_household_flag"
  ) %in% names(out)))
  expect_equal(i01$critical_missing_person_rows, 1L)
  expect_equal(i01$critical_missing_households, 1L)
  expect_equal(diagnostics$official_validation_status, "not_officially_validated")
})

test_that("IPM proxy fallback remains explicit when official child variables are unavailable", {
  data <- tibble::tibble(
    id_hogar = "h1",
    p01 = 1L,
    p03 = 16,
    p07 = 1,
    empleo = 1,
    p24 = 20,
    ingrl = 600
  )

  out <- enemdu_build_ipm_components(data, strict = FALSE, overwrite = TRUE)
  diagnostics <- attr(out, "ipm_component_diagnostics")

  expect_equal(
    diagnostics$variables_used$child_adolescent_employment$rule_status,
    "proxy_fallback_not_official_syntax"
  )
})
