.nbi_kpi_test_data <- function() {
  tibble::tibble(
    idhogar = paste0("h", seq_len(8)),
    hsize = rep(1L, 8),
    upm = seq_len(8),
    estrato = rep(c(1, 2), each = 4),
    fexp = rep(1, 8),
    area = c("1", "1", "2", "2", "1", "1", "2", "2"),
    comp1 = c(0, 1, 1, 1, 0, 0, 1, 1),
    comp2 = c(0, 0, 1, 1, 0, 0, 0, 1),
    comp3 = c(0, 0, 0, 1, 0, 0, 0, 0),
    comp4 = c(0, 0, 0, 1, 0, 0, 0, 0),
    comp5 = c(0, 0, 0, 1, 0, 0, 0, 0)
  )
}

test_that("NBI KPI estimates poverty and extreme poverty indicators", {
  result <- enemdu_kpi_nbi(
    .nbi_kpi_test_data(),
    survey_type = "mensual",
    sample_n_min = 1
  )

  expect_s3_class(result, "enemdu_nbi_kpi")
  expect_true(all(c("pobreza_nbi", "pobreza_extrema_nbi") %in% result$indicator_id))
  expect_true(all(c(
    "nbi_source_status",
    "official_validation_status",
    "official_validation_note",
    "nbi_component_contract"
  ) %in% names(result)))
  expect_equal(unique(result$official_validation_status), "not_officially_validated")
})

test_that("NBI KPI supports grouped estimates", {
  data <- .nbi_kpi_test_data()
  data$area_domain <- ifelse(data$area == "1", "urban", "rural")

  result <- enemdu_kpi_nbi(
    data,
    group_vars = "area_domain",
    survey_type = "mensual",
    domain_level = "urbano_rural",
    domain_var = "area",
    sample_n_min = 1
  )

  expect_true(all(c("urban", "rural") %in% result$area_domain))
  expect_true(all(c("pobreza_nbi", "pobreza_extrema_nbi") %in% result$indicator_id))
})

test_that("NBI component registry is available and auditable", {
  registry <- enemdu_nbi_component_registry()

  expect_s3_class(registry, "tbl_df")
  expect_true(all(c(
    "component_id",
    "component_var",
    "component_label",
    "component_order",
    "expected_binary",
    "source_status",
    "notes"
  ) %in% names(registry)))
  expect_equal(registry$component_var, paste0("comp", 1:5))
})
