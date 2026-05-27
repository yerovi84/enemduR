.nbi_raw_component_test_data <- function() {
  tibble::tibble(
    id_hogar = c("h1", "h1", "h2", "h2", "h3", "h3"),
    p01 = c(1, 2, 1, 2, 1, 2),
    p03 = c(40, 8, 45, 10, 50, 20),
    p04 = c(1, 3, 1, 3, 1, 3),
    p07 = c(2, 2, 2, 1, 2, 2),
    p10a = c(1, 3, 2, 3, 4, 4),
    p10b = c(0, 2, 1, 3, 5, 5),
    empleo = c(0, 0, 1, 0, 1, 1),
    vi04a = c(7, 7, 1, 1, 1, 1),
    vi05a = c(1, 1, 6, 6, 1, 1),
    vi07 = c(0, 0, 1, 1, 2, 2),
    vi09 = c(5, 5, 1, 1, 1, 1),
    vi10 = c(1, 1, 2, 2, 1, 1),
    vi10a = c(1, 1, 3, 3, 1, 1),
    area = c(1, 1, 2, 2, 1, 1),
    fexp = 1,
    upm = c(1, 1, 2, 2, 3, 3),
    estrato = c(1, 1, 2, 2, 3, 3)
  )
}

test_that("NBI derivation registry is available", {
  registry <- enemdu_nbi_derivation_registry()

  expect_s3_class(registry, "tbl_df")
  expect_true(all(c(
    "profile_id",
    "component_id",
    "component_var",
    "component_label",
    "source_vars",
    "rule_id",
    "rule_description",
    "source_status",
    "source_note"
  ) %in% names(registry)))
  expect_true(all(paste0("comp", 1:5) %in% registry$component_var))
  expect_true(any(registry$profile_id == "enemdu_2025_anual"))
})

test_that("NBI component input preflight detects present variables", {
  data <- .nbi_raw_component_test_data()

  preflight <- enemdu_validate_nbi_component_inputs(data)

  expect_s3_class(preflight, "enemdu_nbi_component_input_preflight")
  expect_true(isTRUE(attr(preflight, "preflight_passed")))
  expect_true(all(preflight$issue[preflight$required] == "ok"))
})

test_that("NBI component input preflight detects missing variables", {
  data <- .nbi_raw_component_test_data()
  data$vi10a <- NULL

  preflight <- enemdu_validate_nbi_component_inputs(data)
  missing_row <- preflight[preflight$variable == "vi10a", , drop = FALSE]

  expect_false(isTRUE(attr(preflight, "preflight_passed")))
  expect_false(missing_row$present)
  expect_equal(missing_row$issue, "missing_required_variable")
})

test_that("NBI component builder creates comp1 through comp5", {
  out <- enemdu_build_nbi_components(.nbi_raw_component_test_data())

  expect_true(all(paste0("comp", 1:5) %in% names(out)))
  expect_equal(out$comp1, c(1L, 1L, 1L, 1L, 0L, 0L))
  expect_equal(out$comp2, c(1L, 1L, 0L, 0L, 0L, 0L))
  expect_equal(out$comp3, c(1L, 1L, 1L, 1L, 0L, 0L))
  expect_equal(out$comp4, c(1L, 1L, 0L, 0L, 0L, 0L))
  expect_equal(out$comp5, c(1L, 1L, 0L, 0L, 0L, 0L))
})

test_that("NBI component builder protects existing components unless overwrite is requested", {
  data <- .nbi_raw_component_test_data()
  data$comp1 <- 99L

  expect_error(
    enemdu_build_nbi_components(data),
    class = "enemdu_error_existing_nbi_components"
  )

  out <- enemdu_build_nbi_components(data, overwrite = TRUE)

  expect_equal(out$comp1[1], 1L)
})

test_that("NBI component builder creates household size when absent", {
  out <- enemdu_build_nbi_components(.nbi_raw_component_test_data())

  expect_true("hsize" %in% names(out))
  expect_equal(out$hsize, rep(2L, 6))
})

test_that("NBI overcrowding handles zero sleeping rooms and ratio greater than three", {
  data <- .nbi_raw_component_test_data()
  data$vi07 <- c(0, 0, 1, 1, 1, 1)
  data$id_hogar <- c("h1", "h1", "h2", "h2", "h3", "h3")
  data <- rbind(
    data,
    data[5, ],
    data[6, ]
  )
  data$id_hogar[7:8] <- "h3"
  data$p01 <- seq_len(nrow(data))

  out <- enemdu_build_nbi_components(data)

  expect_equal(out$comp2[out$id_hogar == "h1"], c(1L, 1L))
  expect_true(all(out$comp2[out$id_hogar == "h3"] == 1L))
})

test_that("NBI education deprivation propagates to all household members", {
  out <- enemdu_build_nbi_components(.nbi_raw_component_test_data())

  expect_equal(out$comp4[out$id_hogar == "h1"], c(1L, 1L))
  expect_equal(out$comp4[out$id_hogar == "h2"], c(0L, 0L))
})

test_that("NBI economic capacity handles zero occupied persons without division by zero", {
  out <- enemdu_build_nbi_components(.nbi_raw_component_test_data())

  expect_equal(out$comp5[out$id_hogar == "h1"], c(1L, 1L))
  expect_equal(out$comp5[out$id_hogar == "h2"], c(0L, 0L))
})

test_that("NBI economic capacity treats missing employment as not occupied for ENEMDU 2025 profile", {
  data <- .nbi_raw_component_test_data()
  data$empleo[data$id_hogar == "h1"] <- c(0, NA)

  out <- enemdu_build_nbi_components(data)

  expect_equal(out$comp5[out$id_hogar == "h1"], c(1L, 1L))
})

test_that("NBI component derivation uses visible factor values", {
  data <- .nbi_raw_component_test_data()
  data$vi04a <- factor(as.character(data$vi04a), levels = c("1", "7"))
  data$vi05a <- factor(as.character(data$vi05a), levels = c("1", "6"))
  data$vi07 <- factor(as.character(data$vi07), levels = c("0", "1", "2"))
  data$empleo <- factor(as.character(data$empleo), levels = c("0", "1"))

  out <- enemdu_build_nbi_components(data)

  expect_equal(out$comp1[1], 1L)
  expect_equal(out$comp2[1], 1L)
  expect_equal(out$comp5[1], 1L)
})

test_that("NBI raw components connect to final NBI flags", {
  out <- .nbi_raw_component_test_data()
  out <- enemdu_build_nbi_components(out)
  flagged <- enemdu_build_nbi_flags(out)

  expect_true(all(c("knbi", "nbi", "xnbi") %in% names(flagged)))
  expect_equal(flagged$knbi[1], 5L)
  expect_equal(flagged$nbi[1], 1L)
  expect_equal(flagged$xnbi[1], 1L)
})

test_that("NBI raw component builder does not implement multidimensional outputs", {
  out <- enemdu_build_nbi_components(.nbi_raw_component_test_data())

  expect_false(any(c("tpm", "tpem", "ipm", "intensity") %in% tolower(names(out))))
})

test_that("NBI services component preserves missing water information as non-evaluable", {
  data <- .nbi_raw_component_test_data()

  data$vi09 <- c(1, 1, 1, 1, 1, 1)
  data$vi10 <- c(NA, NA, 1, 1, 1, 1)
  data$vi10a <- c(1, 1, 1, 1, 1, 1)

  out <- enemdu_build_nbi_components(data)

  expect_true(all(is.na(out$comp3[out$id_hogar == "h1"])))
  expect_equal(out$comp3[out$id_hogar == "h2"], c(0L, 0L))
  expect_equal(out$comp3[out$id_hogar == "h3"], c(0L, 0L))
})


test_that("NBI services component keeps observed water deprivation with missing sanitation", {
  data <- .nbi_raw_component_test_data()

  data$vi09 <- c(NA, NA, 1, 1, 1, 1)
  data$vi10 <- c(2, 2, 1, 1, 1, 1)
  data$vi10a <- c(3, 3, 1, 1, 1, 1)

  out <- enemdu_build_nbi_components(data)

  expect_equal(out$comp3[out$id_hogar == "h1"], c(1L, 1L))
  expect_equal(out$comp3[out$id_hogar == "h2"], c(0L, 0L))
  expect_equal(out$comp3[out$id_hogar == "h3"], c(0L, 0L))
})


test_that("NBI schooling assigns zero years when head education level is none and grade is missing", {
  data <- .nbi_raw_component_test_data()

  data$p10a[data$id_hogar == "h1" & data$p04 == 1] <- 1
  data$p10b[data$id_hogar == "h1" & data$p04 == 1] <- NA
  data$empleo[data$id_hogar == "h1"] <- c(NA, NA)

  out <- enemdu_build_nbi_components(data)

  expect_equal(out$comp5[out$id_hogar == "h1"], c(1L, 1L))
})


test_that("NBI economic capacity treats missing empleo as not occupied for ENEMDU 2025 profile", {
  data <- .nbi_raw_component_test_data()

  data$empleo <- c(NA, NA, 1, NA, 1, 1)
  data$p10a[data$id_hogar == "h1" & data$p04 == 1] <- 1
  data$p10b[data$id_hogar == "h1" & data$p04 == 1] <- NA

  out <- enemdu_build_nbi_components(data)

  expect_equal(out$comp5[out$id_hogar == "h1"], c(1L, 1L))
})

test_that("NBI schooling assigns zero years when education level is none", {
  data <- .nbi_raw_component_test_data()

  data$p10a[data$id_hogar == "h1" & data$p04 == 1] <- 1
  data$p10b[data$id_hogar == "h1" & data$p04 == 1] <- NA
  data$empleo[data$id_hogar == "h1"] <- c(NA, NA)

  out <- enemdu_build_nbi_components(data)

  expect_equal(out$comp5[out$id_hogar == "h1"], c(1L, 1L))
})


test_that("NBI basic services accepts other piped water source with piped reception", {
  data <- .nbi_raw_component_test_data()

  data$vi09 <- 1
  data$vi10 <- 3
  data$vi10a <- 2

  out <- enemdu_build_nbi_components(data)

  expect_equal(out$comp3, rep(0L, nrow(out)))
})

test_that("NBI basic services accepts piped water outside dwelling or lot", {
  data <- .nbi_raw_component_test_data()

  data$vi09 <- 1
  data$vi10 <- 3
  data$vi10a <- 3

  out <- enemdu_build_nbi_components(data)

  expect_equal(out$comp3, rep(0L, nrow(out)))
})

test_that("NBI basic services flags no piped water reception", {
  data <- .nbi_raw_component_test_data()

  data$vi09 <- 1
  data$vi10 <- 1
  data$vi10a <- 4

  out <- enemdu_build_nbi_components(data)

  expect_equal(out$comp3, rep(1L, nrow(out)))
})

test_that("NBI basic services flags blind pit sanitation", {
  data <- .nbi_raw_component_test_data()

  data$vi09 <- 3
  data$vi10 <- 1
  data$vi10a <- 1

  out <- enemdu_build_nbi_components(data)

  expect_equal(out$comp3, rep(1L, nrow(out)))
})
