.nbi_test_data <- function() {
  tibble::tibble(
    idhogar = paste0("h", 1:5),
    hsize = rep(1L, 5),
    upm = 1:5,
    estrato = c(1, 1, 2, 2, 3),
    fexp = rep(1, 5),
    area = c("1", "1", "2", "2", "1"),
    comp1 = c(0, 1, 1, 1, NA),
    comp2 = c(0, 0, 1, 1, 0),
    comp3 = c(0, 0, 0, 1, 0),
    comp4 = c(0, 0, 0, 1, 0),
    comp5 = c(0, 0, 0, 1, 0)
  )
}

test_that("NBI flags preserve zero counts and derive poverty flags", {
  out <- enemdu_build_nbi_flags(.nbi_test_data())

  expect_equal(out$knbi[1], 0L)
  expect_equal(out$nbi[1], 0L)
  expect_equal(out$xnbi[1], 0L)

  expect_equal(out$knbi[2], 1L)
  expect_equal(out$nbi[2], 1L)
  expect_equal(out$xnbi[2], 0L)

  expect_equal(out$knbi[3], 2L)
  expect_equal(out$nbi[3], 1L)
  expect_equal(out$xnbi[3], 1L)

  expect_equal(out$knbi[4], 5L)
  expect_equal(out$nbi[4], 1L)
  expect_equal(out$xnbi[4], 1L)
})

test_that("NBI flags leave rows with missing components unevaluated", {
  out <- enemdu_build_nbi_flags(.nbi_test_data())

  expect_true(is.na(out$knbi[5]))
  expect_true(is.na(out$nbi[5]))
  expect_true(is.na(out$xnbi[5]))
})

test_that("NBI flags reject non-binary components in strict mode", {
  data <- .nbi_test_data()
  data$comp3[1] <- 2

  expect_error(
    enemdu_build_nbi_flags(data, strict_binary = TRUE),
    class = "enemdu_error_invalid_nbi_component"
  )
})

test_that("NBI flags safely coerce factor components with displayed 0 and 1 values", {
  data <- tibble::tibble(
    comp1 = factor(c("0", "1", "0", "1")),
    comp2 = factor(c("0", "0", "0", "0")),
    comp3 = factor(c("0", "0", "1", "1")),
    comp4 = factor(c("0", "0", "0", "0")),
    comp5 = factor(c("0", "0", "0", "0"))
  )

  out <- enemdu_build_nbi_flags(data)

  expect_equal(out$knbi, c(0L, 1L, 1L, 2L))
  expect_equal(out$nbi, c(0L, 1L, 1L, 1L))
  expect_equal(out$xnbi, c(0L, 0L, 0L, 1L))
})

test_that("NBI factor components do not use internal factor level codes", {
  data <- tibble::tibble(
    comp1 = factor(c("0", "1")),
    comp2 = factor(c("0", "0")),
    comp3 = factor(c("0", "0")),
    comp4 = factor(c("0", "0")),
    comp5 = factor(c("0", "0"))
  )

  out <- enemdu_build_nbi_flags(data)

  expect_equal(out$knbi, c(0L, 1L))
  expect_false(any(out$knbi == 2L))
})

test_that("NBI flags reject invalid factor component levels in strict mode", {
  data <- tibble::tibble(
    comp1 = factor(c("0", "yes")),
    comp2 = factor(c("0", "0")),
    comp3 = factor(c("0", "0")),
    comp4 = factor(c("0", "0")),
    comp5 = factor(c("0", "0"))
  )

  expect_error(
    enemdu_build_nbi_flags(data, strict_binary = TRUE),
    class = "enemdu_error_invalid_nbi_component"
  )
})

test_that("NBI flags protect existing outputs unless overwrite is requested", {
  data <- .nbi_test_data()
  data$knbi <- 99L

  expect_error(
    enemdu_build_nbi_flags(data),
    class = "enemdu_error_existing_nbi_output"
  )

  out <- enemdu_build_nbi_flags(data, overwrite = TRUE)

  expect_equal(out$knbi[1], 0L)
})

test_that("NBI consistency validation detects inconsistent flags", {
  data <- enemdu_build_nbi_flags(.nbi_test_data())
  data$nbi[3] <- 0L
  data$xnbi[4] <- 0L

  validation <- enemdu_validate_nbi_consistency(data)

  expect_s3_class(validation, "enemdu_nbi_consistency_validation")
  expect_equal(validation$inconsistencias_nbi, 1L)
  expect_equal(validation$inconsistencias_xnbi, 1L)
  expect_equal(validation$validation_status, "failed")
})

test_that("NBI consistency validation parses factor labels by value", {
  data <- enemdu_build_nbi_flags(.nbi_test_data())

  data$knbi <- factor(as.character(data$knbi), levels = c("0", "1", "2", "3", "4", "5"))
  data$nbi <- factor(as.character(data$nbi), levels = c("0", "1"))
  data$xnbi <- factor(as.character(data$xnbi), levels = c("0", "1"))

  validation <- enemdu_validate_nbi_consistency(data)

  expect_equal(validation$inconsistencias_nbi, 0L)
  expect_equal(validation$inconsistencias_xnbi, 0L)
  expect_equal(validation$validation_status, "passed")
})
