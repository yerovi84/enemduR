test_that("missing code registry loads with expected columns", {
  registry <- enemdu_missing_code_registry()

  expect_true(nrow(registry) > 0)

  expect_true(all(
    c(
      "variable",
      "variable_group",
      "code",
      "code_type",
      "meaning",
      "action",
      "applies_to",
      "source_status",
      "source_note"
    ) %in% names(registry)
  ))
})

test_that("sentinel values are detected without modifying data", {
  data <- tibble::tibble(
    p63 = c(100, 999999, 50),
    p66 = c(999, 200, NA_real_)
  )

  report <- enemdu_detect_sentinel_values(
    data,
    vars = c("p63", "p66"),
    applies_to = "income_derivation"
  )

  expect_s3_class(report, "enemdu_sentinel_report")
  expect_true(any(report$variable == "p63" & report$code == "999999"))
  expect_true(any(report$variable == "p66" & report$code == "999"))
  expect_equal(data$p63[2], 999999)
})

test_that("missing report includes system missing and sentinel values", {
  data <- tibble::tibble(
    p63 = c(100, 999999, NA_real_)
  )

  report <- enemdu_missing_report(
    data,
    vars = "p63",
    applies_to = "income_derivation"
  )

  expect_s3_class(report, "enemdu_missing_report")
  expect_true(any(report$code_type == "system_missing"))
  expect_true(any(report$code_type == "special_missing"))
})

test_that("normalization replaces only registered set_na codes", {
  data <- tibble::tibble(
    p63 = c(100, 999999, 50),
    p78 = c(100, 999999, 50)
  )

  out <- enemdu_normalize_missing_values(
    data,
    vars = c("p63", "p78"),
    applies_to = "income_derivation",
    keep_raw = TRUE,
    create_flags = TRUE
  )

  expect_true(is.na(out$p63[2]))
  expect_equal(out$p78[2], 999999)
  expect_true("p63_raw" %in% names(out))
  expect_true("p63_missing_flag" %in% names(out))
  expect_true(out$p63_missing_flag[2])
  expect_false(out$p63_missing_flag[1])
})

test_that("normalization stores an audit log", {
  data <- tibble::tibble(
    p63 = c(100, 999999, 50)
  )

  out <- enemdu_normalize_missing_values(
    data,
    vars = "p63",
    applies_to = "income_derivation"
  )

  log <- attr(out, "missing_normalization_log")

  expect_true(is.data.frame(log))
  expect_true("n_replaced" %in% names(log))
  expect_true(any(log$n_replaced > 0))
})
