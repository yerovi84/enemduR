test_that("value range registry loads with expected columns", {
  registry <- enemdu_value_range_registry()

  expect_true(nrow(registry) > 0)

  expect_true(all(
    c(
      "variable",
      "variable_group",
      "min_value",
      "max_value",
      "allowed_values",
      "ignore_codes",
      "severity",
      "source_status",
      "description"
    ) %in% names(registry)
  ))
})

test_that("value range validation passes for valid values", {
  data <- tibble::tibble(
    p02 = c(1, 2, 1),
    fexp = c(10, 20, 30),
    hsize = c(1, 2, 3)
  )

  report <- enemdu_validate_value_ranges(
    data,
    vars = c("p02", "fexp", "hsize")
  )

  expect_s3_class(report, "enemdu_value_range_report")
  expect_true(all(report$status == "pass"))
})

test_that("value range validation fails for invalid allowed values", {
  data <- tibble::tibble(
    p02 = c(1, 2, 3)
  )

  report <- enemdu_validate_value_ranges(
    data,
    vars = "p02"
  )

  expect_equal(report$status, "fail")
  expect_equal(report$n_affected, 1)
})

test_that("value range validation ignores registered sentinel codes", {
  data <- tibble::tibble(
    p63 = c(100, 999999, 50)
  )

  report <- enemdu_validate_value_ranges(
    data,
    vars = "p63"
  )

  expect_equal(report$status, "pass")
})

test_that("value range validation catches negative income component", {
  data <- tibble::tibble(
    p63 = c(100, -5, 50)
  )

  report <- enemdu_validate_value_ranges(
    data,
    vars = "p63"
  )

  expect_equal(report$status, "fail")
  expect_equal(report$n_affected, 1)
})
