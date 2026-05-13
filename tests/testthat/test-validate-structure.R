test_that("structural validation passes when design vars are present", {
  data <- tibble::tibble(
    upm = 1:3,
    estrato = c(1, 1, 2),
    fexp = c(1.2, 0.9, 1.1)
  )

  report <- enemduR::enemdu_validate_structure(data)

  expect_s3_class(report, "enemdu_validation_report")
  expect_true(all(report$status[1:2] == "pass"))
})

test_that("structural validation fails when design vars are absent", {
  data <- tibble::tibble(
    hogar = 1:3,
    ingreso = c(100, 200, 300)
  )

  report <- enemduR::enemdu_validate_structure(data)

  expect_s3_class(report, "enemdu_validation_report")
  expect_equal(report$status[2], "fail")
  expect_true(grepl("Missing design variables", report$message[2]))
})
