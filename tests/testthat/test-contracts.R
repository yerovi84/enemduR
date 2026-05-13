test_that("kpi output contract is available", {
  contract <- enemdu_output_contract("kpi")

  expect_s3_class(contract, "enemdu_output_contract")
  expect_true("indicator_id" %in% contract$column)
  expect_true("estimate" %in% contract$column)
  expect_true("quality_flag" %in% contract$column)
})

test_that("representativity output contract is available", {
  contract <- enemdu_output_contract("representativity")

  expect_s3_class(contract, "enemdu_output_contract")
  expect_true("effective_n" %in% contract$column)
  expect_true("degrees_freedom" %in% contract$column)
  expect_true("decision" %in% contract$column)
})

test_that("invalid output contract type fails", {
  expect_error(
    enemdu_output_contract("invalid_type")
  )
})
