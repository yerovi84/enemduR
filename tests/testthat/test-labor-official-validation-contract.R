test_that("labor official validation contract is available", {
  path <- system.file(
    "extdata",
    "labor_official_validation_contract.md",
    package = "enemduR"
  )

  expect_true(nzchar(path))
  expect_true(file.exists(path))
})

test_that("labor official validation contract documents critical validation decisions", {
  path <- system.file(
    "extdata",
    "labor_official_validation_contract.md",
    package = "enemduR"
  )

  content <- readLines(path, warn = FALSE, encoding = "UTF-8")

  required_patterns <- c(
    "Labor Official Validation Contract",
    "tolerance_rate = 0.0006",
    "0.06 percentage points",
    "dominio",
    "ciudad",
    "match_official_dash_zero",
    "Santo Domingo de los Tsáchilas",
    "Santo Domingo",
    "R-4-001",
    "closed for the initial labor-indicator block"
  )

  for (pattern in required_patterns) {
    expect_true(
      any(grepl(pattern, content, fixed = TRUE)),
      info = paste("Missing documentation pattern:", pattern)
    )
  }
})
