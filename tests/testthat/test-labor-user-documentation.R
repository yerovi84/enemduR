test_that("labor user documentation file exists", {
  doc_path <- file.path(
    testthat::test_path(),
    "..",
    "..",
    "vignettes",
    "labor-indicators.md"
  )

  doc_path <- normalizePath(doc_path, winslash = "/", mustWork = FALSE)

  expect_true(file.exists(doc_path))
})

test_that("labor user documentation records user-facing validation decisions", {
  doc_path <- file.path(
    testthat::test_path(),
    "..",
    "..",
    "vignettes",
    "labor-indicators.md"
  )

  doc_path <- normalizePath(doc_path, winslash = "/", mustWork = TRUE)
  content <- readLines(doc_path, warn = FALSE, encoding = "UTF-8")

  required_patterns <- c(
    "# Labor indicators in enemduR",
    "enemdu_kpi_employment()",
    "condact",
    "domain_scope",
    "dominio",
    "ciudad",
    "tolerance_rate = 0.0006",
    "match_official_dash_zero",
    "Santo Domingo",
    ".sav",
    "does not reconstruct labor status from raw questionnaire variables",
    "reproducible validation against published INEC outputs"
  )

  for (pattern in required_patterns) {
    expect_true(
      any(grepl(pattern, content, fixed = TRUE)),
      info = paste("Missing documentation pattern:", pattern)
    )
  }
})
