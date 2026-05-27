.ipm_extdata_path <- function(file) {
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

.read_ipm_registry <- function(file) {
  read.csv(
    .ipm_extdata_path(file),
    stringsAsFactors = FALSE,
    check.names = FALSE,
    fileEncoding = "UTF-8"
  )
}

.readLines_ipm_contract <- function() {
  readLines(
    .ipm_extdata_path("ipm_derivation_contract.md"),
    warn = FALSE,
    encoding = "UTF-8"
  )
}

test_that("IPM component registry exists and defines the official structure", {
  path <- .ipm_extdata_path("ipm_component_registry.csv")

  expect_true(file.exists(path))

  registry <- .read_ipm_registry("ipm_component_registry.csv")

  expect_equal(nrow(registry), 12)
  expect_equal(anyDuplicated(registry$indicator_id), 0L)
  expect_equal(registry$indicator_order, 1:12)
  expect_equal(sum(registry$indicator_weight), 1, tolerance = 1e-12)

  dimension_weights <- unique(registry[c("dimension_id", "dimension_weight")])

  expect_equal(nrow(dimension_weights), 4)
  expect_equal(sum(dimension_weights$dimension_weight), 1, tolerance = 1e-12)
  expect_true(all(registry$derivation_status == "pending_implementation"))
})

test_that("IPM derivation registry is non-operational and unvalidated", {
  path <- .ipm_extdata_path("ipm_derivation_registry.csv")

  expect_true(file.exists(path))

  registry <- .read_ipm_registry("ipm_derivation_registry.csv")
  profile_rows <- registry[registry$profile == "enemdu_2025_anual", , drop = FALSE]

  expect_equal(nrow(profile_rows), 12)
  expect_true(all(profile_rows$rule_status == "conceptual_contract_only"))
  expect_true(all(profile_rows$official_alignment_status == "pending_real_data_validation"))
  expect_false(any(profile_rows$official_alignment_status == "officially_validated"))
})

test_that("IPM derivation registry preserves critical source-variable contracts", {
  registry <- .read_ipm_registry("ipm_derivation_registry.csv")

  logro_educativo <- registry[
    registry$indicator_id == "ipm_i03_logro_educativo_incompleto" &
      registry$profile == "enemdu_2025_anual",
    ,
    drop = FALSE
  ]

  saneamiento <- registry[
    registry$indicator_id == "ipm_i11_sin_saneamiento_excretas" &
      registry$profile == "enemdu_2025_anual",
    ,
    drop = FALSE
  ]

  expect_equal(nrow(logro_educativo), 1)
  expect_equal(nrow(saneamiento), 1)

  logro_required_variables <- strsplit(
    logro_educativo$required_variables,
    ";",
    fixed = TRUE
  )[[1]]

  saneamiento_required_variables <- strsplit(
    saneamiento$required_variables,
    ";",
    fixed = TRUE
  )[[1]]

  expect_true("p07" %in% logro_required_variables)
  expect_true("area" %in% saneamiento_required_variables)
})

test_that("IPM registries align one derivation row to each component", {
  components <- .read_ipm_registry("ipm_component_registry.csv")
  derivations <- .read_ipm_registry("ipm_derivation_registry.csv")

  expect_setequal(derivations$indicator_id, components$indicator_id)
  expect_false(any(components$derivation_status == "officially_validated"))
  expect_false(any(derivations$rule_status == "implemented"))
})

test_that("IPM contract documents cutoffs and non-official status", {
  contract <- .readLines_ipm_contract()
  contract_text <- paste(contract, collapse = "\n")
  expected_outputs <- c("ipm_score", "tpm", "tpem", "A", "ipm")

  expect_true(any(grepl("TPM cutoff", contract, fixed = TRUE)))
  expect_true(any(grepl("TPEM cutoff", contract, fixed = TRUE)))
  expect_true(any(grepl("0.3333333333333333", contract, fixed = TRUE)))
  expect_true(any(grepl("0.5", contract, fixed = TRUE)))
  expect_true(all(vapply(
    expected_outputs,
    function(output) any(grepl(output, contract, fixed = TRUE)),
    logical(1)
  )))
  expect_true(grepl("does not implement IPM", contract_text, fixed = TRUE))
  expect_true(grepl("does not claim official institutional validation", contract_text, fixed = TRUE))
  expect_true(grepl("published benchmarks", contract_text, fixed = TRUE))
})
