#' Preflight checks for income poverty reproducibility workflows
#'
#' Reports whether the minimum variables required by the December 2025 income
#' poverty reproducibility workflow are present and usable. The function reports
#' problems instead of stopping when variables are absent.
#'
#' @param data A data frame.
#' @param income_var Household per-capita income variable.
#' @param area_var Urban/rural domain variable.
#' @param ids Primary sampling unit variable passed to
#' `enemdu_kpi_income_poverty()`.
#' @param strata Survey strata variable passed to
#' `enemdu_kpi_income_poverty()`.
#' @param weight Survey expansion factor variable passed to
#' `enemdu_kpi_income_poverty()`.
#' @param weight_var Survey expansion factor variable.
#' @param psu_var Primary sampling unit variable.
#' @param strata_var Survey strata variable.
#' @param required_vars Optional additional required variables.
#'
#' @return A tibble with one row per required variable and a `preflight_passed`
#' attribute.
#' @export
enemdu_validate_poverty_reproducibility_inputs <- function(data,
                                                           income_var = "ingtot_pc",
                                                           area_var = "area",
                                                           weight_var = "fexp",
                                                           psu_var = "upm",
                                                           strata_var = "estrato",
                                                           required_vars = NULL) {
  if (!is.data.frame(data)) {
    .enemdu_abort_invalid_data(caller = "enemdu_validate_poverty_reproducibility_inputs")
  }

  spec <- .enemdu_poverty_reproducibility_required_spec(
    income_var = income_var,
    area_var = area_var,
    weight_var = weight_var,
    psu_var = psu_var,
    strata_var = strata_var,
    required_vars = required_vars
  )

  rows <- vector("list", nrow(spec))

  for (i in seq_len(nrow(spec))) {
    variable <- spec$variable[[i]]
    role <- spec$role[[i]]
    present <- variable %in% names(data)

    if (isTRUE(present)) {
      values <- data[[variable]]
      class_value <- paste(class(values), collapse = "|")
      missing_n <- sum(is.na(values))
      non_missing_n <- sum(!is.na(values))
      issue <- .enemdu_poverty_reproducibility_variable_issue(
        role = role,
        values = values,
        present = present
      )
    } else {
      class_value <- NA_character_
      missing_n <- NA_integer_
      non_missing_n <- NA_integer_
      issue <- "missing_variable"
    }

    rows[[i]] <- tibble::tibble(
      variable = variable,
      role = role,
      present = present,
      class = class_value,
      missing_n = as.integer(missing_n),
      non_missing_n = as.integer(non_missing_n),
      issue = issue
    )
  }

  out <- do.call(rbind, rows)
  out <- tibble::as_tibble(out)

  attr(out, "preflight_passed") <- all(out$issue == "ok")
  class(out) <- unique(c("enemdu_poverty_reproducibility_preflight", class(out)))
  out
}

#' Run December 2025 income poverty reproducibility workflow
#'
#' Runs a local reproducibility workflow for December 2025 income poverty and
#' extreme income poverty estimates. The workflow estimates national and
#' urban/rural domains using explicit published poverty lines, then compares the
#' package estimates against published official benchmarks.
#'
#' This workflow does not derive poverty lines from CPI, does not commit
#' microdata, and does not constitute official validation by INEC.
#'
#' @param data A data frame containing ENEMDU-like microdata.
#' @param period Benchmark period.
#' @param benchmark_set Benchmark set identifier.
#' @param survey_type ENEMDU survey type.
#' @param income_var Household per-capita income variable.
#' @param area_var Urban/rural domain variable.
#' @param urban_values Values in `area_var` interpreted as urban.
#' @param rural_values Values in `area_var` interpreted as rural.
#' @param poverty_line Explicit poverty line for the period.
#' @param extreme_poverty_line Explicit extreme poverty line for the period.
#' @param line_source Source note for the explicit poverty lines.
#' @param tolerance_pp Comparison tolerance in percentage points.
#' @param strict Logical. If `TRUE`, errors when benchmark comparison finds
#' missing estimates or values outside tolerance.
#' @param run_preflight Logical. If `TRUE`, run input preflight checks first.
#' @param ... Additional arguments passed to `enemdu_kpi_income_poverty()`.
#'
#' @return A tibble with benchmark comparison rows and reproducibility metadata.
#' @export
#'
#' @examples
#' \dontrun{
#' data <- tibble::tibble(
#'   idhogar = paste0("h", 1:4),
#'   hsize = rep(1L, 4),
#'   upm = 1:4,
#'   estrato = c(1, 1, 2, 2),
#'   fexp = rep(1, 4),
#'   area = c("1", "1", "2", "2"),
#'   ingtot_pc = c(80, 120, 40, 140)
#' )
#'
#' enemdu_run_poverty_reproducibility(data, sample_n_min = 1)
#' }
enemdu_run_poverty_reproducibility <- function(data,
                                               period = "2025-12",
                                               benchmark_set = "income_poverty_december_2025",
                                               survey_type = "mensual",
                                               income_var = "ingtot_pc",
                                               area_var = "area",
                                               ids = "upm",
                                               strata = "estrato",
                                               weight = "fexp",
                                               urban_values = c("urban", "urbano", "1", 1),
                                               rural_values = c("rural", "2", 2),
                                               poverty_line = 92.40,
                                               extreme_poverty_line = 52.07,
                                               line_source = "INEC published ENEMDU poverty and inequality report, December 2025.",
                                               tolerance_pp = 0.10,
                                               strict = FALSE,
                                               run_preflight = TRUE,
                                               ...) {
  if (!is.data.frame(data)) {
    .enemdu_abort_invalid_data(caller = "enemdu_run_poverty_reproducibility")
  }

  preflight <- enemdu_validate_poverty_reproducibility_inputs(
    data = data,
    income_var = income_var,
    area_var = area_var,
    weight_var = weight,
    psu_var = ids,
    strata_var = strata
  )

  if (isTRUE(run_preflight) && !isTRUE(attr(preflight, "preflight_passed"))) {
    .enemdu_abort_poverty_reproducibility_preflight(preflight)
  }

  if (!isTRUE(run_preflight)) {
    .enemdu_abort_missing_vars(
      vars = c(income_var, area_var, weight, ids, strata),
      names_data = names(data),
      caller = "enemdu_run_poverty_reproducibility"
    )
  }

  work_data <- data
  work_data[[".enemdu_area_domain"]] <- .enemdu_poverty_reproducibility_area_domain(
    values = work_data[[area_var]],
    urban_values = urban_values,
    rural_values = rural_values
  )

  if (all(is.na(work_data[[".enemdu_area_domain"]]))) {
    rlang::abort(
      message = "No urban or rural values could be mapped from the requested area variable.",
      class = c("enemdu_error_poverty_reproducibility_area_mapping", "enemdu_error")
    )
  }

  national_estimates <- enemdu_kpi_income_poverty(
    data = work_data,
    period = period,
    mode = "manual",
    income_var = income_var,
    ids = ids,
    strata = strata,
    weight = weight,
    poverty_line = poverty_line,
    extreme_poverty_line = extreme_poverty_line,
    line_source = line_source,
    survey_type = survey_type,
    official_validation_status = "not_officially_validated",
    ...
  )

  area_estimates <- enemdu_kpi_income_poverty(
    data = work_data,
    group_vars = ".enemdu_area_domain",
    period = period,
    mode = "manual",
    income_var = income_var,
    ids = ids,
    strata = strata,
    weight = weight,
    poverty_line = poverty_line,
    extreme_poverty_line = extreme_poverty_line,
    line_source = line_source,
    survey_type = survey_type,
    domain_level = "urbano_rural",
    domain_var = area_var,
    official_validation_status = "not_officially_validated",
    ...
  )
  area_estimates[["area"]] <- area_estimates[[".enemdu_area_domain"]]

  national_comparison <- enemdu_compare_official_poverty(
    estimates = national_estimates,
    period = period,
    benchmark_set = benchmark_set,
    tolerance_pp = tolerance_pp,
    strict = strict
  )

  area_comparison <- enemdu_compare_official_poverty(
    estimates = area_estimates,
    period = period,
    benchmark_set = benchmark_set,
    domain_vars = "area",
    tolerance_pp = tolerance_pp,
    strict = strict
  )

  result <- .enemdu_bind_estimate_rows(list(national_comparison, area_comparison))
  result[["reproducibility_scope"]] <- ifelse(
    result$domain_type == "national",
    "national",
    "urban_rural"
  )
  result[["reproducibility_status"]] <- .enemdu_poverty_reproducibility_status(
    result$comparison_status
  )
  result[["official_validation_status"]] <- "not_officially_validated"
  result[["official_validation_note"]] <- paste(
    "This workflow compares package estimates against published benchmarks.",
    "It is not an official validation claim."
  )

  result <- tibble::as_tibble(result)
  class(result) <- unique(c(
    "enemdu_poverty_reproducibility_result",
    "enemdu_official_poverty_comparison",
    class(result)
  ))

  attr(result, "reproducibility_policy") <- list(
    period = period,
    benchmark_set = benchmark_set,
    survey_type = survey_type,
    income_var = income_var,
    area_var = area_var,
    poverty_line = poverty_line,
    extreme_poverty_line = extreme_poverty_line,
    line_source = line_source,
    tolerance_pp = tolerance_pp,
    strict = strict,
    preflight = preflight,
    note = paste(
      "December 2025 income poverty reproducibility scaffold.",
      "Published lines are supplied explicitly.",
      "Benchmark comparison does not imply official validation."
    )
  )

  result
}

.enemdu_poverty_reproducibility_required_spec <- function(income_var,
                                                          area_var,
                                                          weight_var,
                                                          psu_var,
                                                          strata_var,
                                                          required_vars) {
  variables <- c(income_var, area_var, weight_var, psu_var, strata_var, required_vars)
  roles <- c(
    "income",
    "area",
    "weight",
    "psu",
    "strata",
    rep("required", length(required_vars))
  )

  unique_variables <- unique(as.character(variables))
  rows <- vector("list", length(unique_variables))

  for (i in seq_along(unique_variables)) {
    variable <- unique_variables[[i]]
    rows[[i]] <- tibble::tibble(
      variable = variable,
      role = paste(unique(roles[variables == variable]), collapse = "|")
    )
  }

  do.call(rbind, rows)
}

.enemdu_poverty_reproducibility_variable_issue <- function(role,
                                                           values,
                                                           present) {
  if (!isTRUE(present)) {
    return("missing_variable")
  }

  if (sum(!is.na(values)) == 0) {
    return("all_missing")
  }

  if (grepl("income|weight", role) && !is.numeric(values)) {
    return("not_numeric")
  }

  "ok"
}

.enemdu_abort_poverty_reproducibility_preflight <- function(preflight) {
  bad <- preflight[preflight$issue != "ok", , drop = FALSE]
  rlang::abort(
    message = glue::glue(
      "Poverty reproducibility preflight failed for {nrow(bad)} variable(s)."
    ),
    class = c("enemdu_error_poverty_reproducibility_preflight_failed", "enemdu_error"),
    preflight = preflight
  )
}

.enemdu_poverty_reproducibility_area_domain <- function(values,
                                                        urban_values,
                                                        rural_values) {
  values_chr <- tolower(trimws(as.character(values)))
  urban_chr <- tolower(trimws(as.character(urban_values)))
  rural_chr <- tolower(trimws(as.character(rural_values)))

  out <- rep(NA_character_, length(values_chr))
  out[values_chr %in% urban_chr] <- "urban"
  out[values_chr %in% rural_chr] <- "rural"
  out
}

.enemdu_poverty_reproducibility_status <- function(comparison_status) {
  out <- rep("benchmark_comparison_requires_review", length(comparison_status))
  out[comparison_status %in% c("matched_reported_rounding", "within_tolerance")] <-
    "benchmark_comparison_within_tolerance"
  out[comparison_status %in% c("missing_official_benchmark", "missing_package_estimate")] <-
    "benchmark_comparison_incomplete"
  out[comparison_status %in% "outside_tolerance"] <-
    "benchmark_difference_requires_review"
  out
}
