#' Preflight checks for NBI reproducibility workflows
#'
#' Reports whether the variables required for NBI reproducibility workflows are
#' present and usable. The function reports problems instead of stopping when
#' variables are absent.
#'
#' @param data A data frame.
#' @param component_vars Final NBI component variables.
#' @param area_var Urban/rural domain variable. Use `NULL` to omit area checks.
#' @param weight_var Survey expansion factor variable.
#' @param psu_var Primary sampling unit variable.
#' @param strata_var Survey strata variable.
#' @param required_vars Optional additional required variables.
#'
#' @return A tibble with one row per required variable and a
#' `preflight_passed` attribute.
#' @export
enemdu_validate_nbi_reproducibility_inputs <- function(data,
                                                       component_vars = c("comp1", "comp2", "comp3", "comp4", "comp5"),
                                                       area_var = "area",
                                                       weight_var = "fexp",
                                                       psu_var = "upm",
                                                       strata_var = "estrato",
                                                       required_vars = NULL) {
  if (!is.data.frame(data)) {
    .enemdu_abort_invalid_data(caller = "enemdu_validate_nbi_reproducibility_inputs")
  }

  spec <- .enemdu_nbi_reproducibility_required_spec(
    component_vars = component_vars,
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
      issue <- .enemdu_nbi_reproducibility_variable_issue(
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
  class(out) <- unique(c("enemdu_nbi_reproducibility_preflight", class(out)))
  out
}

#' Run an NBI reproducibility workflow
#'
#' Runs a local NBI workflow from final NBI components: preflight checks, NBI
#' flag construction, consistency validation, survey-design-aware estimates, and
#' benchmark comparison readiness.
#'
#' This workflow does not reconstruct NBI from raw questionnaire variables and
#' does not constitute official validation by INEC.
#'
#' @param data A data frame containing ENEMDU-like microdata.
#' @param period Optional benchmark period.
#' @param benchmark_set Optional benchmark set identifier.
#' @param survey_type ENEMDU survey type.
#' @param component_vars Final NBI component variables.
#' @param area_var Urban/rural domain variable.
#' @param ids Primary sampling unit variable passed to `enemdu_kpi_nbi()`.
#' @param strata Survey strata variable passed to `enemdu_kpi_nbi()`.
#' @param weight Survey expansion factor variable passed to `enemdu_kpi_nbi()`.
#' @param urban_values Values in `area_var` interpreted as urban.
#' @param rural_values Values in `area_var` interpreted as rural.
#' @param run_area Logical. If `TRUE`, estimate urban/rural domains.
#' @param benchmarks Official NBI benchmark table.
#' @param tolerance_pp Comparison tolerance in percentage points.
#' @param strict Logical. If `TRUE`, benchmark comparison errors on missing or
#' outside-tolerance rows.
#' @param run_preflight Logical. If `TRUE`, run input preflight checks first.
#' @param ... Additional arguments passed to `enemdu_kpi_nbi()`.
#'
#' @return A structured list with preflight, validation, estimates, benchmark
#' comparison, and validation-status metadata.
#' @export
enemdu_run_nbi_reproducibility <- function(data,
                                           period = NULL,
                                           benchmark_set = NULL,
                                           survey_type = "mensual",
                                           component_vars = c("comp1", "comp2", "comp3", "comp4", "comp5"),
                                           area_var = "area",
                                           ids = "upm",
                                           strata = "estrato",
                                           weight = "fexp",
                                           urban_values = c("urban", "urbano", "1", 1),
                                           rural_values = c("rural", "2", 2),
                                           run_area = TRUE,
                                           benchmarks = enemdu_official_nbi_benchmarks(),
                                           tolerance_pp = 0.10,
                                           strict = FALSE,
                                           run_preflight = TRUE,
                                           ...) {
  if (!is.data.frame(data)) {
    .enemdu_abort_invalid_data(caller = "enemdu_run_nbi_reproducibility")
  }

  preflight <- enemdu_validate_nbi_reproducibility_inputs(
    data = data,
    component_vars = component_vars,
    area_var = if (isTRUE(run_area)) area_var else NULL,
    weight_var = weight,
    psu_var = ids,
    strata_var = strata
  )

  if (isTRUE(run_preflight) && !isTRUE(attr(preflight, "preflight_passed"))) {
    .enemdu_abort_nbi_reproducibility_preflight(preflight)
  }

  if (!isTRUE(run_preflight)) {
    required_vars <- c(component_vars, weight, ids, strata)
    if (isTRUE(run_area)) {
      required_vars <- c(required_vars, area_var)
    }

    .enemdu_abort_missing_vars(
      vars = required_vars,
      names_data = names(data),
      caller = "enemdu_run_nbi_reproducibility"
    )
  }

  flagged_data <- enemdu_build_nbi_flags(
    data = data,
    component_vars = component_vars,
    overwrite = TRUE
  )

  validation <- enemdu_validate_nbi_consistency(flagged_data)

  national_estimates <- enemdu_kpi_nbi(
    data = flagged_data,
    component_vars = component_vars,
    build_flags = FALSE,
    survey_type = survey_type,
    ids = ids,
    strata = strata,
    weight = weight,
    official_validation_status = "not_officially_validated",
    ...
  )

  estimates <- national_estimates
  national_comparison <- enemdu_compare_official_nbi(
    estimates = national_estimates,
    benchmarks = benchmarks,
    period = period,
    benchmark_set = benchmark_set,
    tolerance_pp = tolerance_pp,
    strict = strict
  )

  comparisons <- list(national_comparison)

  if (isTRUE(run_area)) {
    flagged_data[[".enemdu_area_domain"]] <- .enemdu_nbi_area_domain(
      values = flagged_data[[area_var]],
      urban_values = urban_values,
      rural_values = rural_values
    )

    if (all(is.na(flagged_data[[".enemdu_area_domain"]]))) {
      rlang::abort(
        message = "No urban or rural values could be mapped from the requested area variable.",
        class = c("enemdu_error_nbi_reproducibility_area_mapping", "enemdu_error")
      )
    }

    area_estimates <- enemdu_kpi_nbi(
      data = flagged_data,
      group_vars = ".enemdu_area_domain",
      component_vars = component_vars,
      build_flags = FALSE,
      survey_type = survey_type,
      ids = ids,
      strata = strata,
      weight = weight,
      domain_level = "urbano_rural",
      domain_var = area_var,
      official_validation_status = "not_officially_validated",
      ...
    )
    area_estimates[["area"]] <- area_estimates[[".enemdu_area_domain"]]

    estimates <- .enemdu_bind_estimate_rows(list(estimates, area_estimates))

    comparisons[[2]] <- enemdu_compare_official_nbi(
      estimates = area_estimates,
      benchmarks = benchmarks,
      period = period,
      benchmark_set = benchmark_set,
      domain_vars = "area",
      tolerance_pp = tolerance_pp,
      strict = strict
    )
  }

  benchmark_comparison <- .enemdu_bind_estimate_rows(comparisons)

  out <- list(
    preflight = preflight,
    validation = validation,
    estimates = estimates,
    benchmark_comparison = benchmark_comparison,
    official_validation_status = "not_officially_validated",
    official_validation_note = paste(
      "This workflow prepares NBI benchmark comparison outputs.",
      "It is not an official validation claim."
    )
  )

  attr(out, "reproducibility_policy") <- list(
    period = period,
    benchmark_set = benchmark_set,
    survey_type = survey_type,
    component_vars = component_vars,
    area_var = area_var,
    ids = ids,
    strata = strata,
    weight = weight,
    run_area = run_area,
    tolerance_pp = tolerance_pp,
    strict = strict,
    note = paste(
      "NBI reproducibility uses final upstream component variables only.",
      "Official validation requires reviewed comparison against published INEC benchmarks."
    )
  )

  class(out) <- c("enemdu_nbi_reproducibility_result", "list")
  out
}

.enemdu_nbi_reproducibility_required_spec <- function(component_vars,
                                                      area_var,
                                                      weight_var,
                                                      psu_var,
                                                      strata_var,
                                                      required_vars) {
  variables <- c(component_vars, area_var, weight_var, psu_var, strata_var, required_vars)
  roles <- c(
    rep("component", length(component_vars)),
    if (is.null(area_var)) character(0) else "area",
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

.enemdu_nbi_reproducibility_variable_issue <- function(role,
                                                       values,
                                                       present) {
  if (!isTRUE(present)) {
    return("missing_variable")
  }

  if (sum(!is.na(values)) == 0) {
    return("all_missing")
  }

  if (grepl("component", role)) {
    numeric_values <- suppressWarnings(as.numeric(values[!is.na(values)]))
    if (any(is.na(numeric_values)) || any(!numeric_values %in% c(0, 1))) {
      return("non_binary_component")
    }
  }

  if (grepl("weight", role) && !is.numeric(values)) {
    return("not_numeric")
  }

  "ok"
}

.enemdu_abort_nbi_reproducibility_preflight <- function(preflight) {
  bad <- preflight[preflight$issue != "ok", , drop = FALSE]
  rlang::abort(
    message = glue::glue(
      "NBI reproducibility preflight failed for {nrow(bad)} variable(s)."
    ),
    class = c("enemdu_error_nbi_reproducibility_preflight_failed", "enemdu_error"),
    preflight = preflight
  )
}

.enemdu_nbi_area_domain <- function(values,
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
