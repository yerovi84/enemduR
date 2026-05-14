#' Estimate an ENEMDU indicator from the indicator registry
#'
#' Estimates an indicator declared in `indicator_registry.csv` by selecting the
#' appropriate survey estimator according to the indicator contract.
#'
#' Supported estimator types in this phase are:
#'
#' - `mean`
#' - `total`
#' - `proportion_0_1`
#'
#' Indicators with `scale_adjustment_required = TRUE` receive an internal
#' household-scale correction for the precision decision when the adjustment is
#' enabled. This does not change the point estimate. It adjusts the effective
#' analytical size used for the representativity decision by summing `1 / hsize`
#' over valid repeated household-level observations.
#'
#' When `integrate_representativity = TRUE`, the function also validates the
#' requested domain against the representativity scope of the ENEMDU survey type
#' and appends a final `representativity_flag`.
#'
#' @param data A data frame.
#' @param indicator_id Indicator identifier declared in `indicator_registry.csv`.
#' @param group_vars Optional grouping variables.
#' @param registry Indicator registry. Defaults to package registry.
#' @param value Optional value variable override.
#' @param ids Primary sampling unit variable. Defaults to `"upm"`.
#' @param strata Strata variable. Defaults to `"estrato"`.
#' @param weight Optional weight variable override. If `NULL`, uses registry
#' value.
#' @param survey_type Optional ENEMDU survey type. If omitted, uses the
#' `survey_type` attribute when available.
#' @param domain_level Optional domain level. If omitted and `group_vars` is
#' supplied, the domain level is inferred from `domain_variable_registry.csv`.
#' @param domain_var Optional domain variable. Usually omitted because
#' `group_vars` is enough for inference.
#' @param strict_domain Logical. If `TRUE`, errors when requested domains are
#' outside the design scope of the selected ENEMDU survey type.
#' @param integrate_representativity Logical. If `TRUE`, appends integrated
#' representativity metadata to the estimate.
#' @param household_id Household identifier used when household-scale adjustment
#' is needed.
#' @param hsize Household-size variable.
#' @param scale_adjustment One of `"metadata"`, `"never"` or `"always"`.
#' `"metadata"` applies the adjustment only when the registry requests it.
#' @param conf_level Confidence level.
#' @param lonely_psu Option passed to `survey.lonely.psu`.
#' @param sample_n_min Preliminary minimum sample size flag.
#'
#' @return A tibble with the design-based estimate, registry metadata, and
#' integrated representativity metadata when available.
#' @export
enemdu_indicator_estimate <- function(data,
                                      indicator_id,
                                      group_vars = NULL,
                                      registry = enemdu_indicator_registry(),
                                      value = NULL,
                                      ids = "upm",
                                      strata = "estrato",
                                      weight = NULL,
                                      survey_type = NULL,
                                      domain_level = NULL,
                                      domain_var = NULL,
                                      strict_domain = FALSE,
                                      integrate_representativity = TRUE,
                                      household_id = "idhogar",
                                      hsize = "hsize",
                                      scale_adjustment = c("metadata", "never", "always"),
                                      conf_level = 0.95,
                                      lonely_psu = "adjust",
                                      sample_n_min = 60) {
  if (!is.data.frame(data)) {
    .enemdu_abort_invalid_data(caller = "enemdu_indicator_estimate")
  }

  if (missing(indicator_id) || is.null(indicator_id) || length(indicator_id) != 1) {
    .enemdu_abort_missing_argument(
      "indicator_id",
      caller = "enemdu_indicator_estimate"
    )
  }

  scale_adjustment <- match.arg(scale_adjustment)

  .enemdu_validate_indicator_registry_for_estimation(registry)

  indicator_row <- registry[registry$indicator_id == indicator_id, , drop = FALSE]

  if (nrow(indicator_row) != 1) {
    rlang::abort(
      message = glue::glue(
        "Indicator `{indicator_id}` was not found uniquely in `indicator_registry.csv`."
      ),
      class = c("enemdu_error_invalid_indicator_id", "enemdu_error")
    )
  }

  estimator_type <- as.character(indicator_row$estimator_type[[1]])

  if (!estimator_type %in% c("mean", "total", "proportion_0_1")) {
    rlang::abort(
      message = glue::glue(
        "Indicator `{indicator_id}` has estimator_type `{estimator_type}`, ",
        "which is not supported by `enemdu_indicator_estimate()` in this phase."
      ),
      class = c("enemdu_error_unsupported_estimator_type", "enemdu_error")
    )
  }

  if (is.null(weight)) {
    weight <- as.character(indicator_row$weight[[1]])
  }

  if (is.na(weight) || !nzchar(weight)) {
    weight <- "fexp"
  }

  resolved_survey_type <- .enemdu_resolve_indicator_survey_type(
    data = data,
    survey_type = survey_type
  )

  value_var <- .enemdu_resolve_indicator_value_var(
    data = data,
    indicator_row = indicator_row,
    value = value
  )

  working_data <- data
  created_count_value <- FALSE

  if (identical(value_var, ".enemdu_count_value")) {
    working_data[[value_var]] <- 1
    created_count_value <- TRUE
  }

  measure <- as.character(indicator_row$indicator_label[[1]])
  if (is.na(measure) || !nzchar(measure)) {
    measure <- indicator_id
  }

  estimate <- switch(
    estimator_type,
    mean = enemdu_survey_mean(
      data = working_data,
      value = value_var,
      group_vars = group_vars,
      ids = ids,
      strata = strata,
      weight = weight,
      survey_type = resolved_survey_type,
      indicator_id = indicator_id,
      measure = measure,
      conf_level = conf_level,
      lonely_psu = lonely_psu,
      sample_n_min = sample_n_min
    ),
    total = enemdu_survey_total(
      data = working_data,
      value = value_var,
      group_vars = group_vars,
      ids = ids,
      strata = strata,
      weight = weight,
      survey_type = resolved_survey_type,
      indicator_id = indicator_id,
      measure = measure,
      conf_level = conf_level,
      lonely_psu = lonely_psu,
      sample_n_min = sample_n_min
    ),
    proportion_0_1 = enemdu_survey_proportion(
      data = working_data,
      value = value_var,
      group_vars = group_vars,
      ids = ids,
      strata = strata,
      weight = weight,
      survey_type = resolved_survey_type,
      indicator_id = indicator_id,
      measure = measure,
      conf_level = conf_level,
      lonely_psu = lonely_psu,
      sample_n_min = sample_n_min
    )
  )

  scale_required <- .enemdu_as_logical(indicator_row$scale_adjustment_required[[1]])

  apply_scale <- identical(scale_adjustment, "always") ||
    (identical(scale_adjustment, "metadata") && isTRUE(scale_required))

  estimate <- .enemdu_attach_indicator_metadata(
    estimate = estimate,
    indicator_row = indicator_row,
    value_var = value_var,
    created_count_value = created_count_value,
    weight = weight
  )

  if (isTRUE(apply_scale)) {
    estimate <- .enemdu_apply_scale_adjusted_precision(
      estimate = estimate,
      data = working_data,
      value_var = value_var,
      group_vars = group_vars,
      household_id = household_id,
      hsize = hsize,
      estimator_type = estimator_type,
      sample_n_min = sample_n_min
    )
  } else {
    estimate[["household_scale_adjustment_required"]] <- scale_required
    estimate[["household_scale_adjustment_applied_to_precision"]] <- FALSE
    estimate[["adjusted_unweighted_n"]] <- NA_real_
    estimate[["adjusted_effective_n"]] <- NA_real_
    estimate[["scale_adjustment_note"]] <- if (isTRUE(scale_required)) {
      "Household-scale adjustment is required by metadata but was not applied."
    } else {
      "Household-scale adjustment is not required by metadata."
    }
  }

  if (isTRUE(integrate_representativity)) {
    estimate <- enemdu_representativity_report(
      estimate = estimate,
      survey_type = resolved_survey_type,
      domain_level = domain_level,
      domain_var = domain_var,
      group_vars = group_vars,
      strict_domain = strict_domain
    )
  } else {
    estimate[["representativity_flag"]] <- NA_character_
    estimate[["representativity_note"]] <- "Integrated representativity report was not requested."
  }

  attr(estimate, "indicator_estimation_policy") <- list(
    indicator_id = indicator_id,
    value_var = value_var,
    estimator_type = estimator_type,
    weight = weight,
    survey_type = resolved_survey_type,
    domain_level = domain_level,
    domain_var = domain_var,
    group_vars = group_vars,
    strict_domain = strict_domain,
    integrate_representativity = integrate_representativity,
    scale_adjustment = scale_adjustment,
    scale_required = scale_required,
    scale_applied = apply_scale,
    note = paste(
      "Indicator estimated from indicator registry.",
      "Point estimates use survey design.",
      "When scale adjustment is applied, it affects the precision decision, not the point estimate.",
      "When integrated representativity is enabled, the output combines precision decision and domain scope."
    )
  )

  class(estimate) <- unique(c("enemdu_indicator_estimate", class(estimate)))
  estimate
}

#' Estimate core income KPIs using survey design
#'
#' Produces inferential KPI estimates for income indicators already available in
#' the data, especially household per-capita income.
#'
#' @param data A data frame.
#' @param group_vars Optional grouping variables.
#' @param income_var Per-capita income variable. Defaults to `"ingtot_pc"`.
#' @param registry Indicator registry.
#' @param ... Additional arguments passed to `enemdu_indicator_estimate()`.
#'
#' @return A tibble of income KPI estimates.
#' @export
enemdu_kpi_income <- function(data,
                              group_vars = NULL,
                              income_var = "ingtot_pc",
                              registry = enemdu_indicator_registry(),
                              ...) {
  if (!is.data.frame(data)) {
    .enemdu_abort_invalid_data(caller = "enemdu_kpi_income")
  }

  outputs <- list()
  i <- 1L

  if (income_var %in% names(data)) {
    outputs[[i]] <- enemdu_indicator_estimate(
      data = data,
      indicator_id = "ingreso_percapita_familiar",
      group_vars = group_vars,
      registry = registry,
      value = income_var,
      ...
    )
    i <- i + 1L
  }

  if ("ingrl" %in% names(data)) {
    outputs[[i]] <- enemdu_indicator_estimate(
      data = data,
      indicator_id = "ingreso_laboral",
      group_vars = group_vars,
      registry = registry,
      value = "ingrl",
      ...
    )
    i <- i + 1L
  }

  if (length(outputs) == 0) {
    return(.enemdu_empty_indicator_estimate())
  }

  .enemdu_bind_estimate_rows(outputs)
}

#' Estimate optional bonus KPIs using survey design
#'
#' Produces inferential KPI estimates for optional bonus variables such as `p78`
#' after they have been transformed with `enemdu_build_optional_bonuses()`. If
#' transformed variables are absent and `p78` is present, the function can build
#' optional bonus variables automatically without creating income scenarios.
#'
#' @param data A data frame.
#' @param group_vars Optional grouping variables.
#' @param build_if_missing Logical. If `TRUE`, calls
#' `enemdu_build_optional_bonuses()` when optional bonus variables are missing.
#' @param registry Indicator registry.
#' @param ... Additional arguments passed to `enemdu_indicator_estimate()`.
#'
#' @return A tibble of optional bonus KPI estimates.
#' @export
enemdu_kpi_optional_bonuses <- function(data,
                                        group_vars = NULL,
                                        build_if_missing = TRUE,
                                        registry = enemdu_indicator_registry(),
                                        ...) {
  if (!is.data.frame(data)) {
    .enemdu_abort_invalid_data(caller = "enemdu_kpi_optional_bonuses")
  }

  out_data <- data

  needed <- c("bono_jgl", "bono_jgl_recibe", "bonos_optional_total")

  if (!all(needed %in% names(out_data)) && isTRUE(build_if_missing) && "p78" %in% names(out_data)) {
    out_data <- enemdu_build_optional_bonuses(
      data = out_data,
      create_income_scenario = FALSE,
      strict = FALSE
    )
  }

  outputs <- list()
  i <- 1L

  if ("bonos_optional_total" %in% names(out_data)) {
    outputs[[i]] <- enemdu_indicator_estimate(
      data = out_data,
      indicator_id = "transferencias_bonos_total_enemdu",
      group_vars = group_vars,
      registry = registry,
      value = "bonos_optional_total",
      scale_adjustment = "never",
      ...
    )
    i <- i + 1L
  }

  if ("bono_jgl_recibe" %in% names(out_data)) {
    outputs[[i]] <- enemdu_indicator_estimate(
      data = out_data,
      indicator_id = "bono_jgl_receptores",
      group_vars = group_vars,
      registry = registry,
      value = "bono_jgl_recibe",
      scale_adjustment = "never",
      ...
    )
    i <- i + 1L
  }

  if (all(c("bono_jgl", "bono_jgl_recibe") %in% names(out_data))) {
    out_data[["bono_jgl_monto_receptor"]] <- ifelse(
      !is.na(out_data[["bono_jgl_recibe"]]) &
        out_data[["bono_jgl_recibe"]] == 1,
      out_data[["bono_jgl"]],
      NA_real_
    )

    outputs[[i]] <- enemdu_indicator_estimate(
      data = out_data,
      indicator_id = "bono_jgl_monto_promedio_receptor",
      group_vars = group_vars,
      registry = registry,
      value = "bono_jgl_monto_receptor",
      scale_adjustment = "never",
      ...
    )
    i <- i + 1L
  }

  if (length(outputs) == 0) {
    return(.enemdu_empty_indicator_estimate())
  }

  result <- .enemdu_bind_estimate_rows(outputs)
  attr(result, "optional_bonus_kpi_policy") <- list(
    build_if_missing = build_if_missing,
    note = paste(
      "Optional bonus estimates are survey-based estimates from ENEMDU.",
      "They must not be interpreted directly as administrative execution."
    )
  )

  result
}

.enemdu_validate_indicator_registry_for_estimation <- function(registry) {
  required_cols <- c(
    "indicator_id",
    "indicator_label",
    "indicator_group",
    "unit",
    "analysis_level",
    "estimator_type",
    "universe",
    "weight",
    "required_vars",
    "derived_vars",
    "scale_adjustment_required",
    "representativity_required",
    "implementation_status",
    "method_note"
  )

  missing_cols <- setdiff(required_cols, names(registry))

  if (length(missing_cols) > 0) {
    .enemdu_abort_invalid_registry(
      registry_name = "indicator_registry",
      message = glue::glue("Missing columns: {paste(missing_cols, collapse = ', ')}."),
      caller = ".enemdu_validate_indicator_registry_for_estimation"
    )
  }

  invisible(TRUE)
}

.enemdu_resolve_indicator_survey_type <- function(data,
                                                  survey_type = NULL) {
  if (!is.null(survey_type)) {
    return(.enemdu_normalize_survey_type(
      survey_type = survey_type,
      caller = ".enemdu_resolve_indicator_survey_type"
    ))
  }

  attr_survey_type <- attr(data, "survey_type")

  if (!is.null(attr_survey_type) && !is.na(attr_survey_type)) {
    return(.enemdu_normalize_survey_type(
      survey_type = attr_survey_type,
      caller = ".enemdu_resolve_indicator_survey_type"
    ))
  }

  NA_character_
}

.enemdu_resolve_indicator_value_var <- function(data,
                                                indicator_row,
                                                value = NULL) {
  if (!is.null(value)) {
    .enemdu_abort_missing_vars(
      vars = value,
      names_data = names(data),
      caller = "enemdu_indicator_estimate"
    )

    return(value)
  }

  indicator_id <- as.character(indicator_row$indicator_id[[1]])
  estimator_type <- as.character(indicator_row$estimator_type[[1]])

  if (indicator_id == "conteo_personas") {
    return(".enemdu_count_value")
  }

  derived_vars <- .enemdu_parse_pipe_values(indicator_row$derived_vars[[1]])
  required_vars <- .enemdu_parse_pipe_values(indicator_row$required_vars[[1]])

  candidates <- c(derived_vars, required_vars)
  candidates <- candidates[nzchar(candidates)]
  candidates <- candidates[candidates %in% names(data)]

  if (length(candidates) >= 1) {
    return(candidates[[1]])
  }

  if (identical(estimator_type, "total") && indicator_id %in% c("conteo_personas")) {
    return(".enemdu_count_value")
  }

  rlang::abort(
    message = glue::glue(
      "Could not resolve a value variable for indicator `{indicator_id}`. ",
      "Provide `value =` explicitly or ensure declared variables exist in data."
    ),
    class = c("enemdu_error_unresolved_indicator_value", "enemdu_error")
  )
}

.enemdu_attach_indicator_metadata <- function(estimate,
                                              indicator_row,
                                              value_var,
                                              created_count_value,
                                              weight) {
  meta <- tibble::tibble(
    indicator_label = as.character(indicator_row$indicator_label[[1]]),
    indicator_group = as.character(indicator_row$indicator_group[[1]]),
    unit = as.character(indicator_row$unit[[1]]),
    analysis_level = as.character(indicator_row$analysis_level[[1]]),
    universe = as.character(indicator_row$universe[[1]]),
    registry_weight = as.character(indicator_row$weight[[1]]),
    estimation_weight = weight,
    registry_value_var = value_var,
    created_count_value = created_count_value,
    registry_method_note = as.character(indicator_row$method_note[[1]])
  )

  repeated_meta <- meta[rep(1, nrow(estimate)), , drop = FALSE]

  out <- cbind(
    estimate["indicator_id"],
    repeated_meta,
    estimate[setdiff(names(estimate), "indicator_id")]
  )

  tibble::as_tibble(out)
}

.enemdu_apply_scale_adjusted_precision <- function(estimate,
                                                   data,
                                                   value_var,
                                                   group_vars,
                                                   household_id,
                                                   hsize,
                                                   estimator_type,
                                                   sample_n_min) {
  out_data <- data

  if (!hsize %in% names(out_data)) {
    out_data <- enemdu_build_hsize(
      data = out_data,
      household_id = household_id,
      hsize_name = hsize,
      overwrite = FALSE
    )
  }

  if (!is.numeric(out_data[[hsize]]) || any(out_data[[hsize]] <= 0, na.rm = TRUE)) {
    .enemdu_abort_invalid_hsize(
      hsize = hsize,
      caller = ".enemdu_apply_scale_adjusted_precision"
    )
  }

  adjusted_n <- .enemdu_adjusted_n_by_group(
    data = out_data,
    value_var = value_var,
    group_vars = group_vars,
    hsize = hsize
  )

  estimate[["household_scale_adjustment_required"]] <- TRUE
  estimate[["household_scale_adjustment_applied_to_precision"]] <- TRUE
  estimate[["adjusted_unweighted_n"]] <- NA_real_
  estimate[["adjusted_effective_n"]] <- NA_real_
  estimate[["scale_adjustment_note"]] <- paste(
    "Internal household-scale adjustment applied to precision decision.",
    "Point estimate was not changed."
  )

  for (i in seq_len(nrow(estimate))) {
    key <- .enemdu_group_key_from_estimate_row(
      estimate_row = estimate[i, , drop = FALSE],
      group_vars = group_vars
    )

    adjusted_value <- adjusted_n[[key]]

    if (is.null(adjusted_value) || is.na(adjusted_value)) {
      next
    }

    deff <- estimate$deff[[i]]
    adjusted_effective_n <- if (!is.na(deff) && deff > 0) {
      adjusted_value / deff
    } else {
      adjusted_value
    }

    estimate$adjusted_unweighted_n[[i]] <- adjusted_value
    estimate$adjusted_effective_n[[i]] <- adjusted_effective_n

    noncomputable_reason <- .enemdu_scale_precision_noncomputable_reason(
      estimate_row = estimate[i, , drop = FALSE]
    )

    if (!is.null(noncomputable_reason)) {
      estimate$decision[[i]] <- "no_recommended_inference"
      estimate$failed_reasons[[i]] <- .enemdu_preserve_failed_reason(
        current_reason = estimate$failed_reasons[[i]],
        fallback_reason = noncomputable_reason
      )
      estimate$quality_flag[[i]] <- .enemdu_preserve_quality_flag(
        current_flag = estimate$quality_flag[[i]],
        fallback_flag = "not_evaluable"
      )
      estimate$warning_flag[[i]] <- .enemdu_preserve_quality_flag(
        current_flag = estimate$warning_flag[[i]],
        fallback_flag = "inference_not_recommended"
      )

      next
    }

    precision <- tryCatch(
      {
        enemdu_evaluate_precision(
          estimate = estimate$estimate[[i]],
          standard_error = estimate$standard_error[[i]],
          cv = estimate$cv[[i]],
          n = adjusted_value,
          effective_n = adjusted_effective_n,
          deff = deff,
          degrees_freedom = estimate$degrees_freedom[[i]],
          estimator_type = estimator_type
        )
      },
      error = function(e) {
        tibble::tibble(
          decision = "precision_evaluation_error",
          failed_reasons = e$message
        )
      }
    )

    estimate$decision[[i]] <- precision$decision[[1]]
    estimate$failed_reasons[[i]] <- precision$failed_reasons[[1]]
    estimate$quality_flag[[i]] <- .enemdu_precision_quality_flag(
      decision = estimate$decision[[i]],
      unweighted_n = adjusted_value,
      sample_n_min = sample_n_min
    )
    estimate$warning_flag[[i]] <- .enemdu_precision_warning_flag(
      decision = estimate$decision[[i]]
    )
  }

  estimate
}

.enemdu_scale_precision_noncomputable_reason <- function(estimate_row) {
  if (!"estimate" %in% names(estimate_row) ||
      !.enemdu_is_finite_scalar(estimate_row$estimate[[1]])) {
    return("estimate_not_computable")
  }

  if (!"standard_error" %in% names(estimate_row) ||
      !.enemdu_is_finite_scalar(estimate_row$standard_error[[1]])) {
    return("standard_error_not_computable")
  }

  if (!"degrees_freedom" %in% names(estimate_row) ||
      !.enemdu_is_finite_scalar(estimate_row$degrees_freedom[[1]])) {
    return("degrees_freedom_not_computable")
  }

  NULL
}

.enemdu_is_finite_scalar <- function(x) {
  is.numeric(x) &&
    length(x) == 1 &&
    !is.na(x) &&
    is.finite(x)
}

.enemdu_preserve_failed_reason <- function(current_reason,
                                           fallback_reason) {
  if (.enemdu_is_nonempty_string(current_reason)) {
    return(as.character(current_reason))
  }

  as.character(fallback_reason)
}

.enemdu_preserve_quality_flag <- function(current_flag,
                                          fallback_flag) {
  if (.enemdu_is_nonempty_string(current_flag)) {
    return(as.character(current_flag))
  }

  as.character(fallback_flag)
}

.enemdu_is_nonempty_string <- function(x) {
  if (is.null(x) || length(x) != 1 || is.na(x)) {
    return(FALSE)
  }

  nzchar(trimws(as.character(x)))
}

.enemdu_adjusted_n_by_group <- function(data,
                                        value_var,
                                        group_vars,
                                        hsize) {
  valid_value <- rep(TRUE, nrow(data))

  if (value_var %in% names(data)) {
    valid_value <- !is.na(data[[value_var]])
  }

  contribution <- ifelse(valid_value, 1 / data[[hsize]], NA_real_)

  if (is.null(group_vars) || length(group_vars) == 0) {
    return(list(total = sum(contribution, na.rm = TRUE)))
  }

  group_info <- .enemdu_group_index(
    data = data,
    group_vars = group_vars,
    drop_na_groups = TRUE
  )

  out <- list()

  for (group_name in names(group_info$groups)) {
    idx <- group_info$groups[[group_name]]
    out[[group_name]] <- sum(contribution[idx], na.rm = TRUE)
  }

  out
}

.enemdu_group_key_from_estimate_row <- function(estimate_row, group_vars) {
  if (is.null(group_vars) || length(group_vars) == 0) {
    return("total")
  }

  values <- lapply(group_vars, function(v) {
    x <- as.character(estimate_row[[v]][[1]])
    if (is.na(x)) "<NA>" else x
  })

  do.call(paste, c(values, sep = "\r"))
}

.enemdu_as_logical <- function(x) {
  if (is.logical(x)) {
    return(isTRUE(x))
  }

  if (is.numeric(x)) {
    return(!is.na(x) && x != 0)
  }

  x <- tolower(trimws(as.character(x)))

  if (x %in% c("true", "t", "1", "yes", "y", "si", "sí")) {
    return(TRUE)
  }

  FALSE
}

.enemdu_empty_indicator_estimate <- function() {
  tibble::tibble(
    indicator_id = character(),
    indicator_label = character(),
    indicator_group = character(),
    unit = character(),
    analysis_level = character(),
    universe = character(),
    registry_weight = character(),
    estimation_weight = character(),
    registry_value_var = character(),
    created_count_value = logical(),
    registry_method_note = character(),
    measure = character(),
    statistic = character(),
    estimator_type = character(),
    survey_type = character(),
    design_domains = character(),
    estimate = numeric(),
    standard_error = numeric(),
    cv = numeric(),
    ci_lower = numeric(),
    ci_upper = numeric(),
    unweighted_n = integer(),
    weighted_n = numeric(),
    deff = numeric(),
    effective_n = numeric(),
    degrees_freedom = numeric(),
    decision = character(),
    failed_reasons = character(),
    quality_flag = character(),
    warning_flag = character(),
    method_note = character(),
    household_scale_adjustment_required = logical(),
    household_scale_adjustment_applied_to_precision = logical(),
    adjusted_unweighted_n = numeric(),
    adjusted_effective_n = numeric(),
    scale_adjustment_note = character(),
    domain_scope_flag = character(),
    domain_is_design_domain = logical(),
    domain_requires_precision_evaluation = logical(),
    domain_scope_message = character(),
    representativity_flag = character(),
    representativity_note = character()
  )
}

.enemdu_bind_estimate_rows <- function(outputs) {
  all_cols <- unique(unlist(lapply(outputs, names)))

  normalized <- lapply(outputs, function(x) {
    missing_cols <- setdiff(all_cols, names(x))

    for (col in missing_cols) {
      x[[col]] <- NA
    }

    x[all_cols]
  })

  out <- do.call(rbind, normalized)
  row.names(out) <- NULL
  tibble::as_tibble(out)
}
