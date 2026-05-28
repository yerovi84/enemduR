#' Estimate IPM KPIs from row-level IPM flags
#'
#' Estimates Ecuador IPM analytical outputs from already-built row-level
#' variables. The function consumes `ipm_score`, `tpm`, and `tpem`, or builds
#' them from the 12 registered component columns with
#' `enemdu_build_ipm_flags()`. If requested, it can first call
#' `enemdu_build_ipm_components()`, respecting that builder's pending-component
#' behavior.
#'
#' The aggregate outputs are `tpm`, `tpem`, `A`, and `ipm`. Here `A` is the
#' average deprivation intensity among multidimensionally poor persons, and
#' aggregate `ipm` is calculated as `tpm * A`. These are aggregate KPI outputs,
#' not row-level variables.
#'
#' This function does not derive new raw IPM component rules and does not claim
#' official institutional validation.
#'
#' @param data A data frame.
#' @param survey_type One of `"anual"`, `"trimestral"`, or `"mensual"`.
#' @param by Optional grouping variable or variables.
#' @param ids Primary sampling unit variable.
#' @param strata Survey strata variable.
#' @param weight Survey expansion factor variable.
#' @param score_var Row-level IPM score variable.
#' @param tpm_var Row-level multidimensional poverty flag variable.
#' @param tpem_var Row-level extreme multidimensional poverty flag variable.
#' @param build_components Logical. If `TRUE`, call
#' `enemdu_build_ipm_components()` before flags are checked.
#' @param build_flags Logical. If `TRUE`, build row-level flags from registered
#' component columns when those components are available.
#' @param component_cols Optional registered IPM component columns passed to
#' `enemdu_build_ipm_flags()`.
#' @param strict Logical. If `TRUE`, abort on missing or invalid score or flag
#' variables.
#' @param ... Additional named arguments. Arguments matching
#' `enemdu_build_ipm_components()` are used only when `build_components = TRUE`;
#' the remaining arguments are passed to survey estimators.
#'
#' @return A tibble with survey KPI estimates for `tpm`, `tpem`, `A`, and
#' `ipm`, plus non-official validation metadata.
#' @export
enemdu_kpi_ipm <- function(
  data,
  survey_type = c("anual", "trimestral", "mensual"),
  by = NULL,
  ids = "upm",
  strata = "estrato",
  weight = "fexp",
  score_var = "ipm_score",
  tpm_var = "tpm",
  tpem_var = "tpem",
  build_components = FALSE,
  build_flags = TRUE,
  component_cols = NULL,
  strict = TRUE,
  ...
) {
  caller <- "enemdu_kpi_ipm"

  if (!is.data.frame(data)) {
    .enemdu_abort_invalid_data(caller = caller)
  }

  survey_type <- match.arg(survey_type)
  by <- .enemdu_ipm_normalize_by(by)

  ids <- .enemdu_ipm_single_kpi_var(ids, "ids", caller)
  strata <- .enemdu_ipm_single_kpi_var(strata, "strata", caller)
  weight <- .enemdu_ipm_single_kpi_var(weight, "weight", caller)
  score_var <- .enemdu_ipm_single_kpi_var(score_var, "score_var", caller)
  tpm_var <- .enemdu_ipm_single_kpi_var(tpm_var, "tpm_var", caller)
  tpem_var <- .enemdu_ipm_single_kpi_var(tpem_var, "tpem_var", caller)

  dots <- list(...)
  split_dots <- .enemdu_ipm_split_dots(dots)

  prepared <- .enemdu_prepare_ipm_kpi_data(
    data = data,
    build_components = build_components,
    build_flags = build_flags,
    component_cols = component_cols,
    score_var = score_var,
    tpm_var = tpm_var,
    tpem_var = tpem_var,
    strict = strict,
    component_dots = split_dots$component_dots
  )

  work_data <- prepared$data

  .enemdu_validate_ipm_kpi_variables(
    data = work_data,
    vars = c(ids, strata, weight, score_var, tpm_var, tpem_var, by),
    caller = caller
  )

  work_data[[score_var]] <- .enemdu_coerce_ipm_score(
    values = work_data[[score_var]],
    var = score_var,
    strict = strict,
    caller = caller
  )
  work_data[[tpm_var]] <- as.integer(.enemdu_coerce_ipm_component(
    values = work_data[[tpm_var]],
    var = tpm_var,
    strict = strict
  ))
  work_data[[tpem_var]] <- as.integer(.enemdu_coerce_ipm_component(
    values = work_data[[tpem_var]],
    var = tpem_var,
    strict = strict
  ))

  if (!is.numeric(work_data[[weight]])) {
    .enemdu_abort_invalid_numeric_var(var = weight, caller = caller)
  }

  tpm_estimates <- .enemdu_ipm_survey_proportion(
    data = work_data,
    value = tpm_var,
    group_vars = by,
    ids = ids,
    strata = strata,
    weight = weight,
    survey_type = survey_type,
    indicator_id = "tpm",
    label = .enemdu_ipm_indicator_label("tpm"),
    survey_dots = split_dots$survey_dots
  )

  tpem_estimates <- .enemdu_ipm_survey_proportion(
    data = work_data,
    value = tpem_var,
    group_vars = by,
    ids = ids,
    strata = strata,
    weight = weight,
    survey_type = survey_type,
    indicator_id = "tpem",
    label = .enemdu_ipm_indicator_label("tpem"),
    survey_dots = split_dots$survey_dots
  )

  a_estimates <- .enemdu_ipm_intensity_estimates(
    data = work_data,
    score_var = score_var,
    tpm_var = tpm_var,
    group_vars = by,
    ids = ids,
    strata = strata,
    weight = weight,
    survey_type = survey_type,
    template = tpm_estimates,
    survey_dots = split_dots$survey_dots
  )

  ipm_estimates <- .enemdu_ipm_product_estimates(
    tpm_estimates = tpm_estimates,
    a_estimates = a_estimates,
    group_vars = by
  )

  out <- .enemdu_ipm_bind_rows(list(
    tpm_estimates,
    tpem_estimates,
    a_estimates,
    ipm_estimates
  ))

  out <- .enemdu_add_ipm_domain_metadata(out, by = by)
  out[["estimate_percent"]] <- out[["estimate"]] * 100
  out[["official_validation_status"]] <- "not_officially_validated"
  out[["official_validation_note"]] <- paste(
    "IPM KPI estimates are local reproducibility outputs.",
    "They are not official institutional validation."
  )

  attr(out, "ipm_kpi_policy") <- list(
    survey_type = survey_type,
    by = by,
    ids = ids,
    strata = strata,
    weight = weight,
    score_var = score_var,
    tpm_var = tpm_var,
    tpem_var = tpem_var,
    build_components = isTRUE(build_components),
    build_flags = isTRUE(build_flags),
    components_diagnostics = prepared$component_diagnostics,
    flags_diagnostics = prepared$flags_diagnostics,
    official_validation_status = "not_officially_validated",
    note = paste(
      "The KPI layer consumes already-built IPM components or flags.",
      "Benchmarks must be compared separately and do not imply official validation."
    )
  )

  class(out) <- unique(c("enemdu_ipm_kpi", class(out)))
  out
}

.enemdu_prepare_ipm_kpi_data <- function(data,
                                         build_components,
                                         build_flags,
                                         component_cols,
                                         score_var,
                                         tpm_var,
                                         tpem_var,
                                         strict,
                                         component_dots = list()) {
  out <- data
  component_diagnostics <- NULL
  flags_diagnostics <- NULL

  if (isTRUE(build_components)) {
    component_args <- c(
      list(data = out, strict = strict),
      component_dots
    )

    out <- do.call(enemdu_build_ipm_components, component_args)
    component_diagnostics <- attr(out, "ipm_component_diagnostics")
  }

  resolved_components <- .enemdu_resolve_ipm_components(component_cols)
  flag_vars <- c(score_var, tpm_var, tpem_var)
  components_available <- all(resolved_components$component_cols %in% names(out))
  flags_available <- all(flag_vars %in% names(out))

  if (isTRUE(build_flags) && isTRUE(components_available)) {
    out <- enemdu_build_ipm_flags(
      data = out,
      component_cols = resolved_components$component_cols,
      score_var = score_var,
      tpm_var = tpm_var,
      tpem_var = tpem_var,
      overwrite = flags_available,
      strict = strict
    )
    flags_diagnostics <- attr(out, "ipm_flags_diagnostics")
  } else if (!isTRUE(flags_available)) {
    missing_flags <- setdiff(flag_vars, names(out))

    rlang::abort(
      message = glue::glue(
        "IPM KPI inputs are incomplete. Missing score or flag variables: ",
        "{paste(missing_flags, collapse = ', ')}. ",
        "Provide prebuilt flags or registered component columns."
      ),
      class = c("enemdu_error_missing_ipm_kpi_inputs", "enemdu_error"),
      missing_vars = missing_flags
    )
  }

  list(
    data = out,
    component_diagnostics = component_diagnostics,
    flags_diagnostics = flags_diagnostics
  )
}

.enemdu_ipm_survey_proportion <- function(data,
                                          value,
                                          group_vars,
                                          ids,
                                          strata,
                                          weight,
                                          survey_type,
                                          indicator_id,
                                          label,
                                          survey_dots) {
  args <- c(
    list(
      data = data,
      value = value,
      group_vars = group_vars,
      ids = ids,
      strata = strata,
      weight = weight,
      survey_type = survey_type,
      indicator_id = indicator_id,
      measure = label
    ),
    survey_dots
  )

  out <- do.call(enemdu_survey_proportion, args)
  out <- .enemdu_ipm_fill_weighted_mean(
    estimates = out,
    data = data,
    value = value,
    group_vars = group_vars,
    weight = weight
  )
  .enemdu_ipm_set_indicator_metadata(
    out = out,
    indicator_id = indicator_id,
    label = label,
    statistic = "survey_proportion",
    estimator_type = "proportion_0_1"
  )
}

.enemdu_ipm_intensity_estimates <- function(data,
                                           score_var,
                                           tpm_var,
                                           group_vars,
                                           ids,
                                           strata,
                                           weight,
                                           survey_type,
                                           template,
                                           survey_dots) {
  out <- .enemdu_ipm_set_indicator_metadata(
    out = template,
    indicator_id = "A",
    label = .enemdu_ipm_indicator_label("A"),
    statistic = "survey_mean_among_tpm",
    estimator_type = "mean_among_multidimensionally_poor"
  )
  out <- .enemdu_ipm_clear_precision(out)
  out[["estimate"]] <- 0
  out[["failed_reasons"]] <- "no_multidimensionally_poor_persons"
  out[["method_note"]] <- paste(
    "A is estimated as mean ipm_score among people with tpm == 1.",
    "Domains with no multidimensionally poor persons receive A = 0."
  )

  poor_rows <- !is.na(data[[tpm_var]]) & data[[tpm_var]] == 1 &
    !is.na(data[[score_var]])

  if (!any(poor_rows)) {
    return(out)
  }

  poor_data <- data[poor_rows, , drop = FALSE]
  args <- c(
    list(
      data = poor_data,
      value = score_var,
      group_vars = group_vars,
      ids = ids,
      strata = strata,
      weight = weight,
      survey_type = survey_type,
      indicator_id = "A",
      measure = .enemdu_ipm_indicator_label("A")
    ),
    survey_dots
  )

  poor_estimates <- do.call(enemdu_survey_mean, args)
  poor_estimates <- .enemdu_ipm_fill_weighted_mean(
    estimates = poor_estimates,
    data = poor_data,
    value = score_var,
    group_vars = group_vars,
    weight = weight
  )
  poor_estimates <- .enemdu_ipm_set_indicator_metadata(
    out = poor_estimates,
    indicator_id = "A",
    label = .enemdu_ipm_indicator_label("A"),
    statistic = "survey_mean_among_tpm",
    estimator_type = "mean_among_multidimensionally_poor"
  )

  template_key <- .enemdu_ipm_estimate_key(out, group_vars)
  poor_key <- .enemdu_ipm_estimate_key(poor_estimates, group_vars)
  matched <- match(template_key, poor_key)
  cols <- intersect(names(out), names(poor_estimates))

  for (i in seq_along(matched)) {
    j <- matched[[i]]

    if (!is.na(j)) {
      out[i, cols] <- poor_estimates[j, cols]
    }
  }

  out
}

.enemdu_ipm_product_estimates <- function(tpm_estimates,
                                          a_estimates,
                                          group_vars) {
  out <- .enemdu_ipm_set_indicator_metadata(
    out = tpm_estimates,
    indicator_id = "ipm",
    label = .enemdu_ipm_indicator_label("ipm"),
    statistic = "derived_product",
    estimator_type = "tpm_times_A"
  )
  out <- .enemdu_ipm_clear_precision(out)

  tpm_key <- .enemdu_ipm_estimate_key(tpm_estimates, group_vars)
  a_key <- .enemdu_ipm_estimate_key(a_estimates, group_vars)
  matched <- match(tpm_key, a_key)

  a_values <- rep(NA_real_, nrow(tpm_estimates))
  observed_match <- !is.na(matched)
  a_values[observed_match] <- a_estimates$estimate[matched[observed_match]]

  out[["estimate"]] <- tpm_estimates$estimate * a_values
  out[["method_note"]] <- "Aggregate IPM is calculated as TPM multiplied by A."
  out
}

.enemdu_ipm_fill_weighted_mean <- function(estimates,
                                           data,
                                           value,
                                           group_vars,
                                           weight) {
  if (!"estimate" %in% names(estimates) || nrow(estimates) == 0) {
    return(estimates)
  }

  keys <- .enemdu_ipm_estimate_key(estimates, group_vars)

  for (i in seq_len(nrow(estimates))) {
    if (!is.na(estimates$estimate[[i]])) {
      next
    }

    idx <- .enemdu_ipm_data_key(data, group_vars) == keys[[i]]
    x <- suppressWarnings(as.numeric(data[[value]]))
    w <- suppressWarnings(as.numeric(data[[weight]]))
    valid <- idx & !is.na(x) & is.finite(x) & !is.na(w) & is.finite(w) & w > 0

    if (!any(valid)) {
      next
    }

    estimates$estimate[[i]] <- stats::weighted.mean(x[valid], w[valid])

    if ("unweighted_n" %in% names(estimates)) {
      estimates$unweighted_n[[i]] <- as.integer(sum(valid))
    }

    if ("weighted_n" %in% names(estimates)) {
      estimates$weighted_n[[i]] <- sum(w[valid])
    }

    if ("failed_reasons" %in% names(estimates)) {
      estimates$failed_reasons[[i]] <- "point_estimate_filled_by_weighted_mean"
    }

    if ("method_note" %in% names(estimates)) {
      estimates$method_note[[i]] <- paste(
        estimates$method_note[[i]],
        "Point estimate was filled by the survey-weighted mean when precision metadata was not computable."
      )
    }
  }

  estimates
}

.enemdu_ipm_clear_precision <- function(out) {
  precision_cols <- c(
    "standard_error",
    "cv",
    "ci_lower",
    "ci_upper",
    "deff",
    "effective_n",
    "degrees_freedom"
  )

  for (col in intersect(precision_cols, names(out))) {
    out[[col]] <- NA_real_
  }

  if ("decision" %in% names(out)) {
    out[["decision"]] <- "derived_kpi"
  }
  if ("quality_flag" %in% names(out)) {
    out[["quality_flag"]] <- "derived_kpi"
  }
  if ("warning_flag" %in% names(out)) {
    out[["warning_flag"]] <- "derived_kpi"
  }
  if ("failed_reasons" %in% names(out)) {
    out[["failed_reasons"]] <- NA_character_
  }

  out
}

.enemdu_ipm_set_indicator_metadata <- function(out,
                                               indicator_id,
                                               label,
                                               statistic,
                                               estimator_type) {
  out[["indicator_id"]] <- indicator_id
  out[["indicator_label"]] <- label
  out[["measure"]] <- label
  out[["indicator_group"]] <- "ipm"
  out[["unit"]] <- "proportion"
  out[["statistic"]] <- statistic
  out[["estimator_type"]] <- estimator_type
  out
}

.enemdu_add_ipm_domain_metadata <- function(out, by) {
  if (is.null(by) || length(by) == 0) {
    out[["domain_type"]] <- "national"
    out[["domain_value"]] <- "national"
    out[["domain_label"]] <- "National"
    out[["domain"]] <- "national"
    return(out)
  }

  if (length(by) == 1) {
    raw <- as.character(out[[by]])
    mapped <- .enemdu_ipm_map_domain_values(raw)
    out[["domain_type"]] <- by
    out[["domain_value"]] <- mapped
    out[["domain_label"]] <- .enemdu_ipm_domain_label(mapped)
    out[["domain"]] <- mapped
    return(out)
  }

  raw <- .enemdu_ipm_estimate_key(out, by)
  out[["domain_type"]] <- paste(by, collapse = "|")
  out[["domain_value"]] <- raw
  out[["domain_label"]] <- raw
  out[["domain"]] <- raw
  out
}

.enemdu_ipm_map_domain_values <- function(values) {
  values_chr <- tolower(trimws(as.character(values)))
  out <- values_chr
  out[values_chr %in% c("1", "urban", "urbano")] <- "urban"
  out[values_chr %in% c("2", "rural")] <- "rural"
  out
}

.enemdu_ipm_domain_label <- function(values) {
  out <- values
  out[values == "national"] <- "National"
  out[values == "urban"] <- "Urban"
  out[values == "rural"] <- "Rural"
  out
}

.enemdu_ipm_estimate_key <- function(estimates, group_vars) {
  if (is.null(group_vars) || length(group_vars) == 0) {
    return(rep("national", nrow(estimates)))
  }

  missing_group_vars <- setdiff(group_vars, names(estimates))

  if (length(missing_group_vars) > 0) {
    return(rep(NA_character_, nrow(estimates)))
  }

  .enemdu_ipm_data_key(estimates, group_vars)
}

.enemdu_ipm_data_key <- function(data, group_vars) {
  if (is.null(group_vars) || length(group_vars) == 0) {
    return(rep("national", nrow(data)))
  }

  values <- lapply(group_vars, function(var) {
    as.character(data[[var]])
  })

  do.call(paste, c(values, sep = "\r"))
}

.enemdu_coerce_ipm_score <- function(values, var, strict, caller) {
  missing <- is.na(values)
  numeric_values <- .enemdu_coerce_ipm_numeric(values)
  invalid_conversion <- !missing & is.na(numeric_values)
  invalid_range <- !missing & !is.na(numeric_values) &
    (!is.finite(numeric_values) | numeric_values < 0 | numeric_values > 1)

  if (any(invalid_conversion | invalid_range, na.rm = TRUE)) {
    rlang::abort(
      message = glue::glue(
        "IPM score variable `{var}` must contain numeric values from 0 to 1."
      ),
      class = c("enemdu_error_invalid_ipm_score", "enemdu_error")
    )
  }

  if (isTRUE(strict) && any(missing)) {
    rlang::abort(
      message = glue::glue("IPM score variable `{var}` contains missing values."),
      class = c("enemdu_error_missing_ipm_score", "enemdu_error")
    )
  }

  numeric_values
}

.enemdu_validate_ipm_kpi_variables <- function(data, vars, caller) {
  .enemdu_abort_missing_vars(
    vars = unique(vars),
    names_data = names(data),
    caller = caller
  )
}

.enemdu_ipm_single_kpi_var <- function(var, arg, caller) {
  var <- as.character(var)

  if (length(var) != 1 || is.na(var) || !nzchar(var)) {
    rlang::abort(
      message = glue::glue("`{arg}` must be a single non-empty variable name."),
      class = c("enemdu_error_invalid_ipm_kpi_input", "enemdu_error")
    )
  }

  var
}

.enemdu_ipm_normalize_by <- function(by) {
  if (is.null(by)) {
    return(NULL)
  }

  by <- as.character(by)

  if (length(by) == 0 || any(is.na(by)) || any(!nzchar(by))) {
    rlang::abort(
      message = "`by` must be `NULL` or a non-empty character vector.",
      class = c("enemdu_error_invalid_ipm_kpi_input", "enemdu_error")
    )
  }

  unique(by)
}

.enemdu_ipm_split_dots <- function(dots) {
  if (length(dots) == 0) {
    return(list(component_dots = list(), survey_dots = list()))
  }

  dot_names <- names(dots)

  if (is.null(dot_names) || any(!nzchar(dot_names))) {
    rlang::abort(
      message = "Additional IPM KPI arguments in `...` must be named.",
      class = c("enemdu_error_invalid_ipm_kpi_dots", "enemdu_error")
    )
  }

  component_formals <- setdiff(
    names(formals(enemdu_build_ipm_components)),
    "data"
  )
  component_dots <- dots[dot_names %in% component_formals]
  survey_dots <- dots[!dot_names %in% component_formals]

  list(component_dots = component_dots, survey_dots = survey_dots)
}

.enemdu_ipm_indicator_label <- function(indicator_id) {
  labels <- c(
    tpm = "Multidimensional poverty rate",
    tpem = "Extreme multidimensional poverty rate",
    A = "Average deprivation intensity among multidimensionally poor persons",
    ipm = "Adjusted multidimensional poverty index"
  )

  unname(labels[[indicator_id]] %||% indicator_id)
}

.enemdu_ipm_bind_rows <- function(outputs) {
  outputs <- outputs[lengths(outputs) > 0]

  if (length(outputs) == 0) {
    return(tibble::tibble())
  }

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
