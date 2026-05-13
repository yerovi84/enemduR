#' Build inferential indicator tables for ENEMDU outputs
#'
#' Builds a stable long-format inferential table for one or more indicators
#' declared in `indicator_registry.csv`. This function is intentionally an
#' orchestration layer: it delegates estimation, precision evaluation, domain
#' validation, and representativity flags to `enemdu_indicator_estimate()`.
#'
#' @param data A data frame.
#' @param indicator_id Character vector of indicator identifiers.
#' @param group_vars Optional character vector of grouping variables.
#' @param registry Indicator registry. Defaults to package registry.
#' @param value Optional value override. Use `NULL`, a single value for a single
#' indicator, a named vector/list keyed by `indicator_id`, or a vector with the
#' same length as `indicator_id`.
#' @param ids Primary sampling unit variable. Defaults to `"upm"`.
#' @param strata Strata variable. Defaults to `"estrato"`.
#' @param weight Optional weight variable override. If `NULL`, uses registry
#' weights through `enemdu_indicator_estimate()`.
#' @param survey_type Optional ENEMDU survey type.
#' @param domain_level Optional domain level.
#' @param domain_var Optional domain variable.
#' @param strict_domain Logical. If `TRUE`, blocks domains outside design scope.
#' @param integrate_representativity Logical. If `TRUE`, appends domain and
#' representativity metadata.
#' @param household_id Household identifier for household-scale adjustment.
#' @param hsize Household-size variable.
#' @param scale_adjustment One of `"metadata"`, `"never"` or `"always"`.
#' @param conf_level Confidence level.
#' @param lonely_psu Option passed to `survey.lonely.psu`.
#' @param sample_n_min Minimum sample threshold.
#' @param unsupported What to do with unsupported estimator types: `"row"`,
#' `"skip"` or `"error"`.
#' @param on_error What to do with estimation errors: `"error"` or `"row"`.
#' @param table_id Optional stable table identifier.
#'
#' @return A tibble in long format.
#' @export
enemdu_indicator_table <- function(data,
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
                                   sample_n_min = 60,
                                   unsupported = c("row", "skip", "error"),
                                   on_error = c("error", "row"),
                                   table_id = NULL) {
  if (!is.data.frame(data)) {
    .enemdu_abort_invalid_data(caller = "enemdu_indicator_table")
  }

  if (missing(indicator_id) || is.null(indicator_id) || length(indicator_id) == 0) {
    .enemdu_abort_missing_argument("indicator_id", caller = "enemdu_indicator_table")
  }

  indicator_id <- as.character(indicator_id)
  indicator_id <- indicator_id[!is.na(indicator_id) & nzchar(indicator_id)]

  if (length(indicator_id) == 0) {
    .enemdu_abort_missing_argument("indicator_id", caller = "enemdu_indicator_table")
  }

  if (!is.null(group_vars)) {
    .enemdu_abort_missing_vars(
      vars = group_vars,
      names_data = names(data),
      caller = "enemdu_indicator_table"
    )
  }

  scale_adjustment <- match.arg(scale_adjustment)
  unsupported <- match.arg(unsupported)
  on_error <- match.arg(on_error)

  .enemdu_validate_indicator_registry_for_estimation(registry)

  table_id <- table_id %||% .enemdu_indicator_table_id(
    indicator_id = indicator_id,
    group_vars = group_vars,
    survey_type = survey_type
  )

  outputs <- list()

  for (i in seq_along(indicator_id)) {
    indicator_i <- indicator_id[[i]]
    indicator_row <- registry[registry$indicator_id == indicator_i, , drop = FALSE]

    if (nrow(indicator_row) != 1) {
      rlang::abort(
        message = glue::glue("Indicator `{indicator_i}` was not found uniquely in `indicator_registry.csv`."),
        class = c("enemdu_error_invalid_indicator_id", "enemdu_error")
      )
    }

    estimator_type <- as.character(indicator_row$estimator_type[[1]])

    if (!estimator_type %in% c("mean", "total", "proportion_0_1")) {
      if (identical(unsupported, "error")) {
        rlang::abort(
          message = glue::glue("Indicator `{indicator_i}` has unsupported estimator_type `{estimator_type}`."),
          class = c("enemdu_error_unsupported_estimator_type", "enemdu_error")
        )
      }

      if (identical(unsupported, "skip")) {
        rlang::warn(
          message = glue::glue("Indicator `{indicator_i}` was skipped because estimator_type `{estimator_type}` is not supported."),
          class = c("enemdu_warning_unsupported_indicator_skipped", "enemdu_warning")
        )
        next
      }

      outputs[[length(outputs) + 1L]] <- .enemdu_indicator_table_status_row(
        indicator_row = indicator_row,
        table_id = table_id,
        group_vars = group_vars,
        survey_type = survey_type,
        status = "unsupported_estimator_type",
        message = glue::glue("Indicator `{indicator_i}` has estimator_type `{estimator_type}` and was not estimated.")
      )
      next
    }

    value_i <- .enemdu_indicator_table_value(value, indicator_i, i, length(indicator_id))

    estimated <- tryCatch(
      enemdu_indicator_estimate(
        data = data,
        indicator_id = indicator_i,
        group_vars = group_vars,
        registry = registry,
        value = value_i,
        ids = ids,
        strata = strata,
        weight = weight,
        survey_type = survey_type,
        domain_level = domain_level,
        domain_var = domain_var,
        strict_domain = strict_domain,
        integrate_representativity = integrate_representativity,
        household_id = household_id,
        hsize = hsize,
        scale_adjustment = scale_adjustment,
        conf_level = conf_level,
        lonely_psu = lonely_psu,
        sample_n_min = sample_n_min
      ),
      error = function(e) e
    )

    if (inherits(estimated, "error")) {
      if (identical(on_error, "error")) {
        rlang::abort(
          message = glue::glue("Could not estimate indicator `{indicator_i}`: {estimated$message}"),
          parent = estimated,
          class = c("enemdu_error_indicator_table_estimation", "enemdu_error")
        )
      }

      outputs[[length(outputs) + 1L]] <- .enemdu_indicator_table_status_row(
        indicator_row = indicator_row,
        table_id = table_id,
        group_vars = group_vars,
        survey_type = survey_type,
        status = "estimation_error",
        message = estimated$message
      )
      next
    }

    outputs[[length(outputs) + 1L]] <- .enemdu_indicator_table_add_table_cols(
      estimated,
      table_id = table_id,
      group_vars = group_vars,
      status = "estimated",
      message = "Indicator estimated through enemdu_indicator_estimate()."
    )
  }

  if (length(outputs) == 0) {
    out <- .enemdu_indicator_table_empty()
  } else {
    out <- .enemdu_bind_estimate_rows(outputs)
    out <- .enemdu_indicator_table_reorder(out)
  }

  attr(out, "indicator_table_policy") <- list(
    table_id = table_id,
    indicator_id = indicator_id,
    group_vars = group_vars,
    survey_type = survey_type,
    strict_domain = strict_domain,
    integrate_representativity = integrate_representativity,
    unsupported = unsupported,
    on_error = on_error,
    note = paste(
      "This table is an orchestration layer over enemdu_indicator_estimate().",
      "Rows with table_status different from 'estimated' are controlled non-estimated rows."
    )
  )

  class(out) <- unique(c("enemdu_indicator_table", class(out)))
  out
}

#' Build inferential survey tabulations for ENEMDU indicators
#'
#' Compatibility wrapper around `enemdu_indicator_table()`.
#'
#' @inheritParams enemdu_indicator_table
#' @return A tibble in long format.
#' @export
enemdu_survey_tabulate <- function(data,
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
                                   sample_n_min = 60,
                                   unsupported = c("row", "skip", "error"),
                                   on_error = c("error", "row"),
                                   table_id = NULL) {
  scale_adjustment <- match.arg(scale_adjustment)
  unsupported <- match.arg(unsupported)
  on_error <- match.arg(on_error)

  enemdu_indicator_table(
    data = data,
    indicator_id = indicator_id,
    group_vars = group_vars,
    registry = registry,
    value = value,
    ids = ids,
    strata = strata,
    weight = weight,
    survey_type = survey_type,
    domain_level = domain_level,
    domain_var = domain_var,
    strict_domain = strict_domain,
    integrate_representativity = integrate_representativity,
    household_id = household_id,
    hsize = hsize,
    scale_adjustment = scale_adjustment,
    conf_level = conf_level,
    lonely_psu = lonely_psu,
    sample_n_min = sample_n_min,
    unsupported = unsupported,
    on_error = on_error,
    table_id = table_id
  )
}

.enemdu_indicator_table_add_table_cols <- function(x,
                                                   table_id,
                                                   group_vars,
                                                   status,
                                                   message) {
  prefix <- tibble::tibble(
    table_id = rep(table_id, nrow(x)),
    table_type = rep("inferential_indicator_table", nrow(x)),
    table_status = rep(status, nrow(x)),
    table_status_message = rep(message, nrow(x)),
    group_vars = rep(.enemdu_indicator_table_group_label(group_vars), nrow(x))
  )

  tibble::as_tibble(cbind(prefix, x))
}

.enemdu_indicator_table_status_row <- function(indicator_row,
                                               table_id,
                                               group_vars,
                                               survey_type,
                                               status,
                                               message) {
  tibble::tibble(
    table_id = table_id,
    table_type = "inferential_indicator_table",
    table_status = status,
    table_status_message = as.character(message),
    group_vars = .enemdu_indicator_table_group_label(group_vars),
    indicator_id = as.character(indicator_row$indicator_id[[1]]),
    indicator_label = as.character(indicator_row$indicator_label[[1]]),
    indicator_group = as.character(indicator_row$indicator_group[[1]]),
    unit = as.character(indicator_row$unit[[1]]),
    analysis_level = as.character(indicator_row$analysis_level[[1]]),
    universe = as.character(indicator_row$universe[[1]]),
    registry_weight = as.character(indicator_row$weight[[1]]),
    estimation_weight = NA_character_,
    registry_value_var = NA_character_,
    created_count_value = FALSE,
    registry_method_note = as.character(indicator_row$method_note[[1]]),
    measure = as.character(indicator_row$indicator_label[[1]]),
    statistic = NA_character_,
    estimator_type = as.character(indicator_row$estimator_type[[1]]),
    survey_type = survey_type %||% NA_character_,
    design_domains = NA_character_,
    estimate = NA_real_,
    standard_error = NA_real_,
    cv = NA_real_,
    ci_lower = NA_real_,
    ci_upper = NA_real_,
    unweighted_n = NA_integer_,
    weighted_n = NA_real_,
    deff = NA_real_,
    effective_n = NA_real_,
    degrees_freedom = NA_real_,
    decision = "not_estimated",
    failed_reasons = as.character(message),
    quality_flag = "not_evaluable",
    warning_flag = status,
    method_note = "Controlled non-estimated row generated by enemdu_indicator_table().",
    household_scale_adjustment_required = NA,
    household_scale_adjustment_applied_to_precision = NA,
    adjusted_unweighted_n = NA_real_,
    adjusted_effective_n = NA_real_,
    scale_adjustment_note = NA_character_,
    domain_scope_flag = NA_character_,
    domain_is_design_domain = NA,
    domain_requires_precision_evaluation = NA,
    domain_scope_message = NA_character_,
    representativity_flag = "not_estimated",
    representativity_note = as.character(message)
  )
}

.enemdu_indicator_table_value <- function(value, indicator_id, position, n_indicators) {
  if (is.null(value)) {
    return(NULL)
  }

  if (is.list(value)) {
    nms <- names(value)

    if (!is.null(nms) && indicator_id %in% nms) {
      return(as.character(value[[indicator_id]]))
    }

    if (n_indicators == 1 && length(value) == 1) {
      return(as.character(value[[1]]))
    }

    if (length(value) == n_indicators && is.null(nms)) {
      return(as.character(value[[position]]))
    }
  } else {
    value <- as.character(value)
    nms <- names(value)

    if (!is.null(nms) && indicator_id %in% nms) {
      return(value[[indicator_id]])
    }

    if (n_indicators == 1 && length(value) == 1) {
      return(value[[1]])
    }

    if (length(value) == n_indicators && is.null(nms)) {
      return(value[[position]])
    }
  }

  rlang::abort(
    message = "`value` must be NULL, length 1 for one indicator, named by indicator_id, or have the same length as indicator_id.",
    class = c("enemdu_error_indicator_table_value_override", "enemdu_error")
  )
}

.enemdu_indicator_table_reorder <- function(out) {
  preferred <- c(
    "table_id", "table_type", "table_status", "table_status_message", "group_vars",
    "indicator_id", "indicator_label", "indicator_group", "unit", "analysis_level",
    "universe", "registry_weight", "estimation_weight", "registry_value_var",
    "created_count_value", "registry_method_note", "measure", "statistic",
    "estimator_type", "survey_type", "design_domains", "estimate", "standard_error",
    "cv", "ci_lower", "ci_upper", "unweighted_n", "weighted_n", "deff",
    "effective_n", "degrees_freedom", "decision", "failed_reasons",
    "quality_flag", "warning_flag", "method_note",
    "household_scale_adjustment_required",
    "household_scale_adjustment_applied_to_precision", "adjusted_unweighted_n",
    "adjusted_effective_n", "scale_adjustment_note", "domain_scope_flag",
    "domain_is_design_domain", "domain_requires_precision_evaluation",
    "domain_scope_message", "representativity_flag", "representativity_note"
  )

  ordered <- c(preferred[preferred %in% names(out)], setdiff(names(out), preferred))
  tibble::as_tibble(out[ordered])
}

.enemdu_indicator_table_empty <- function() {
  tibble::tibble(
    table_id = character(), table_type = character(), table_status = character(),
    table_status_message = character(), group_vars = character(), indicator_id = character(),
    indicator_label = character(), indicator_group = character(), unit = character(),
    analysis_level = character(), universe = character(), registry_weight = character(),
    estimation_weight = character(), registry_value_var = character(), created_count_value = logical(),
    registry_method_note = character(), measure = character(), statistic = character(),
    estimator_type = character(), survey_type = character(), design_domains = character(),
    estimate = numeric(), standard_error = numeric(), cv = numeric(), ci_lower = numeric(),
    ci_upper = numeric(), unweighted_n = integer(), weighted_n = numeric(), deff = numeric(),
    effective_n = numeric(), degrees_freedom = numeric(), decision = character(),
    failed_reasons = character(), quality_flag = character(), warning_flag = character(),
    method_note = character(), household_scale_adjustment_required = logical(),
    household_scale_adjustment_applied_to_precision = logical(), adjusted_unweighted_n = numeric(),
    adjusted_effective_n = numeric(), scale_adjustment_note = character(), domain_scope_flag = character(),
    domain_is_design_domain = logical(), domain_requires_precision_evaluation = logical(),
    domain_scope_message = character(), representativity_flag = character(), representativity_note = character()
  )
}

.enemdu_indicator_table_id <- function(indicator_id, group_vars, survey_type) {
  paste(
    "indicator_table",
    survey_type %||% "survey_type_unspecified",
    .enemdu_indicator_table_group_label(group_vars),
    paste(indicator_id, collapse = "__"),
    sep = "__"
  )
}

.enemdu_indicator_table_group_label <- function(group_vars) {
  if (is.null(group_vars) || length(group_vars) == 0) {
    return("total")
  }

  paste(as.character(group_vars), collapse = "|")
}
