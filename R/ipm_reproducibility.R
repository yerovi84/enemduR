#' Preflight checks for IPM reproducibility workflows
#'
#' Checks whether the minimum variables required to estimate IPM KPIs are
#' present and usable. This function validates already-built IPM score and flag
#' variables plus survey design variables. It does not process microdata files,
#' derive raw IPM components, or claim official validation.
#'
#' @param data A data frame.
#' @param survey_type ENEMDU survey type.
#' @param ids Primary sampling unit variable.
#' @param strata Survey strata variable.
#' @param weight Survey expansion factor variable.
#' @param score_var Row-level IPM score variable.
#' @param tpm_var Row-level multidimensional poverty flag variable.
#' @param tpem_var Row-level extreme multidimensional poverty flag variable.
#' @param strict Logical. If `TRUE`, abort when preflight fails.
#'
#' @return A tibble with one row per required variable and a
#' `preflight_passed` attribute.
#' @export
enemdu_validate_ipm_reproducibility_inputs <- function(
  data,
  survey_type = "anual",
  ids = "upm",
  strata = "estrato",
  weight = "fexp",
  score_var = "ipm_score",
  tpm_var = "tpm",
  tpem_var = "tpem",
  strict = TRUE
) {
  caller <- "enemdu_validate_ipm_reproducibility_inputs"

  if (!is.data.frame(data)) {
    .enemdu_abort_invalid_data(caller = caller)
  }

  survey_type <- .enemdu_normalize_survey_type(survey_type, caller = caller)

  spec <- .enemdu_ipm_reproducibility_required_spec(
    ids = ids,
    strata = strata,
    weight = weight,
    score_var = score_var,
    tpm_var = tpm_var,
    tpem_var = tpem_var
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
      issue <- .enemdu_ipm_reproducibility_variable_issue(
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
  out[["survey_type"]] <- survey_type

  attr(out, "preflight_passed") <- all(out$issue == "ok")
  class(out) <- unique(c("enemdu_ipm_reproducibility_preflight", class(out)))

  if (isTRUE(strict) && !isTRUE(attr(out, "preflight_passed"))) {
    .enemdu_abort_ipm_reproducibility_preflight(out)
  }

  out
}

#' Run IPM reproducibility workflow
#'
#' Runs a local IPM reproducibility workflow: validates inputs, estimates IPM
#' KPIs, loads published IPM benchmarks, and compares local estimates to those
#' benchmarks. It consumes already-built score and flag variables or can build
#' flags from registered component columns. It does not implement new raw IPM
#' component rules and does not read microdata files directly.
#'
#' Benchmarks are published comparison values, not institutional validation.
#'
#' @param data A data frame.
#' @param period Benchmark period.
#' @param survey_type ENEMDU survey type.
#' @param by Optional domain variable. Defaults to `"area"`.
#' @param ids Primary sampling unit variable.
#' @param strata Survey strata variable.
#' @param weight Survey expansion factor variable.
#' @param build_components Logical. If `TRUE`, call
#' `enemdu_build_ipm_components()` before flag validation.
#' @param build_flags Logical. If `TRUE`, build row-level flags from registered
#' components when available.
#' @param strict Logical. If `TRUE`, abort on invalid reproducibility inputs.
#' @param tolerance_pp Benchmark-comparison tolerance in percentage points.
#' @param missing_component_policy Missing IPM evidence policy. `"error"`
#' refuses incomplete registered components or score/flag inputs. `"complete_case"`
#' excludes rows with incomplete registered components, or incomplete score/flag
#' inputs when components are unavailable, before estimation.
#' @param ... Additional named arguments passed to `enemdu_kpi_ipm()` and,
#' when relevant, `enemdu_build_ipm_components()`.
#'
#' @return A structured list with preflight, validation, estimates, benchmarks,
#' comparison, complete-case diagnostics, and non-official validation metadata.
#' @export
enemdu_run_ipm_reproducibility <- function(
  data,
  period = "2025-12",
  survey_type = "anual",
  by = "area",
  ids = "upm",
  strata = "estrato",
  weight = "fexp",
  build_components = FALSE,
  build_flags = TRUE,
  strict = TRUE,
  tolerance_pp = 0.5,
  missing_component_policy = c("error", "complete_case"),
  ...
) {
  caller <- "enemdu_run_ipm_reproducibility"

  if (!is.data.frame(data)) {
    .enemdu_abort_invalid_data(caller = caller)
  }

  survey_type <- .enemdu_normalize_survey_type(survey_type, caller = caller)
  missing_component_policy <- match.arg(missing_component_policy)
  dots <- list(...)
  component_cols <- if (!is.null(dots$component_cols)) {
    dots$component_cols
  } else {
    NULL
  }

  score_var <- if (!is.null(dots$score_var)) {
    dots$score_var
  } else {
    "ipm_score"
  }

  tpm_var <- if (!is.null(dots$tpm_var)) {
    dots$tpm_var
  } else {
    "tpm"
  }

  tpem_var <- if (!is.null(dots$tpem_var)) {
    dots$tpem_var
  } else {
    "tpem"
  }

  dots_for_split <- dots[setdiff(
    names(dots),
    c("component_cols", "score_var", "tpm_var", "tpem_var")
  )]

  split_dots <- .enemdu_ipm_split_dots(dots_for_split)

  work_input <- data
  component_diagnostics <- NULL
  flags_diagnostics <- NULL

  if (isTRUE(build_components)) {
    component_args <- c(
      list(data = work_input, strict = strict),
      split_dots$component_dots
    )

    work_input <- do.call(enemdu_build_ipm_components, component_args)
    component_diagnostics <- attr(work_input, "ipm_component_diagnostics")
  }

  resolved_components <- .enemdu_resolve_ipm_components(component_cols)
  flag_vars <- c(score_var, tpm_var, tpem_var)

  complete_case <- .enemdu_ipm_reproducibility_complete_cases(
    data = work_input,
    component_cols = resolved_components$component_cols,
    flag_vars = flag_vars,
    weight = weight,
    by = by,
    missing_component_policy = missing_component_policy
  )
  complete_case_diagnostics <- complete_case$diagnostics
  complete_case_by_domain <- complete_case$by_domain

  if (
    identical(missing_component_policy, "error") &&
      complete_case_diagnostics$rows_excluded[[1]] > 0
  ) {
    .enemdu_abort_ipm_reproducibility_incomplete_cases(
      diagnostics = complete_case_diagnostics
    )
  }

  if (identical(missing_component_policy, "complete_case")) {
    if (complete_case_diagnostics$rows_complete[[1]] == 0) {
      .enemdu_abort_ipm_reproducibility_complete_case_empty(
        diagnostics = complete_case_diagnostics
      )
    }

    work_input <- work_input[complete_case$complete, , drop = FALSE]
  }

  components_available <- all(resolved_components$component_cols %in% names(work_input))
  flags_available <- all(flag_vars %in% names(work_input))

  if (isTRUE(build_flags) && isTRUE(components_available)) {
    work_data <- enemdu_build_ipm_flags(
      data = work_input,
      component_cols = resolved_components$component_cols,
      score_var = score_var,
      tpm_var = tpm_var,
      tpem_var = tpem_var,
      overwrite = flags_available,
      strict = strict
    )
    flags_diagnostics <- attr(work_data, "ipm_flags_diagnostics")
  } else if (!isTRUE(flags_available)) {
    missing_flags <- setdiff(flag_vars, names(work_input))

    rlang::abort(
      message = glue::glue(
        "IPM KPI inputs are incomplete. Missing score or flag variables: ",
        "{paste(missing_flags, collapse = ', ')}. ",
        "Provide prebuilt flags or registered component columns."
      ),
      class = c("enemdu_error_missing_ipm_kpi_inputs", "enemdu_error"),
      missing_vars = missing_flags
    )
  } else {
    work_data <- work_input
  }

  preflight <- enemdu_validate_ipm_reproducibility_inputs(
    data = work_data,
    survey_type = survey_type,
    ids = ids,
    strata = strata,
    weight = weight,
    score_var = score_var,
    tpm_var = tpm_var,
    tpem_var = tpem_var,
    strict = strict
  )

  validation <- tibble::tibble(
    validation_status = if (isTRUE(attr(preflight, "preflight_passed"))) {
      "passed"
    } else {
      "failed"
    },
    preflight_passed = isTRUE(attr(preflight, "preflight_passed")),
    strict = isTRUE(strict),
    missing_component_policy = missing_component_policy,
    official_validation_status = "not_officially_validated"
  )

  national_estimates <- do.call(
    enemdu_kpi_ipm,
    c(
      list(
        data = work_data,
        survey_type = survey_type,
        by = NULL,
        ids = ids,
        strata = strata,
        weight = weight,
        build_components = FALSE,
        build_flags = FALSE,
        component_cols = component_cols,
        score_var = score_var,
        tpm_var = tpm_var,
        tpem_var = tpem_var,
        strict = strict
      ),
      split_dots$survey_dots
    )
  )

  estimate_pieces <- list(national_estimates)

  if (!is.null(by)) {
    domain_estimates <- do.call(
      enemdu_kpi_ipm,
      c(
        list(
          data = work_data,
          survey_type = survey_type,
          by = by,
          ids = ids,
          strata = strata,
          weight = weight,
          build_components = FALSE,
          build_flags = FALSE,
          component_cols = component_cols,
          score_var = score_var,
          tpm_var = tpm_var,
          tpem_var = tpem_var,
          strict = strict
        ),
        split_dots$survey_dots
      )
    )
    estimate_pieces[[length(estimate_pieces) + 1L]] <- domain_estimates
  }

  estimates <- .enemdu_ipm_bind_rows(estimate_pieces)
  estimates[["period"]] <- as.character(period)

  benchmarks <- enemdu_official_ipm_benchmarks(
    period = period,
    survey_type = survey_type
  )

  comparison_input <- estimates[
    estimates$indicator_id %in% c("tpm", "tpem", "ipm"),
    ,
    drop = FALSE
  ]

  comparison <- enemdu_compare_official_ipm(
    estimates = comparison_input,
    benchmarks = benchmarks,
    tolerance_pp = tolerance_pp
  )

  result <- list(
    preflight = preflight,
    validation = validation,
    estimates = estimates,
    benchmarks = benchmarks,
    comparison = comparison,
    complete_case_diagnostics = complete_case_diagnostics,
    complete_case_by_domain = complete_case_by_domain,
    official_validation_status = "not_officially_validated",
    official_validation_note = paste(
      "This workflow compares local IPM estimates against published benchmarks.",
      "It is not an official institutional validation claim."
    )
  )

  attr(result, "reproducibility_policy") <- list(
    period = period,
    survey_type = survey_type,
    by = by,
    ids = ids,
    strata = strata,
    weight = weight,
    build_components = isTRUE(build_components),
    build_flags = isTRUE(build_flags),
    tolerance_pp = tolerance_pp,
    missing_component_policy = missing_component_policy,
    component_cols = component_cols,
    score_var = score_var,
    tpm_var = tpm_var,
    tpem_var = tpem_var,
    strict = isTRUE(strict),
    components_diagnostics = component_diagnostics,
    flags_diagnostics = flags_diagnostics,
    complete_case_diagnostics = complete_case_diagnostics,
    complete_case_by_domain = complete_case_by_domain,
    official_validation_status = "not_officially_validated",
    note = paste(
      "IPM reproducibility workflow for local analytical comparison.",
      "Published benchmarks are not institutional validation."
    )
  )

  class(result) <- unique(c("enemdu_ipm_reproducibility_result", class(result)))
  result
}

.enemdu_ipm_reproducibility_required_spec <- function(ids,
                                                      strata,
                                                      weight,
                                                      score_var,
                                                      tpm_var,
                                                      tpem_var) {
  variables <- c(ids, strata, weight, score_var, tpm_var, tpem_var)
  roles <- c("psu", "strata", "weight", "score", "flag_tpm", "flag_tpem")
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

.enemdu_ipm_reproducibility_complete_cases <- function(data,
                                                       component_cols,
                                                       flag_vars,
                                                       weight,
                                                       by,
                                                       missing_component_policy) {
  components_available <- all(component_cols %in% names(data))
  flags_available <- all(flag_vars %in% names(data))

  if (isTRUE(components_available)) {
    complete <- stats::complete.cases(data[, component_cols, drop = FALSE])
    source <- "components"
    variables <- component_cols
  } else if (isTRUE(flags_available)) {
    complete <- stats::complete.cases(data[, flag_vars, drop = FALSE])
    source <- "flags"
    variables <- flag_vars
  } else {
    complete <- rep(TRUE, nrow(data))
    source <- "unavailable"
    variables <- character()
  }

  diagnostics <- .enemdu_ipm_reproducibility_complete_case_diagnostics(
    data = data,
    complete = complete,
    weight = weight,
    missing_component_policy = missing_component_policy,
    complete_case_source = source,
    complete_case_variables = variables
  )
  by_domain <- .enemdu_ipm_reproducibility_complete_case_by_domain(
    data = data,
    complete = complete,
    weight = weight,
    by = by
  )

  list(
    complete = complete,
    diagnostics = diagnostics,
    by_domain = by_domain
  )
}

.enemdu_ipm_reproducibility_complete_case_diagnostics <- function(data,
                                                                 complete,
                                                                 weight,
                                                                 missing_component_policy,
                                                                 complete_case_source,
                                                                 complete_case_variables) {
  weights <- .enemdu_ipm_reproducibility_weights(data, weight)
  rows_total <- length(complete)
  rows_complete <- sum(complete)
  rows_excluded <- rows_total - rows_complete
  weighted_total <- .enemdu_ipm_reproducibility_weighted_sum(weights)
  weighted_complete <- .enemdu_ipm_reproducibility_weighted_sum(weights[complete])
  weighted_excluded <- if (is.na(weighted_total) || is.na(weighted_complete)) {
    NA_real_
  } else {
    weighted_total - weighted_complete
  }

  tibble::tibble(
    rows_total = as.integer(rows_total),
    rows_complete = as.integer(rows_complete),
    rows_excluded = as.integer(rows_excluded),
    share_rows_excluded = .enemdu_ipm_reproducibility_share(
      numerator = rows_excluded,
      denominator = rows_total
    ),
    weighted_total = weighted_total,
    weighted_complete = weighted_complete,
    weighted_excluded = weighted_excluded,
    share_weighted_excluded = .enemdu_ipm_reproducibility_share(
      numerator = weighted_excluded,
      denominator = weighted_total
    ),
    missing_component_policy = missing_component_policy,
    complete_case_source = complete_case_source,
    complete_case_variables = paste(complete_case_variables, collapse = ", "),
    official_validation_status = "not_officially_validated"
  )
}

.enemdu_ipm_reproducibility_complete_case_by_domain <- function(data,
                                                               complete,
                                                               weight,
                                                               by) {
  out <- tibble::tibble(
    domain_variable = character(),
    domain_value = character(),
    complete_case_status = character(),
    n = integer(),
    weighted_n = numeric()
  )

  if (is.null(by)) {
    return(out)
  }

  by <- as.character(by)
  if (
    length(by) == 0 ||
      any(is.na(by)) ||
      any(!nzchar(by)) ||
      any(!by %in% names(data))
  ) {
    return(out)
  }

  domain_variable <- paste(by, collapse = "|")
  domain_value <- .enemdu_ipm_reproducibility_domain_value(data, by)
  complete_case_status <- ifelse(complete, "complete", "incomplete")
  weights <- .enemdu_ipm_reproducibility_weights(data, weight)

  groups <- unique(data.frame(
    domain_value = domain_value,
    complete_case_status = complete_case_status,
    stringsAsFactors = FALSE
  ))

  if (nrow(groups) == 0) {
    return(out)
  }

  rows <- vector("list", nrow(groups))

  for (i in seq_len(nrow(groups))) {
    same_domain <- domain_value == groups$domain_value[[i]]
    same_domain[is.na(same_domain)] <- is.na(domain_value[is.na(same_domain)]) &
      is.na(groups$domain_value[[i]])
    idx <- same_domain &
      complete_case_status == groups$complete_case_status[[i]]
    rows[[i]] <- tibble::tibble(
      domain_variable = domain_variable,
      domain_value = groups$domain_value[[i]],
      complete_case_status = groups$complete_case_status[[i]],
      n = as.integer(sum(idx)),
      weighted_n = .enemdu_ipm_reproducibility_weighted_sum(weights[idx])
    )
  }

  do.call(rbind, rows)
}

.enemdu_ipm_reproducibility_domain_value <- function(data, by) {
  values <- lapply(by, function(var) {
    out <- as.character(data[[var]])
    out[is.na(data[[var]])] <- NA_character_
    out
  })

  if (length(values) == 1) {
    return(values[[1]])
  }

  out <- do.call(paste, c(values, sep = "|"))
  has_missing <- Reduce(`|`, lapply(values, is.na))
  out[has_missing] <- NA_character_
  out
}

.enemdu_ipm_reproducibility_weights <- function(data, weight) {
  if (!weight %in% names(data)) {
    return(rep(NA_real_, nrow(data)))
  }

  suppressWarnings(as.numeric(data[[weight]]))
}

.enemdu_ipm_reproducibility_weighted_sum <- function(weights) {
  valid <- !is.na(weights) & is.finite(weights)

  if (!any(valid)) {
    return(NA_real_)
  }

  sum(weights[valid])
}

.enemdu_ipm_reproducibility_share <- function(numerator, denominator) {
  if (
    length(numerator) == 0 ||
      length(denominator) == 0 ||
      is.na(numerator) ||
      is.na(denominator) ||
      denominator == 0
  ) {
    return(NA_real_)
  }

  as.numeric(numerator) / as.numeric(denominator)
}

.enemdu_ipm_reproducibility_variable_issue <- function(role,
                                                       values,
                                                       present) {
  if (!isTRUE(present)) {
    return("missing_variable")
  }

  if (sum(!is.na(values)) == 0) {
    return("all_missing")
  }

  if (grepl("weight", role) && !is.numeric(values)) {
    return("not_numeric")
  }

  if (grepl("score", role)) {
    score <- .enemdu_coerce_ipm_numeric(values)

    if (any(!is.na(values) & is.na(score))) {
      return("not_numeric")
    }

    if (any(!is.na(score) & (!is.finite(score) | score < 0 | score > 1))) {
      return("invalid_score")
    }

    if (any(is.na(values))) {
      return("missing_values")
    }
  }

  if (grepl("flag", role)) {
    issue <- tryCatch(
      {
        .enemdu_coerce_ipm_component(values = values, var = role, strict = FALSE)
        "ok"
      },
      error = function(e) {
        "non_binary_flag"
      }
    )

    if (!identical(issue, "ok")) {
      return(issue)
    }

    if (any(is.na(values))) {
      return("missing_values")
    }
  }

  "ok"
}

.enemdu_abort_ipm_reproducibility_preflight <- function(preflight) {
  bad <- preflight[preflight$issue != "ok", , drop = FALSE]

  rlang::abort(
    message = glue::glue(
      "IPM reproducibility preflight failed for {nrow(bad)} variable(s)."
    ),
    class = c("enemdu_error_ipm_reproducibility_preflight_failed", "enemdu_error"),
    preflight = preflight
  )
}

.enemdu_abort_ipm_reproducibility_incomplete_cases <- function(diagnostics) {
  rlang::abort(
    message = glue::glue(
      "IPM reproducibility inputs contain incomplete IPM evidence in ",
      "{diagnostics$rows_excluded[[1]]} row(s). Use ",
      "`missing_component_policy = \"complete_case\"` to exclude incomplete ",
      "rows explicitly."
    ),
    class = c(
      "enemdu_error_ipm_reproducibility_incomplete_cases",
      "enemdu_error"
    ),
    complete_case_diagnostics = diagnostics
  )
}

.enemdu_abort_ipm_reproducibility_complete_case_empty <- function(diagnostics) {
  rlang::abort(
    message = paste(
      "Complete-case IPM reproducibility excluded all rows.",
      "At least one row with complete IPM evidence is required."
    ),
    class = c(
      "enemdu_error_ipm_reproducibility_complete_case_empty",
      "enemdu_error"
    ),
    complete_case_diagnostics = diagnostics
  )
}
