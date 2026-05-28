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
#' @param ... Additional named arguments passed to `enemdu_kpi_ipm()` and,
#' when relevant, `enemdu_build_ipm_components()`.
#'
#' @return A structured list with preflight, validation, estimates, benchmarks,
#' comparison, and non-official validation metadata.
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
  ...
) {
  caller <- "enemdu_run_ipm_reproducibility"

  if (!is.data.frame(data)) {
    .enemdu_abort_invalid_data(caller = caller)
  }

  survey_type <- .enemdu_normalize_survey_type(survey_type, caller = caller)
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
    component_cols = component_cols,
    score_var = score_var,
    tpm_var = tpm_var,
    tpem_var = tpem_var,
    strict = isTRUE(strict),
    components_diagnostics = prepared$component_diagnostics,
    flags_diagnostics = prepared$flags_diagnostics,
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
