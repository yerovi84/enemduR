#' Run a functional smoke test on ENEMDU microdata
#'
#' Runs a minimal functional check over ENEMDU microdata. The smoke test is not a
#' substitute for methodological validation or final statistical reporting. Its
#' purpose is to verify that the core analytical pipeline can execute on real or
#' representative microdata:
#'
#' - optional structural validation against an official dictionary,
#' - income-variable construction,
#' - validated social-bonus construction,
#' - basic indicator estimation,
#' - social-bonus KPI estimation.
#'
#' @param data A data frame with ENEMDU microdata.
#' @param dictionary Optional official dictionary tibble.
#' @param survey_type ENEMDU survey type: `"mensual"`, `"trimestral"` or
#' `"anual"`.
#' @param core_indicator_ids Character vector of core indicators to estimate
#' after variable construction.
#' @param domain_group_vars Optional grouping variables for a grouped smoke
#' estimate, for example `"area"`.
#' @param include_validation Logical. If `TRUE`, validates `data` against
#' `dictionary` and the internal microdata contract.
#' @param include_build_variables Logical. If `TRUE`, runs
#' `enemdu_build_variables()`.
#' @param include_social_bonuses Logical. If `TRUE`, runs
#' `enemdu_build_optional_bonuses()` and social-bonus KPIs.
#' @param include_core_indicators Logical. If `TRUE`, runs
#' `enemdu_indicator_table()` for `core_indicator_ids`.
#' @param sample_n_min Minimum sample size threshold passed to indicator
#' functions.
#' @param strict_domain Logical. Passed to inferential indicator tables.
#' @param keep_built_data Logical. If `TRUE`, returns the built data in the
#' output object. Defaults to `FALSE` to avoid storing large microdata objects.
#' @param emit Logical. If `TRUE`, emits a compact summary.
#'
#' @return A list with summary and smoke-test outputs.
#' @export
enemdu_smoke_test_microdata <- function(
    data,
    dictionary = NULL,
    survey_type,
    core_indicator_ids = c("ingreso_percapita_familiar", "ingreso_laboral"),
    domain_group_vars = NULL,
    include_validation = TRUE,
    include_build_variables = TRUE,
    include_social_bonuses = TRUE,
    include_core_indicators = TRUE,
    sample_n_min = 60,
    strict_domain = FALSE,
    keep_built_data = FALSE,
    emit = TRUE) {
  if (!is.data.frame(data)) {
    .enemdu_abort_invalid_data(caller = "enemdu_smoke_test_microdata")
  }

  if (missing(survey_type) || is.null(survey_type) || length(survey_type) != 1) {
    .enemdu_abort_missing_argument(
      "survey_type",
      caller = "enemdu_smoke_test_microdata"
    )
  }

  survey_type <- .enemdu_normalize_survey_type(
    survey_type = survey_type,
    caller = "enemdu_smoke_test_microdata"
  )

  summary_rows <- list()
  validation <- NULL
  built_data <- data
  core_indicators <- NULL
  domain_indicators <- NULL
  social_bonus_indicators <- NULL

  if (isTRUE(include_validation)) {
    validation_capture <- .enemdu_smoke_capture(
      enemdu_validate_microdata_against_dictionary(
        data = built_data,
        dictionary = dictionary,
        survey_type = survey_type,
        emit = FALSE
      )
    )

    validation <- validation_capture$value

    validation_errors <- if (is.data.frame(validation)) {
      sum(validation$severity == "error", na.rm = TRUE)
    } else {
      NA_integer_
    }

    validation_warnings <- if (is.data.frame(validation)) {
      sum(validation$severity == "warning", na.rm = TRUE)
    } else {
      NA_integer_
    }

    validation_status <- .enemdu_smoke_status(
      capture = validation_capture,
      error_count = validation_errors,
      warning_count = validation_warnings
    )

    summary_rows[[length(summary_rows) + 1L]] <- .enemdu_smoke_summary_row(
      step = "microdata_dictionary_validation",
      status = validation_status,
      rows = if (is.data.frame(validation)) nrow(validation) else NA_integer_,
      issues = validation_errors + validation_warnings,
      message = .enemdu_smoke_message(
        capture = validation_capture,
        ok_message = glue::glue(
          "Validation completed with {validation_errors} error-level issue(s) and {validation_warnings} warning-level issue(s)."
        )
      )
    )
  }

  if (isTRUE(include_build_variables)) {
    build_capture <- .enemdu_smoke_capture(
      enemdu_build_variables(built_data)
    )

    if (build_capture$ok) {
      built_data <- build_capture$value
    }

    summary_rows[[length(summary_rows) + 1L]] <- .enemdu_smoke_summary_row(
      step = "build_variables",
      status = .enemdu_smoke_status(build_capture),
      rows = if (is.data.frame(build_capture$value)) nrow(build_capture$value) else NA_integer_,
      issues = length(build_capture$warnings),
      message = .enemdu_smoke_message(
        capture = build_capture,
        ok_message = "Income and household variables were built."
      )
    )
  }

  if (isTRUE(include_social_bonuses)) {
    bonus_capture <- .enemdu_smoke_capture(
      enemdu_build_optional_bonuses(
        data = built_data,
        strict = FALSE,
        overwrite = FALSE
      )
    )

    if (bonus_capture$ok) {
      built_data <- bonus_capture$value
    }

    summary_rows[[length(summary_rows) + 1L]] <- .enemdu_smoke_summary_row(
      step = "build_social_bonuses",
      status = .enemdu_smoke_status(bonus_capture),
      rows = if (is.data.frame(bonus_capture$value)) nrow(bonus_capture$value) else NA_integer_,
      issues = length(bonus_capture$warnings),
      message = .enemdu_smoke_message(
        capture = bonus_capture,
        ok_message = "Validated social-bonus variables were built."
      )
    )
  }

  if (isTRUE(include_core_indicators)) {
    core_capture <- .enemdu_smoke_capture(
      enemdu_indicator_table(
        data = built_data,
        indicator_id = core_indicator_ids,
        group_vars = NULL,
        survey_type = survey_type,
        sample_n_min = sample_n_min,
        strict_domain = strict_domain,
        unsupported = "row",
        on_error = "row"
      )
    )

    core_indicators <- core_capture$value
    core_issues <- .enemdu_smoke_indicator_issue_count(core_indicators)

    summary_rows[[length(summary_rows) + 1L]] <- .enemdu_smoke_summary_row(
      step = "core_indicator_table",
      status = .enemdu_smoke_status(
        capture = core_capture,
        error_count = core_issues
      ),
      rows = if (is.data.frame(core_indicators)) nrow(core_indicators) else NA_integer_,
      issues = core_issues + length(core_capture$warnings),
      message = .enemdu_smoke_message(
        capture = core_capture,
        ok_message = "Core inferential indicator table was estimated."
      )
    )

    if (!is.null(domain_group_vars)) {
      domain_capture <- .enemdu_smoke_capture(
        enemdu_indicator_table(
          data = built_data,
          indicator_id = core_indicator_ids,
          group_vars = domain_group_vars,
          survey_type = survey_type,
          sample_n_min = sample_n_min,
          strict_domain = strict_domain,
          unsupported = "row",
          on_error = "row"
        )
      )

      domain_indicators <- domain_capture$value
      domain_issues <- .enemdu_smoke_indicator_issue_count(domain_indicators)

      summary_rows[[length(summary_rows) + 1L]] <- .enemdu_smoke_summary_row(
        step = "domain_indicator_table",
        status = .enemdu_smoke_status(
          capture = domain_capture,
          error_count = domain_issues
        ),
        rows = if (is.data.frame(domain_indicators)) nrow(domain_indicators) else NA_integer_,
        issues = domain_issues + length(domain_capture$warnings),
        message = .enemdu_smoke_message(
          capture = domain_capture,
          ok_message = glue::glue(
            "Grouped inferential indicator table was estimated by `{paste(domain_group_vars, collapse = '|')}`."
          )
        )
      )
    }
  }

  if (isTRUE(include_social_bonuses)) {
    social_capture <- .enemdu_smoke_capture(
      enemdu_kpi_social_bonuses(
        data = built_data,
        group_vars = NULL,
        survey_type = survey_type,
        sample_n_min = sample_n_min
      )
    )

    social_bonus_indicators <- social_capture$value
    social_issues <- .enemdu_smoke_indicator_issue_count(social_bonus_indicators)

    summary_rows[[length(summary_rows) + 1L]] <- .enemdu_smoke_summary_row(
      step = "social_bonus_kpis",
      status = .enemdu_smoke_status(
        capture = social_capture,
        error_count = social_issues
      ),
      rows = if (is.data.frame(social_bonus_indicators)) nrow(social_bonus_indicators) else NA_integer_,
      issues = social_issues + length(social_capture$warnings),
      message = .enemdu_smoke_message(
        capture = social_capture,
        ok_message = "Social-bonus survey KPI estimates were computed."
      )
    )
  }

  summary <- .enemdu_bind_smoke_summary_rows(summary_rows)

  result <- list(
    summary = summary,
    validation = validation,
    core_indicators = core_indicators,
    domain_indicators = domain_indicators,
    social_bonus_indicators = social_bonus_indicators
  )

  if (isTRUE(keep_built_data)) {
    result$built_data <- built_data
  }

  attr(result, "smoke_test_policy") <- list(
    survey_type = survey_type,
    core_indicator_ids = core_indicator_ids,
    domain_group_vars = domain_group_vars,
    sample_n_min = sample_n_min,
    strict_domain = strict_domain,
    keep_built_data = keep_built_data,
    note = paste(
      "This smoke test validates execution of the functional analytical pipeline.",
      "It does not certify final official results."
    )
  )

  class(result) <- unique(c("enemdu_smoke_test_result", class(result)))

  if (isTRUE(emit)) {
    n_error <- sum(summary$status == "error", na.rm = TRUE)
    n_warning <- sum(summary$status == "warning", na.rm = TRUE)

    rlang::inform(
      message = glue::glue(
        "ENEMDU smoke test completed: {n_error} error step(s), {n_warning} warning step(s)."
      ),
      class = c("enemdu_message_smoke_test", "enemdu_message")
    )
  }

  result
}

#' Run a functional smoke test on an ENEMDU microdata file
#'
#' Reads a microdata file and runs `enemdu_smoke_test_microdata()`.
#'
#' Supported formats are inherited from
#' `enemdu_validate_microdata_file_against_dictionary()`: `.sav`, `.dta`,
#' `.rds`, and `.csv`.
#'
#' @param path Path to the microdata file.
#' @param dictionary Optional official dictionary tibble.
#' @param n_max Optional maximum number of rows to read. Useful for fast smoke
#' tests before full-data execution.
#' @param ... Arguments passed to `enemdu_smoke_test_microdata()`.
#'
#' @return A smoke-test result object.
#' @export
enemdu_smoke_test_microdata_file <- function(path,
                                             dictionary = NULL,
                                             n_max = NULL,
                                             ...) {
  data <- .enemdu_read_microdata_file_for_validation(
    path = path,
    n_max = n_max
  )

  enemdu_smoke_test_microdata(
    data = data,
    dictionary = dictionary,
    ...
  )
}

.enemdu_smoke_capture <- function(expr) {
  warnings <- character(0)

  value <- tryCatch(
    {
      withCallingHandlers(
        expr,
        warning = function(w) {
          warnings <<- c(warnings, conditionMessage(w))
          invokeRestart("muffleWarning")
        }
      )
    },
    error = function(e) {
      e
    }
  )

  ok <- !inherits(value, "error")

  list(
    ok = ok,
    value = if (ok) value else NULL,
    error = if (ok) NA_character_ else conditionMessage(value),
    warnings = warnings
  )
}

.enemdu_smoke_status <- function(capture,
                                 error_count = 0,
                                 warning_count = 0) {
  if (!isTRUE(capture$ok)) {
    return("error")
  }

  if (!is.na(error_count) && error_count > 0) {
    return("error")
  }

  if (!is.na(warning_count) && warning_count > 0) {
    return("warning")
  }

  if (length(capture$warnings) > 0) {
    return("warning")
  }

  "ok"
}

.enemdu_smoke_message <- function(capture,
                                  ok_message) {
  if (!isTRUE(capture$ok)) {
    return(capture$error)
  }

  if (length(capture$warnings) > 0) {
    return(paste(unique(capture$warnings), collapse = " | "))
  }

  as.character(ok_message)
}

.enemdu_smoke_summary_row <- function(step,
                                      status,
                                      rows,
                                      issues,
                                      message) {
  tibble::tibble(
    step = as.character(step),
    status = as.character(status),
    rows = as.integer(rows),
    issues = as.integer(issues),
    message = as.character(message)
  )
}

.enemdu_bind_smoke_summary_rows <- function(rows) {
  if (length(rows) == 0) {
    return(tibble::tibble(
      step = character(),
      status = character(),
      rows = integer(),
      issues = integer(),
      message = character()
    ))
  }

  out <- do.call(rbind, rows)
  row.names(out) <- NULL
  tibble::as_tibble(out)
}

.enemdu_smoke_indicator_issue_count <- function(x) {
  if (!is.data.frame(x) || nrow(x) == 0) {
    return(0L)
  }

  issues <- 0L

  if ("table_status" %in% names(x)) {
    issues <- issues + sum(!is.na(x$table_status) & x$table_status != "estimated")
  }

  if ("decision" %in% names(x)) {
    issues <- issues + sum(!is.na(x$decision) & x$decision %in% c(
      "precision_evaluation_error",
      "not_estimated"
    ))
  }

  as.integer(issues)
}
