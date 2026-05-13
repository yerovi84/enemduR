#' Build poverty and extreme poverty flags
#'
#' Classifies poverty and extreme poverty using per-capita household income and
#' explicit poverty-line parameters. In strict mode, poverty lines are resolved
#' from the poverty-line registry. In manual mode, both poverty lines must be
#' provided explicitly together with a source note.
#'
#' This function deliberately refuses to derive poverty indicators from implicit
#' or non-auditable poverty lines.
#'
#' @param data A data frame.
#' @param period Period identifier used to resolve poverty lines in strict mode.
#' @param income_var Name of the income per capita variable. Defaults to
#' `"ingtot_pc"`.
#' @param poverty_line Manual poverty line. Required in manual mode.
#' @param extreme_poverty_line Manual extreme poverty line. Required in manual
#' mode.
#' @param poverty_lines Poverty-line registry. Defaults to package registry.
#' @param mode One of `"strict"`, `"manual"` or `"diagnostic_only"`.
#' @param line_source Source note required in manual mode.
#' @param poverty_var Output poverty flag variable. Defaults to `"pobre"`.
#' @param extreme_poverty_var Output extreme poverty flag variable. Defaults to
#' `"expobre"`.
#' @param add_line_vars Logical. If `TRUE`, adds `linea_pobreza` and
#' `linea_pobreza_extrema` to the data.
#' @param overwrite Logical. If `TRUE`, overwrites existing output variables.
#'
#' @return A data frame with poverty flags, or the unchanged data with a
#' diagnostic attribute when `mode = "diagnostic_only"`.
#' @export
enemdu_build_poverty_flags <- function(data,
                                       period = NULL,
                                       income_var = "ingtot_pc",
                                       poverty_line = NULL,
                                       extreme_poverty_line = NULL,
                                       poverty_lines = enemdu_poverty_line_registry(),
                                       mode = c("strict", "manual", "diagnostic_only"),
                                       line_source = NULL,
                                       poverty_var = "pobre",
                                       extreme_poverty_var = "expobre",
                                       add_line_vars = TRUE,
                                       overwrite = FALSE) {
  mode <- match.arg(mode)

  if (!is.data.frame(data)) {
    .enemdu_abort_invalid_data(caller = "enemdu_build_poverty_flags")
  }

  .enemdu_abort_missing_vars(
    vars = income_var,
    names_data = names(data),
    caller = "enemdu_build_poverty_flags"
  )

  if (!is.numeric(data[[income_var]])) {
    .enemdu_abort_invalid_numeric_var(
      var = income_var,
      caller = "enemdu_build_poverty_flags"
    )
  }

  diagnostic <- .enemdu_poverty_input_report(
    data = data,
    period = period,
    income_var = income_var,
    mode = mode,
    poverty_line = poverty_line,
    extreme_poverty_line = extreme_poverty_line,
    line_source = line_source
  )

  if (identical(mode, "diagnostic_only")) {
    attr(data, "poverty_input_report") <- diagnostic
    return(data)
  }

  output_vars <- c(poverty_var, extreme_poverty_var)

  if (isTRUE(add_line_vars)) {
    output_vars <- c(output_vars, "linea_pobreza", "linea_pobreza_extrema")
  }

  existing_outputs <- intersect(output_vars, names(data))
  if (length(existing_outputs) > 0 && !isTRUE(overwrite)) {
    rlang::abort(
      message = glue::glue(
        "Output variables already exist: {paste(existing_outputs, collapse = ', ')}. ",
        "Use `overwrite = TRUE` to replace them."
      ),
      class = c("enemdu_error_existing_poverty_output", "enemdu_error")
    )
  }

  resolved_lines <- .enemdu_resolve_poverty_lines(
    period = period,
    poverty_line = poverty_line,
    extreme_poverty_line = extreme_poverty_line,
    poverty_lines = poverty_lines,
    mode = mode,
    line_source = line_source
  )

  poverty_value <- resolved_lines$line_value[resolved_lines$line_type == "poverty"]
  extreme_value <- resolved_lines$line_value[resolved_lines$line_type == "extreme_poverty"]

  .enemdu_assert_positive_scalar(
    x = poverty_value,
    arg = "poverty_line",
    caller = "enemdu_build_poverty_flags"
  )

  .enemdu_assert_positive_scalar(
    x = extreme_value,
    arg = "extreme_poverty_line",
    caller = "enemdu_build_poverty_flags"
  )

  if (extreme_value > poverty_value) {
    .enemdu_abort_invalid_poverty_line(
      message = "extreme_poverty_line cannot be greater than poverty_line.",
      caller = "enemdu_build_poverty_flags"
    )
  }

  out <- data
  income <- out[[income_var]]
  valid_income <- !is.na(income) & income > 0

  poverty_flag <- rep(NA_integer_, length(income))
  extreme_poverty_flag <- rep(NA_integer_, length(income))

  poverty_flag[valid_income] <- as.integer(income[valid_income] < poverty_value)
  extreme_poverty_flag[valid_income] <- as.integer(income[valid_income] < extreme_value)

  out[[poverty_var]] <- poverty_flag
  out[[extreme_poverty_var]] <- extreme_poverty_flag

  if (isTRUE(add_line_vars)) {
    out[["linea_pobreza"]] <- poverty_value
    out[["linea_pobreza_extrema"]] <- extreme_value
  }

  attr(out, "poverty_line_metadata") <- resolved_lines
  attr(out, "poverty_input_report") <- diagnostic
  attr(out, "poverty_flag_policy") <- list(
    mode = mode,
    income_var = income_var,
    poverty_var = poverty_var,
    extreme_poverty_var = extreme_poverty_var,
    valid_income_rule = paste(
      "Poverty flags are computed only for records where income_var is not missing",
      "and greater than zero."
    ),
    note = paste(
      "Poverty and extreme poverty were derived only after resolving explicit,",
      "positive, and auditable poverty lines."
    )
  )

  class(out) <- unique(c("enemdu_tbl", class(out)))
  out
}

.enemdu_resolve_poverty_lines <- function(period,
                                          poverty_line,
                                          extreme_poverty_line,
                                          poverty_lines,
                                          mode,
                                          line_source) {
  if (identical(mode, "strict")) {
    if (is.null(period) || length(period) != 1 || is.na(period)) {
      .enemdu_abort_missing_argument(
        "period",
        caller = "enemdu_build_poverty_flags"
      )
    }

    poverty_row <- enemdu_get_poverty_line(
      period = period,
      line_type = "poverty",
      registry = poverty_lines,
      mode = "strict"
    )

    extreme_row <- enemdu_get_poverty_line(
      period = period,
      line_type = "extreme_poverty",
      registry = poverty_lines,
      mode = "strict"
    )

    out <- rbind(poverty_row, extreme_row)
    row.names(out) <- NULL
    return(out)
  }

  if (identical(mode, "manual")) {
    if (is.null(line_source) || length(line_source) != 1 || is.na(line_source) || !nzchar(line_source)) {
      .enemdu_abort_invalid_poverty_line(
        message = "`line_source` must be declared in manual mode.",
        caller = "enemdu_build_poverty_flags"
      )
    }

    .enemdu_assert_positive_scalar(
      x = poverty_line,
      arg = "poverty_line",
      caller = "enemdu_build_poverty_flags"
    )

    .enemdu_assert_positive_scalar(
      x = extreme_poverty_line,
      arg = "extreme_poverty_line",
      caller = "enemdu_build_poverty_flags"
    )

    period_value <- if (is.null(period) || is.na(period)) NA_character_ else as.character(period)

    return(tibble::tibble(
      period = c(period_value, period_value),
      period_type = c("manual", "manual"),
      line_type = c("poverty", "extreme_poverty"),
      line_value = c(poverty_line, extreme_poverty_line),
      currency = c("USD", "USD"),
      ipc_value = c(NA_real_, NA_real_),
      base_line_value = c(NA_real_, NA_real_),
      base_period = c(NA_character_, NA_character_),
      update_method = c("manual_explicit", "manual_explicit"),
      source_status = c("manual", "manual"),
      source_note = c(line_source, line_source),
      valid_from = c(NA_character_, NA_character_),
      valid_to = c(NA_character_, NA_character_),
      notes = c(
        "Manual poverty line supplied by user.",
        "Manual extreme poverty line supplied by user."
      )
    ))
  }

  .enemdu_abort_invalid_choice(
    arg = "mode",
    value = mode,
    choices = c("strict", "manual", "diagnostic_only"),
    caller = ".enemdu_resolve_poverty_lines"
  )
}

.enemdu_poverty_input_report <- function(data,
                                         period,
                                         income_var,
                                         mode,
                                         poverty_line,
                                         extreme_poverty_line,
                                         line_source) {
  income <- data[[income_var]]

  tibble::tibble(
    component = c(
      "income_variable_exists",
      "income_variable_numeric",
      "income_non_missing",
      "income_positive",
      "period_declared",
      "poverty_line_manual_declared",
      "extreme_poverty_line_manual_declared",
      "line_source_declared"
    ),
    status = c(
      income_var %in% names(data),
      is.numeric(income),
      sum(!is.na(income)),
      sum(!is.na(income) & income > 0),
      !is.null(period) && length(period) == 1 && !is.na(period),
      !is.null(poverty_line) && length(poverty_line) == 1 && !is.na(poverty_line),
      !is.null(extreme_poverty_line) && length(extreme_poverty_line) == 1 && !is.na(extreme_poverty_line),
      !is.null(line_source) && length(line_source) == 1 && !is.na(line_source) && nzchar(line_source)
    ),
    mode = mode,
    message = c(
      glue::glue("Income variable `{income_var}` was found."),
      glue::glue("Income variable `{income_var}` is numeric."),
      "Number of non-missing income values.",
      "Number of positive income values used for poverty classification.",
      "Whether period was declared.",
      "Whether manual poverty line was declared.",
      "Whether manual extreme poverty line was declared.",
      "Whether manual line source was declared."
    )
  )
}
