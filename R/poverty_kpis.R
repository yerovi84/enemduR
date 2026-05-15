#' Estimate income poverty KPIs from explicit poverty lines
#'
#' Builds income-poverty and extreme-income-poverty flags from explicit,
#' auditable poverty lines and estimates their incidence with the package
#' survey-design-aware indicator estimator.
#'
#' This function does not derive official poverty lines, does not ingest CPI
#' series, and does not claim official validation. Official validation requires
#' comparing estimates against official INEC tabulations and documenting the
#' evidence.
#'
#' @param data A data frame.
#' @param group_vars Optional grouping variables.
#' @param income_var Household per-capita income variable. Defaults to
#' `"ingtot_pc"`.
#' @param period Period identifier used for poverty-line metadata and strict
#' registry lookup.
#' @param mode One of `"strict"` or `"manual"`.
#' @param poverty_line Manual poverty line. Required in manual mode.
#' @param extreme_poverty_line Manual extreme poverty line. Required in manual
#' mode.
#' @param poverty_lines Poverty-line registry. Defaults to the package template
#' registry.
#' @param line_source Source note required in manual mode.
#' @param poverty_var Output poverty flag variable. Defaults to `"pobre"`.
#' @param extreme_poverty_var Output extreme poverty flag variable. Defaults to
#' `"expobre"`.
#' @param add_line_vars Logical. If `TRUE`, adds poverty-line variables to the
#' working data before estimation.
#' @param overwrite Logical. If `TRUE`, overwrites existing poverty output
#' variables.
#' @param registry Indicator registry used by `enemdu_indicator_estimate()`.
#' @param official_validation_status Status label for official validation.
#' Defaults to `"not_officially_validated"`.
#' @param official_validation_note Note explaining the official validation
#' status.
#' @param ... Additional arguments passed to `enemdu_indicator_estimate()`.
#'
#' @return A tibble with income-poverty KPI estimates and poverty-line
#' provenance metadata.
#' @export
#'
#' @examples
#' \dontrun{
#' data <- tibble::tibble(
#'   idhogar = c("h1", "h1", "h2", "h2"),
#'   hsize = c(2L, 2L, 2L, 2L),
#'   upm = c(1, 2, 3, 4),
#'   estrato = c(1, 1, 2, 2),
#'   fexp = c(1, 1, 1, 1),
#'   ingtot_pc = c(40, 75, 125, 150)
#' )
#'
#' enemdu_kpi_income_poverty(
#'   data = data,
#'   period = "2025-12",
#'   mode = "manual",
#'   poverty_line = 100,
#'   extreme_poverty_line = 50,
#'   line_source = "Synthetic manual lines for examples; not official validation.",
#'   survey_type = "anual",
#'   sample_n_min = 1
#' )
#' }
enemdu_kpi_income_poverty <- function(data,
                                      group_vars = NULL,
                                      income_var = "ingtot_pc",
                                      period = NULL,
                                      mode = c("strict", "manual"),
                                      poverty_line = NULL,
                                      extreme_poverty_line = NULL,
                                      poverty_lines = enemdu_poverty_line_registry(),
                                      line_source = NULL,
                                      poverty_var = "pobre",
                                      extreme_poverty_var = "expobre",
                                      add_line_vars = TRUE,
                                      overwrite = FALSE,
                                      registry = enemdu_indicator_registry(),
                                      official_validation_status = "not_officially_validated",
                                      official_validation_note = "Estimates have not been compared against official INEC tabulations.",
                                      ...) {
  if (!is.data.frame(data)) {
    .enemdu_abort_invalid_data(caller = "enemdu_kpi_income_poverty")
  }

  .enemdu_abort_missing_vars(
    vars = income_var,
    names_data = names(data),
    caller = "enemdu_kpi_income_poverty"
  )

  if (!is.numeric(data[[income_var]])) {
    .enemdu_abort_invalid_numeric_var(
      var = income_var,
      caller = "enemdu_kpi_income_poverty"
    )
  }

  mode <- match.arg(mode)

  out_data <- enemdu_build_poverty_flags(
    data = data,
    period = period,
    income_var = income_var,
    poverty_line = poverty_line,
    extreme_poverty_line = extreme_poverty_line,
    poverty_lines = poverty_lines,
    mode = mode,
    line_source = line_source,
    poverty_var = poverty_var,
    extreme_poverty_var = extreme_poverty_var,
    add_line_vars = add_line_vars,
    overwrite = overwrite
  )

  poverty_line_metadata <- attr(out_data, "poverty_line_metadata")

  outputs <- list(
    enemdu_indicator_estimate(
      data = out_data,
      indicator_id = "pobreza_ingresos",
      value = poverty_var,
      group_vars = group_vars,
      registry = registry,
      ...
    ),
    enemdu_indicator_estimate(
      data = out_data,
      indicator_id = "pobreza_extrema_ingresos",
      value = extreme_poverty_var,
      group_vars = group_vars,
      registry = registry,
      ...
    )
  )

  result <- .enemdu_bind_estimate_rows(outputs)
  result <- .enemdu_add_income_poverty_kpi_metadata(
    result = result,
    poverty_line_metadata = poverty_line_metadata,
    mode = mode,
    income_var = income_var,
    poverty_var = poverty_var,
    extreme_poverty_var = extreme_poverty_var,
    official_validation_status = official_validation_status,
    official_validation_note = official_validation_note
  )

  attr(result, "poverty_kpi_policy") <- list(
    income_var = income_var,
    poverty_var = poverty_var,
    extreme_poverty_var = extreme_poverty_var,
    mode = mode,
    period = period,
    official_validation_status = official_validation_status,
    note = paste(
      "Income poverty KPI estimates require explicit and auditable poverty lines.",
      "Official validation requires comparison against official INEC tabulations."
    )
  )

  class(result) <- unique(c("enemdu_income_poverty_kpi", class(result)))
  result
}

.enemdu_add_income_poverty_kpi_metadata <- function(result,
                                                    poverty_line_metadata,
                                                    mode,
                                                    income_var,
                                                    poverty_var,
                                                    extreme_poverty_var,
                                                    official_validation_status,
                                                    official_validation_note) {
  result <- tibble::as_tibble(result)

  result[["poverty_line_period"]] <- NA_character_
  result[["poverty_line_mode"]] <- mode
  result[["poverty_income_var"]] <- income_var
  result[["poverty_flag_var"]] <- NA_character_
  result[["poverty_line_type"]] <- NA_character_
  result[["poverty_line_value"]] <- NA_real_
  result[["poverty_line_currency"]] <- NA_character_
  result[["poverty_line_source_status"]] <- NA_character_
  result[["poverty_line_source_note"]] <- NA_character_
  result[["poverty_line_update_method"]] <- NA_character_
  result[["official_validation_status"]] <- official_validation_status
  result[["official_validation_note"]] <- official_validation_note

  if (nrow(result) == 0 || !is.data.frame(poverty_line_metadata)) {
    return(result)
  }

  for (i in seq_len(nrow(result))) {
    indicator_id <- as.character(result$indicator_id[[i]])

    if (identical(indicator_id, "pobreza_ingresos")) {
      line_type <- "poverty"
      flag_var <- poverty_var
    } else if (identical(indicator_id, "pobreza_extrema_ingresos")) {
      line_type <- "extreme_poverty"
      flag_var <- extreme_poverty_var
    } else {
      next
    }

    line_row <- poverty_line_metadata[
      as.character(poverty_line_metadata$line_type) == line_type,
      ,
      drop = FALSE
    ]

    if (nrow(line_row) < 1) {
      next
    }

    line_row <- line_row[1, , drop = FALSE]

    result$poverty_flag_var[[i]] <- flag_var
    result$poverty_line_type[[i]] <- line_type
    result$poverty_line_period[[i]] <- .enemdu_poverty_kpi_chr(line_row, "period")
    result$poverty_line_value[[i]] <- .enemdu_poverty_kpi_num(line_row, "line_value")
    result$poverty_line_currency[[i]] <- .enemdu_poverty_kpi_chr(line_row, "currency")
    result$poverty_line_source_status[[i]] <- .enemdu_poverty_kpi_chr(line_row, "source_status")
    result$poverty_line_source_note[[i]] <- .enemdu_poverty_kpi_chr(line_row, "source_note")
    result$poverty_line_update_method[[i]] <- .enemdu_poverty_kpi_chr(line_row, "update_method")
  }

  result
}

.enemdu_poverty_kpi_chr <- function(row, col) {
  if (!col %in% names(row)) {
    return(NA_character_)
  }

  value <- row[[col]][[1]]
  if (is.null(value) || length(value) != 1 || is.na(value)) {
    return(NA_character_)
  }

  as.character(value)
}

.enemdu_poverty_kpi_num <- function(row, col) {
  if (!col %in% names(row)) {
    return(NA_real_)
  }

  value <- suppressWarnings(as.numeric(row[[col]][[1]]))
  if (length(value) != 1 || is.na(value)) {
    return(NA_real_)
  }

  value
}
