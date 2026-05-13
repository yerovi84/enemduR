# Internal metadata helpers ------------------------------------------------

.enemdu_supported_survey_types <- function() {
  c("mensual", "trimestral", "anual")
}

.enemdu_default_design_vars <- function() {
  c("upm", "estrato", "fexp")
}

.enemdu_extdata_path <- function(file) {
  system.file("extdata", file, package = "enemduR")
}

.enemdu_read_csv_registry <- function(file) {
  path <- .enemdu_extdata_path(file)

  if (!nzchar(path)) {
    rlang::abort(
      message = glue::glue("Metadata file `{file}` was not found in `inst/extdata`."),
      class = c("enemdu_error_missing_metadata_file", "enemdu_error")
    )
  }

  readr::read_csv(path, show_col_types = FALSE, progress = FALSE)
}

.enemdu_survey_registry <- function() {
  .enemdu_read_csv_registry("survey_registry.csv")
}

.enemdu_domain_registry <- function() {
  .enemdu_read_csv_registry("domain_registry.csv")
}

.enemdu_analysis_level_registry <- function() {
  .enemdu_read_csv_registry("analysis_level_registry.csv")
}

.enemdu_representativity_thresholds <- function() {
  .enemdu_read_csv_registry("representativity_thresholds.csv")
}

.enemdu_comparability_registry <- function() {
  .enemdu_read_csv_registry("comparability_registry.csv")
}

.enemdu_variable_catalog <- function() {
  .enemdu_read_csv_registry("variable_catalog.csv")
}

.enemdu_indicator_registry <- function() {
  .enemdu_read_csv_registry("indicator_registry.csv")
}

.enemdu_validation_registry <- function() {
  .enemdu_read_csv_registry("validation_registry.csv")
}

.enemdu_income_component_registry <- function() {
  .enemdu_read_csv_registry("income_component_registry.csv")
}

.enemdu_missing_code_registry <- function() {
  .enemdu_read_csv_registry("missing_code_registry.csv")
}

.enemdu_value_range_registry <- function() {
  .enemdu_read_csv_registry("value_range_registry.csv")
}

.enemdu_poverty_line_registry <- function() {
  .enemdu_read_csv_registry("poverty_line_registry.csv")
}

.enemdu_optional_bonus_registry <- function() {
  .enemdu_read_csv_registry("optional_bonus_registry.csv")
}

.enemdu_normalize_survey_type <- function(survey_type, caller = "enemdu_internal") {
  if (is.null(survey_type) || length(survey_type) != 1 || is.na(survey_type)) {
    .enemdu_abort_missing_argument("survey_type", caller = caller)
  }

  survey_type <- tolower(trimws(as.character(survey_type)))
  choices <- .enemdu_supported_survey_types()

  if (!survey_type %in% choices) {
    .enemdu_abort_invalid_choice(
      arg = "survey_type",
      value = survey_type,
      choices = choices,
      caller = caller
    )
  }

  survey_type
}

.enemdu_find_comparability_notes <- function(period = NULL, survey_type = NULL) {
  if (is.null(period) || is.null(survey_type)) {
    return(character(0))
  }

  registry <- .enemdu_comparability_registry()
  period <- as.character(period)
  survey_type <- .enemdu_normalize_survey_type(
    survey_type,
    caller = ".enemdu_find_comparability_notes"
  )

  matches <- logical(nrow(registry))

  for (i in seq_len(nrow(registry))) {
    scope_values <- trimws(unlist(strsplit(registry$survey_type_scope[i], ",")))
    scope_ok <- survey_type %in% scope_values

    start_period <- registry$start_period[i]
    end_period <- registry$end_period[i]

    period_ok <- FALSE

    if (nchar(start_period) == nchar(period) && nchar(end_period) == nchar(period)) {
      period_ok <- period >= start_period && period <= end_period
    }

    if (scope_ok && period_ok) {
      matches[i] <- TRUE
    }
  }

  registry$note[matches]
}

.enemdu_get_survey_registry_row <- function(survey_type) {
  survey_type <- .enemdu_normalize_survey_type(
    survey_type,
    caller = ".enemdu_get_survey_registry_row"
  )

  registry <- .enemdu_survey_registry()
  row <- registry[registry$survey_type == survey_type, , drop = FALSE]

  if (nrow(row) != 1) {
    rlang::abort(
      message = glue::glue("Survey type `{survey_type}` was not found uniquely in survey registry."),
      class = c("enemdu_error_invalid_survey_registry", "enemdu_error")
    )
  }

  row
}

.enemdu_get_threshold <- function(threshold_id) {
  thresholds <- .enemdu_representativity_thresholds()
  row <- thresholds[thresholds$threshold_id == threshold_id, , drop = FALSE]

  if (nrow(row) != 1) {
    rlang::abort(
      message = glue::glue("Threshold `{threshold_id}` was not found uniquely."),
      class = c("enemdu_error_invalid_threshold", "enemdu_error")
    )
  }

  row
}
