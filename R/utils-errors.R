# Internal error and warning helpers --------------------------------------

.enemdu_abort_not_implemented <- function(function_name) {
  rlang::abort(
    message = glue::glue(
      "`{function_name}()` is declared in the package API but is not implemented yet. ",
      "This function is scheduled for a later phase once contracts and metadata are closed."
    ),
    class = c("enemdu_error_not_implemented", "enemdu_error")
  )
}

.enemdu_abort_missing_argument <- function(arg, caller = NULL) {
  caller <- caller %||% "enemdu_internal"
  rlang::abort(
    message = glue::glue("Argument `{arg}` is required in `{caller}()`."),
    class = c("enemdu_error_missing_argument", "enemdu_error")
  )
}

.enemdu_abort_invalid_choice <- function(arg, value, choices, caller = NULL) {
  caller <- caller %||% "enemdu_internal"
  rlang::abort(
    message = glue::glue(
      "Invalid value for `{arg}` in `{caller}()`: `{value}`. ",
      "Valid choices are: {paste(choices, collapse = ', ')}."
    ),
    class = c("enemdu_error_invalid_choice", "enemdu_error")
  )
}

.enemdu_abort_missing_vars <- function(vars, names_data, caller = NULL) {
  caller <- caller %||% "enemdu_internal"
  missing_vars <- setdiff(vars, names_data)

  if (length(missing_vars) == 0) {
    return(invisible(TRUE))
  }

  rlang::abort(
    message = glue::glue(
      "Missing required variables in `{caller}()`: {paste(missing_vars, collapse = ', ')}."
    ),
    class = c("enemdu_error_missing_vars", "enemdu_error"),
    missing_vars = missing_vars
  )
}

.enemdu_abort_invalid_data <- function(caller = NULL) {
  caller <- caller %||% "enemdu_internal"
  rlang::abort(
    message = glue::glue("`data` must be a data frame in `{caller}()`."),
    class = c("enemdu_error_invalid_data", "enemdu_error")
  )
}

.enemdu_abort_invalid_numeric_var <- function(var, caller = NULL) {
  caller <- caller %||% "enemdu_internal"
  rlang::abort(
    message = glue::glue("Variable `{var}` must be numeric in `{caller}()`."),
    class = c("enemdu_error_invalid_numeric_var", "enemdu_error")
  )
}

.enemdu_abort_invalid_hsize <- function(hsize, caller = NULL) {
  caller <- caller %||% "enemdu_internal"
  rlang::abort(
    message = glue::glue(
      "Household size variable `{hsize}` must exist, be numeric, and contain positive values."
    ),
    class = c("enemdu_error_invalid_hsize", "enemdu_error")
  )
}

.enemdu_abort_invalid_registry <- function(registry_name, message, caller = NULL) {
  caller <- caller %||% "enemdu_internal"
  rlang::abort(
    message = glue::glue("Invalid `{registry_name}` in `{caller}()`: {message}"),
    class = c("enemdu_error_invalid_registry", "enemdu_error")
  )
}

.enemdu_abort_missing_poverty_line <- function(period, line_type, caller = NULL) {
  caller <- caller %||% "enemdu_internal"
  rlang::abort(
    message = glue::glue(
      "No valid poverty line was found for period `{period}` and line type `{line_type}` in `{caller}()`. ",
      "Poverty indicators cannot be derived without explicit and auditable poverty-line parameters."
    ),
    class = c("enemdu_error_missing_poverty_line", "enemdu_error")
  )
}

.enemdu_abort_invalid_poverty_line <- function(message, caller = NULL) {
  caller <- caller %||% "enemdu_internal"
  rlang::abort(
    message = glue::glue(
      "Invalid poverty-line configuration in `{caller}()`: {message}"
    ),
    class = c("enemdu_error_invalid_poverty_line", "enemdu_error")
  )
}

.enemdu_warn_comparability <- function(note, caller = NULL) {
  caller <- caller %||% "enemdu_internal"
  rlang::warn(
    message = glue::glue(
      "Comparability alert from `{caller}()`: {note}"
    ),
    class = c("enemdu_warning_comparability", "enemdu_warning")
  )
}

.enemdu_warn_missing_normalization <- function(message, caller = NULL) {
  caller <- caller %||% "enemdu_internal"
  rlang::warn(
    message = glue::glue(
      "Missing-value normalization warning from `{caller}()`: {message}"
    ),
    class = c("enemdu_warning_missing_normalization", "enemdu_warning")
  )
}

.enemdu_inform_representativity_scope <- function(message, caller = NULL) {
  caller <- caller %||% "enemdu_internal"
  rlang::inform(
    message = glue::glue(
      "Representativity scope from `{caller}()`: {message}"
    ),
    class = c("enemdu_message_representativity_scope", "enemdu_message")
  )
}

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
