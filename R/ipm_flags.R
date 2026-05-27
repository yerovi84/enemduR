#' Build IPM score and flags from registered components
#'
#' Computes row-level IPM weighted deprivation scores and poverty flags from the
#' 12 already-built binary IPM component indicators declared in
#' `ipm_component_registry.csv`. This function does not derive the 12 indicators
#' from raw ENEMDU questionnaire variables.
#'
#' This function computes `ipm_score`, `tpm`, and `tpem` only. It does not
#' compute aggregate `A` or aggregate `IPM`; those require survey-design-aware
#' aggregation among multidimensionally poor people and belong to a future KPI
#' layer. This function does not claim official validation.
#'
#' @param data A data frame.
#' @param component_cols Optional character vector with the 12 binary IPM
#' component columns. If `NULL`, the expected component names are read from
#' `ipm_component_registry.csv`.
#' @param score_var Output weighted deprivation score variable.
#' @param tpm_var Output multidimensional poverty flag variable.
#' @param tpem_var Output extreme multidimensional poverty flag variable.
#' @param overwrite Logical. If `TRUE`, overwrite existing output variables.
#' @param strict Logical. If `TRUE`, abort on missing component columns, invalid
#' binary values, or missing component values. If `FALSE`, component missing
#' values are allowed to propagate to `ipm_score`, `tpm`, and `tpem`.
#'
#' @return A data frame with IPM score and flags plus an
#' `ipm_flags_diagnostics` attribute.
#' @export
enemdu_build_ipm_flags <- function(data,
                                   component_cols = NULL,
                                   score_var = "ipm_score",
                                   tpm_var = "tpm",
                                   tpem_var = "tpem",
                                   overwrite = FALSE,
                                   strict = TRUE) {
  if (!is.data.frame(data)) {
    .enemdu_abort_invalid_data(caller = "enemdu_build_ipm_flags")
  }

  .enemdu_assert_ipm_output_names(
    score_var = score_var,
    tpm_var = tpm_var,
    tpem_var = tpem_var
  )

  resolved <- .enemdu_resolve_ipm_components(component_cols)
  component_cols <- resolved$component_cols
  weights <- resolved$weights

  .enemdu_abort_missing_vars(
    vars = component_cols,
    names_data = names(data),
    caller = "enemdu_build_ipm_flags"
  )

  output_vars <- c(score_var, tpm_var, tpem_var)
  existing_outputs <- intersect(output_vars, names(data))

  if (length(existing_outputs) > 0 && !isTRUE(overwrite)) {
    rlang::abort(
      message = glue::glue(
        "IPM output variables already exist: {paste(existing_outputs, collapse = ', ')}. ",
        "Use `overwrite = TRUE` to replace them."
      ),
      class = c("enemdu_error_existing_ipm_output", "enemdu_error")
    )
  }

  component_data <- lapply(component_cols, function(var) {
    .enemdu_coerce_ipm_component(
      values = data[[var]],
      var = var,
      strict = strict
    )
  })
  component_data <- as.data.frame(component_data, optional = TRUE)
  names(component_data) <- component_cols

  complete_components <- stats::complete.cases(component_data)
  score <- as.numeric(as.matrix(component_data) %*% weights)
  score[!complete_components] <- NA_real_

  tpm_cutoff <- 1 / 3
  tpem_cutoff <- 1 / 2

  tpm <- .enemdu_ipm_cutoff_flag(score, tpm_cutoff)
  tpem <- .enemdu_ipm_cutoff_flag(score, tpem_cutoff)

  out <- data
  out[[score_var]] <- score
  out[[tpm_var]] <- tpm
  out[[tpem_var]] <- tpem

  attr(out, "ipm_flags_diagnostics") <- list(
    n_rows = nrow(out),
    n_components = length(component_cols),
    component_cols = component_cols,
    score_var = score_var,
    tpm_var = tpm_var,
    tpem_var = tpem_var,
    tpm_cutoff = tpm_cutoff,
    tpem_cutoff = tpem_cutoff,
    weights_sum = sum(weights),
    n_score_na = sum(is.na(score)),
    n_tpm_na = sum(is.na(tpm)),
    n_tpem_na = sum(is.na(tpem))
  )

  class(out) <- unique(c("enemdu_tbl", class(out)))
  out
}

.enemdu_ipm_component_registry <- function() {
  registry <- .enemdu_read_csv_registry("ipm_component_registry.csv")
  .enemdu_validate_ipm_component_registry(registry)
  registry
}

.enemdu_validate_ipm_component_registry <- function(registry) {
  required_cols <- c(
    "indicator_order",
    "expected_component_name",
    "indicator_weight"
  )

  missing_cols <- setdiff(required_cols, names(registry))

  if (length(missing_cols) > 0) {
    .enemdu_abort_invalid_registry(
      registry_name = "ipm_component_registry",
      message = glue::glue("Missing columns: {paste(missing_cols, collapse = ', ')}."),
      caller = ".enemdu_validate_ipm_component_registry"
    )
  }

  invisible(TRUE)
}

.enemdu_resolve_ipm_components <- function(component_cols = NULL) {
  registry <- .enemdu_ipm_component_registry()
  registry <- registry[order(registry$indicator_order), , drop = FALSE]

  registered_components <- as.character(registry$expected_component_name)
  registered_weights <- as.numeric(registry$indicator_weight)

  if (any(is.na(registered_components)) || any(!nzchar(registered_components))) {
    .enemdu_abort_invalid_ipm_component_registry(
      "Registered IPM component names must be non-missing and non-empty."
    )
  }

  if (anyDuplicated(registered_components) > 0) {
    .enemdu_abort_invalid_ipm_component_registry(
      "Registered IPM component names must be unique."
    )
  }

  if (length(registered_components) != 12) {
    .enemdu_abort_invalid_ipm_component_registry(
      "The IPM component registry must define exactly 12 components."
    )
  }

  if (any(is.na(registered_weights)) || any(!is.finite(registered_weights))) {
    .enemdu_abort_invalid_ipm_component_registry(
      "Registered IPM indicator weights must be finite numeric values."
    )
  }

  if (abs(sum(registered_weights) - 1) > .enemdu_ipm_tolerance()) {
    .enemdu_abort_invalid_ipm_component_registry(
      "Registered IPM indicator weights must sum to 1."
    )
  }

  if (is.null(component_cols)) {
    component_cols <- registered_components
  } else {
    component_cols <- as.character(component_cols)
  }

  if (
    length(component_cols) != 12 ||
      any(is.na(component_cols)) ||
      any(!nzchar(component_cols)) ||
      anyDuplicated(component_cols) > 0
  ) {
    rlang::abort(
      message = "`component_cols` must contain exactly 12 unique non-empty component names.",
      class = c("enemdu_error_invalid_ipm_components", "enemdu_error")
    )
  }

  component_match <- match(component_cols, registered_components)

  if (any(is.na(component_match))) {
    missing_weights <- component_cols[is.na(component_match)]
    rlang::abort(
      message = glue::glue(
        "IPM component columns are not registered: {paste(missing_weights, collapse = ', ')}."
      ),
      class = c("enemdu_error_unregistered_ipm_component", "enemdu_error")
    )
  }

  list(
    component_cols = component_cols,
    weights = registered_weights[component_match]
  )
}

.enemdu_coerce_ipm_numeric <- function(values) {
  missing <- is.na(values)

  if (is.factor(values)) {
    raw <- trimws(as.character(values))
  } else if (is.logical(values)) {
    raw <- as.integer(values)
  } else if (is.numeric(values) || is.integer(values)) {
    raw <- values
  } else if (is.character(values)) {
    raw <- trimws(values)
  } else {
    raw <- trimws(as.character(values))
  }

  numeric_values <- suppressWarnings(as.numeric(raw))
  numeric_values[missing] <- NA_real_

  numeric_values
}

.enemdu_coerce_ipm_component <- function(values, var, strict) {
  missing <- is.na(values)
  numeric_values <- .enemdu_coerce_ipm_numeric(values)

  invalid_conversion <- !missing & is.na(numeric_values)
  invalid_binary <- !missing & !is.na(numeric_values) & !(numeric_values %in% c(0, 1))

  if (any(invalid_conversion | invalid_binary, na.rm = TRUE)) {
    rlang::abort(
      message = glue::glue("IPM component variable `{var}` must contain binary 0/1 values."),
      class = c("enemdu_error_invalid_ipm_component", "enemdu_error")
    )
  }

  if (isTRUE(strict) && any(missing)) {
    rlang::abort(
      message = glue::glue("IPM component variable `{var}` contains missing values."),
      class = c("enemdu_error_missing_ipm_component", "enemdu_error")
    )
  }

  numeric_values
}

.enemdu_ipm_cutoff_flag <- function(score, cutoff) {
  out <- rep(NA_integer_, length(score))
  observed <- !is.na(score)

  out[observed] <- as.integer(
    score[observed] > cutoff |
      abs(score[observed] - cutoff) <= .enemdu_ipm_tolerance()
  )

  out
}

.enemdu_assert_ipm_output_names <- function(score_var, tpm_var, tpem_var) {
  output_vars <- c(score_var, tpm_var, tpem_var)

  if (
    length(output_vars) != 3 ||
      any(is.na(output_vars)) ||
      any(!nzchar(output_vars)) ||
      anyDuplicated(output_vars) > 0
  ) {
    rlang::abort(
      message = "`score_var`, `tpm_var`, and `tpem_var` must be unique non-empty variable names.",
      class = c("enemdu_error_invalid_ipm_output_names", "enemdu_error")
    )
  }

  invisible(TRUE)
}

.enemdu_abort_invalid_ipm_component_registry <- function(message) {
  .enemdu_abort_invalid_registry(
    registry_name = "ipm_component_registry",
    message = message,
    caller = ".enemdu_resolve_ipm_components"
  )
}

.enemdu_ipm_tolerance <- function() {
  1e-12
}
