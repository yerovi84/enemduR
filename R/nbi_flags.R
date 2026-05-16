#' Build NBI flags from final NBI components
#'
#' Builds `knbi`, `nbi`, and `xnbi` from final NBI component variables that
#' already exist in the data. This function does not reconstruct NBI from raw
#' questionnaire variables.
#'
#' @param data A data frame.
#' @param component_vars Final NBI component variables. Defaults to
#' `comp1` through `comp5`.
#' @param nbi_count_var Output variable for the count of observed NBI
#' deprivations.
#' @param nbi_var Output NBI poverty flag.
#' @param extreme_nbi_var Output extreme NBI poverty flag.
#' @param overwrite Logical. If `TRUE`, overwrite existing output variables.
#' @param strict_binary Logical. If `TRUE`, require all non-missing component
#' values to be binary `0`/`1`.
#'
#' @return A data frame with NBI flags and an `nbi_flag_policy` attribute.
#' @export
enemdu_build_nbi_flags <- function(data,
                                   component_vars = c("comp1", "comp2", "comp3", "comp4", "comp5"),
                                   nbi_count_var = "knbi",
                                   nbi_var = "nbi",
                                   extreme_nbi_var = "xnbi",
                                   overwrite = FALSE,
                                   strict_binary = TRUE) {
  if (!is.data.frame(data)) {
    .enemdu_abort_invalid_data(caller = "enemdu_build_nbi_flags")
  }

  component_vars <- as.character(component_vars)

  .enemdu_abort_missing_vars(
    vars = component_vars,
    names_data = names(data),
    caller = "enemdu_build_nbi_flags"
  )

  output_vars <- c(nbi_count_var, nbi_var, extreme_nbi_var)
  existing_outputs <- intersect(output_vars, names(data))

  if (length(existing_outputs) > 0 && !isTRUE(overwrite)) {
    rlang::abort(
      message = glue::glue(
        "NBI output variables already exist: {paste(existing_outputs, collapse = ', ')}. ",
        "Use `overwrite = TRUE` to replace them."
      ),
      class = c("enemdu_error_existing_nbi_output", "enemdu_error")
    )
  }

  component_data <- lapply(component_vars, function(var) {
    .enemdu_coerce_nbi_component(
      values = data[[var]],
      var = var,
      strict_binary = strict_binary
    )
  })
  component_data <- as.data.frame(component_data, optional = TRUE)
  names(component_data) <- component_vars

  complete_components <- stats::complete.cases(component_data)
  knbi <- rowSums(component_data, na.rm = FALSE)
  knbi[!complete_components] <- NA_real_

  nbi <- rep(NA_integer_, length(knbi))
  nbi[!is.na(knbi)] <- as.integer(knbi[!is.na(knbi)] >= 1)

  xnbi <- rep(NA_integer_, length(knbi))
  xnbi[!is.na(knbi)] <- as.integer(knbi[!is.na(knbi)] >= 2)

  out <- data
  out[[nbi_count_var]] <- as.integer(knbi)
  out[[nbi_var]] <- nbi
  out[[extreme_nbi_var]] <- xnbi

  attr(out, "nbi_component_contract") <- list(
    component_vars = component_vars,
    note = paste(
      "NBI flags are derived from final upstream NBI component variables only.",
      "No raw questionnaire reconstruction is performed."
    )
  )

  attr(out, "nbi_flag_policy") <- list(
    component_vars = component_vars,
    nbi_count_var = nbi_count_var,
    nbi_var = nbi_var,
    extreme_nbi_var = extreme_nbi_var,
    missing_component_rule = "Rows with any missing component are not evaluated.",
    zero_count_rule = "knbi = 0 is valid and remains classified as nbi = 0 and xnbi = 0.",
    note = paste(
      "nbi is 1 when knbi >= 1.",
      "xnbi is 1 when knbi >= 2."
    )
  )

  class(out) <- unique(c("enemdu_tbl", class(out)))
  out
}

#' Validate consistency of existing NBI variables
#'
#' Checks whether `nbi` and `xnbi` are consistent with the observed `knbi`
#' deprivation count.
#'
#' @param data A data frame.
#' @param nbi_count_var NBI deprivation-count variable.
#' @param nbi_var NBI poverty flag.
#' @param extreme_nbi_var Extreme NBI poverty flag.
#'
#' @return A one-row tibble with consistency counts and validation status.
#' @export
enemdu_validate_nbi_consistency <- function(data,
                                            nbi_count_var = "knbi",
                                            nbi_var = "nbi",
                                            extreme_nbi_var = "xnbi") {
  if (!is.data.frame(data)) {
    .enemdu_abort_invalid_data(caller = "enemdu_validate_nbi_consistency")
  }

  .enemdu_abort_missing_vars(
    vars = c(nbi_count_var, nbi_var, extreme_nbi_var),
    names_data = names(data),
    caller = "enemdu_validate_nbi_consistency"
  )

  knbi <- suppressWarnings(as.numeric(data[[nbi_count_var]]))
  nbi <- suppressWarnings(as.numeric(data[[nbi_var]]))
  xnbi <- suppressWarnings(as.numeric(data[[extreme_nbi_var]]))

  valid_knbi <- !is.na(knbi)

  inconsistencies_nbi <- sum(
    valid_knbi &
      (
        (knbi >= 1 & (is.na(nbi) | nbi != 1)) |
          (knbi < 1 & (is.na(nbi) | nbi != 0))
      ),
    na.rm = TRUE
  )

  inconsistencies_xnbi <- sum(
    valid_knbi &
      (
        (knbi >= 2 & (is.na(xnbi) | xnbi != 1)) |
          (knbi < 2 & (is.na(xnbi) | xnbi != 0))
      ),
    na.rm = TRUE
  )

  out <- tibble::tibble(
    n = nrow(data),
    n_knbi_na = sum(is.na(knbi)),
    n_nbi_na = sum(is.na(nbi)),
    n_xnbi_na = sum(is.na(xnbi)),
    inconsistencias_nbi = as.integer(inconsistencies_nbi),
    inconsistencias_xnbi = as.integer(inconsistencies_xnbi),
    min_knbi = if (all(is.na(knbi))) NA_real_ else min(knbi, na.rm = TRUE),
    max_knbi = if (all(is.na(knbi))) NA_real_ else max(knbi, na.rm = TRUE),
    validation_status = if (
      inconsistencies_nbi == 0 && inconsistencies_xnbi == 0
    ) {
      "passed"
    } else {
      "failed"
    }
  )

  class(out) <- unique(c("enemdu_nbi_consistency_validation", class(out)))
  out
}

.enemdu_coerce_nbi_component <- function(values,
                                         var,
                                         strict_binary) {
  missing <- is.na(values)

  if (is.factor(values)) {
    raw <- as.character(values)
  } else if (is.logical(values)) {
    raw <- as.integer(values)
  } else if (is.numeric(values)) {
    raw <- values
  } else if (is.character(values)) {
    raw <- trimws(values)
  } else {
    raw <- as.character(values)
  }

  numeric_values <- suppressWarnings(as.numeric(raw))
  numeric_values[missing] <- NA_real_

  invalid_conversion <- !missing & is.na(numeric_values)
  invalid_binary <- !missing & !is.na(numeric_values) & !(numeric_values %in% c(0, 1))

  if (isTRUE(strict_binary) && any(invalid_conversion | invalid_binary)) {
    rlang::abort(
      message = glue::glue(
        "NBI component variable `{var}` must contain binary 0/1 values."
      ),
      class = c("enemdu_error_invalid_nbi_component", "enemdu_error")
    )
  }

  numeric_values
}
