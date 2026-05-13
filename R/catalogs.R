#' Return the ENEMDU variable catalog
#'
#' Reads the package variable catalog. The catalog is intentionally metadata-based
#' so that variable contracts can be audited and extended without hiding rules
#' inside analytical functions.
#'
#' @return A tibble with the variable catalog.
#' @export
enemdu_variable_catalog <- function() {
  .enemdu_variable_catalog()
}

#' Return the ENEMDU indicator registry
#'
#' Reads the package indicator registry. The registry defines the analytical
#' contract for each indicator, including analysis level, estimator type,
#' weight, universe, and whether household-scale adjustment may be required.
#'
#' @return A tibble with the indicator registry.
#' @export
enemdu_indicator_registry <- function() {
  .enemdu_indicator_registry()
}

#' Return the ENEMDU validation registry
#'
#' Reads the package validation registry. Validation rules are used to make
#' structural and substantive checks explicit and auditable.
#'
#' @return A tibble with the validation registry.
#' @export
enemdu_validation_registry <- function() {
  .enemdu_validation_registry()
}

#' Return the income component registry
#'
#' Reads the income component registry used to formalize the official income
#' construction workflow before implementation.
#'
#' @return A tibble with income component rules.
#' @export
enemdu_income_component_registry <- function() {
  .enemdu_income_component_registry()
}

#' Return the optional bonus registry
#'
#' Reads the optional bonus registry. This registry defines bonus variables such
#' as `p78` that are valid for optional transfer analysis but are not included in
#' the base poverty-income construction by default.
#'
#' @return A tibble with optional bonus rules.
#' @export
enemdu_optional_bonus_registry <- function() {
  .enemdu_optional_bonus_registry()
}

#' Return the ENEMDU domain registry
#'
#' Reads the registry that defines which domain levels are design domains for
#' monthly, quarterly, and annual ENEMDU bases.
#'
#' @return A tibble with domain-scope rules.
#' @export
enemdu_domain_registry <- function() {
  .enemdu_domain_registry()
}

#' Return the domain-variable registry
#'
#' Reads the registry that maps common variable names to domain levels. This
#' registry is intentionally editable because variable names may vary across
#' analytical projects.
#'
#' @return A tibble with variable-to-domain mappings.
#' @export
enemdu_domain_variable_registry <- function() {
  .enemdu_domain_variable_registry()
}

#' Return required variables for an indicator
#'
#' Resolves the required variables declared in `indicator_registry.csv`.
#'
#' @param indicator_id Indicator identifier.
#'
#' @return A character vector with required variable names.
#' @export
enemdu_required_vars_for_indicator <- function(indicator_id) {
  if (missing(indicator_id) || is.null(indicator_id) || length(indicator_id) != 1) {
    .enemdu_abort_missing_argument(
      "indicator_id",
      caller = "enemdu_required_vars_for_indicator"
    )
  }

  registry <- .enemdu_indicator_registry()
  row <- registry[registry$indicator_id == indicator_id, , drop = FALSE]

  if (nrow(row) != 1) {
    rlang::abort(
      message = glue::glue(
        "Indicator `{indicator_id}` was not found uniquely in `indicator_registry.csv`."
      ),
      class = c("enemdu_error_invalid_indicator_id", "enemdu_error")
    )
  }

  raw_vars <- row$required_vars[[1]]

  if (is.na(raw_vars) || !nzchar(raw_vars)) {
    return(character(0))
  }

  trimws(unlist(strsplit(raw_vars, "\\|", fixed = FALSE)))
}
