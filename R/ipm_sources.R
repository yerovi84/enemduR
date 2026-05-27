#' Join IPM person and household source files
#'
#' Appends household or housing variables from a household-level source to a
#' person-level ENEMDU source using a controlled household-key match. This
#' function only joins source variables. It does not derive IPM components, does
#' not compute `ipm_score`, `tpm`, `tpem`, `A`, or `ipm`, and does not validate
#' official IPM reproducibility.
#'
#' The output is intended to prepare person-level data for a future
#' `enemdu_build_ipm_components()` implementation.
#'
#' @param data Person-level ENEMDU data.
#' @param household_data Household- or housing-level ENEMDU data.
#' @param household_id Household identifier used in both sources.
#' @param housing_vars Household or housing variables to append from
#'   `household_data`. If `NULL`, the default IPM source variable set is used.
#' @param overwrite Logical. If `TRUE`, replace existing `housing_vars` in
#'   `data`.
#' @param strict Logical. If `TRUE`, abort when a non-missing household in
#'   `data` has no match in `household_data`. If `FALSE`, unmatched person rows
#'   are preserved and receive `NA` housing values.
#'
#' @return A data frame with appended household or housing variables and an
#' `ipm_source_join_diagnostics` attribute.
#' @export
#'
#' @examples
#' person <- tibble::tibble(
#'   id_hogar = c("h1", "h1", "h2"),
#'   p01 = c(1, 2, 1),
#'   p03 = c(40, 8, 45)
#' )
#' household <- tibble::tibble(
#'   id_hogar = c("h1", "h2"),
#'   vi03a = c(1, 2),
#'   vi03b = c(1, 1),
#'   vi04a = c(7, 1),
#'   vi04b = c(1, 1),
#'   vi05a = c(1, 6),
#'   vi05b = c(1, 1),
#'   vi07 = c(0, 1),
#'   vi09 = c(5, 1),
#'   vi10 = c(1, 2),
#'   vi13 = c(2, 1)
#' )
#'
#' joined <- enemdu_join_ipm_sources(person, household)
#' joined$vi03a
enemdu_join_ipm_sources <- function(data,
                                    household_data,
                                    household_id = "id_hogar",
                                    housing_vars = NULL,
                                    overwrite = FALSE,
                                    strict = TRUE) {
  if (is.null(housing_vars)) {
    housing_vars <- .enemdu_ipm_default_housing_vars()
  }

  resolved_housing_vars <- as.character(housing_vars)

  out <- enemdu_join_nbi_sources(
    person_data = data,
    household_data = household_data,
    household_id = household_id,
    housing_vars = resolved_housing_vars,
    overwrite = overwrite,
    strict = strict
  )

  nbi_diagnostics <- attr(out, "nbi_source_join_diagnostics")

  diagnostics <- list(
    person_rows_before = nbi_diagnostics$person_rows_before,
    person_rows_after = nbi_diagnostics$person_rows_after,
    household_rows = nbi_diagnostics$household_rows,
    unique_person_households = nbi_diagnostics$unique_person_households,
    unique_household_rows = nbi_diagnostics$unique_household_rows,
    unmatched_person_households = nbi_diagnostics$unmatched_person_households,
    joined_housing_vars = resolved_housing_vars,
    household_id = as.character(household_id),
    strict = isTRUE(strict),
    overwrite = isTRUE(overwrite),
    source_join = "enemdu_join_nbi_sources_wrapper"
  )

  attr(out, "ipm_source_join_diagnostics") <- diagnostics
  out
}

.enemdu_ipm_default_housing_vars <- function() {
  c(
    "vi03a",
    "vi03b",
    "vi04a",
    "vi04b",
    "vi05a",
    "vi05b",
    "vi07",
    "vi09",
    "vi10",
    "vi13"
  )
}
