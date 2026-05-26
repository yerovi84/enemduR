#' Join NBI person and housing source files
#'
#' Appends housing variables from a household-level source to a person-level
#' ENEMDU source using a controlled household-key match. The function preserves
#' the number and order of person rows. It only joins sources; it does not derive
#' NBI components and does not validate official estimates.
#'
#' @param person_data Person-level ENEMDU data.
#' @param household_data Household- or housing-level ENEMDU data.
#' @param household_id Household identifier used in both sources.
#' @param housing_vars Housing variables to append from `household_data`.
#' @param overwrite Logical. If `TRUE`, replace existing `housing_vars` in
#' `person_data`.
#' @param strict Logical. If `TRUE`, abort when a non-missing household in
#' `person_data` has no match in `household_data`. If `FALSE`, unmatched person
#' rows are preserved and receive `NA` housing values.
#'
#' @return A data frame with appended housing variables and an
#' `nbi_source_join_diagnostics` attribute.
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
#'   vi04a = c(7, 1),
#'   vi05a = c(1, 6),
#'   vi07 = c(0, 1),
#'   vi09 = c(5, 1),
#'   vi10 = c(1, 2),
#'   vi10a = c(1, 3)
#' )
#'
#' joined <- enemdu_join_nbi_sources(person, household)
#' joined$vi04a
enemdu_join_nbi_sources <- function(person_data,
                                    household_data,
                                    household_id = "id_hogar",
                                    housing_vars = c("vi04a", "vi05a", "vi07", "vi09", "vi10", "vi10a"),
                                    overwrite = FALSE,
                                    strict = TRUE) {
  if (!is.data.frame(person_data)) {
    rlang::abort(
      message = "`person_data` must be a data frame.",
      class = c("enemdu_error_invalid_nbi_source_data", "enemdu_error")
    )
  }

  if (!is.data.frame(household_data)) {
    rlang::abort(
      message = "`household_data` must be a data frame.",
      class = c("enemdu_error_invalid_nbi_source_data", "enemdu_error")
    )
  }

  household_id <- as.character(household_id)
  housing_vars <- as.character(housing_vars)

  if (
    length(household_id) != 1 ||
      is.na(household_id) ||
      !nzchar(household_id)
  ) {
    rlang::abort(
      message = "`household_id` must be a single non-empty variable name.",
      class = c("enemdu_error_missing_nbi_source_key", "enemdu_error")
    )
  }

  if (!household_id %in% names(person_data)) {
    rlang::abort(
      message = glue::glue(
        "Household key `{household_id}` is missing from `person_data`."
      ),
      class = c("enemdu_error_missing_nbi_source_key", "enemdu_error"),
      missing_vars = household_id
    )
  }

  if (!household_id %in% names(household_data)) {
    rlang::abort(
      message = glue::glue(
        "Household key `{household_id}` is missing from `household_data`."
      ),
      class = c("enemdu_error_missing_nbi_source_key", "enemdu_error"),
      missing_vars = household_id
    )
  }

  if (
    length(housing_vars) == 0 ||
      any(is.na(housing_vars)) ||
      any(!nzchar(housing_vars))
  ) {
    rlang::abort(
      message = "`housing_vars` must contain valid variable names.",
      class = c("enemdu_error_missing_nbi_housing_vars", "enemdu_error")
    )
  }

  missing_housing_vars <- setdiff(housing_vars, names(household_data))

  if (length(missing_housing_vars) > 0) {
    rlang::abort(
      message = glue::glue(
        "Housing variables are missing from `household_data`: ",
        "{paste(missing_housing_vars, collapse = ', ')}."
      ),
      class = c("enemdu_error_missing_nbi_housing_vars", "enemdu_error"),
      missing_vars = missing_housing_vars
    )
  }

  existing_housing_vars <- intersect(housing_vars, names(person_data))

  if (length(existing_housing_vars) > 0 && !isTRUE(overwrite)) {
    rlang::abort(
      message = glue::glue(
        "Housing variables already exist in `person_data`: ",
        "{paste(existing_housing_vars, collapse = ', ')}. ",
        "Use `overwrite = TRUE` to replace them."
      ),
      class = c("enemdu_error_existing_nbi_housing_vars", "enemdu_error"),
      existing_vars = existing_housing_vars
    )
  }

  household_keys <- household_data[[household_id]]
  household_keys_non_missing <- household_keys[!is.na(household_keys)]
  duplicate_household_keys <- unique(household_keys_non_missing[
    duplicated(household_keys_non_missing)
  ])

  if (length(duplicate_household_keys) > 0) {
    rlang::abort(
      message = glue::glue(
        "Duplicate household keys found in `household_data`: ",
        "{paste(duplicate_household_keys, collapse = ', ')}."
      ),
      class = c("enemdu_error_duplicate_nbi_household_ids", "enemdu_error"),
      duplicate_household_ids = duplicate_household_keys
    )
  }

  person_keys <- person_data[[household_id]]
  row_index <- match(person_keys, household_keys)
  row_index[is.na(person_keys)] <- NA_integer_

  unmatched_person_households <- unique(person_keys[
    !is.na(person_keys) & is.na(row_index)
  ])

  if (isTRUE(strict) && length(unmatched_person_households) > 0) {
    rlang::abort(
      message = glue::glue(
        "Some person-level households have no match in `household_data`: ",
        "{paste(unmatched_person_households, collapse = ', ')}."
      ),
      class = c("enemdu_error_unmatched_nbi_households", "enemdu_error"),
      unmatched_household_ids = unmatched_person_households
    )
  }

  out <- person_data

  for (var in housing_vars) {
    out[[var]] <- household_data[[var]][row_index]
  }

  diagnostics <- list(
    person_rows_before = nrow(person_data),
    person_rows_after = nrow(out),
    household_rows = nrow(household_data),
    unique_person_households = length(unique(person_keys[!is.na(person_keys)])),
    unique_household_rows = length(unique(household_keys_non_missing)),
    unmatched_person_households = as.character(unmatched_person_households),
    housing_vars = housing_vars,
    household_id = household_id,
    strict = isTRUE(strict),
    overwrite = isTRUE(overwrite)
  )

  attr(out, "nbi_source_join_diagnostics") <- diagnostics
  class(out) <- unique(c("enemdu_tbl", class(out)))
  out
}
