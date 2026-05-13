#' Read ENEMDU microdata from a Stata file
#'
#' Reads a `.dta` file and returns an object prepared for the `enemduR` workflow.
#' The function also stores the ENEMDU survey type and emits a representativity
#' scope message according to the loaded base: monthly, quarterly, or annual.
#'
#' @param path Path to the `.dta` file.
#' @param survey_type One of `"mensual"`, `"trimestral"` or `"anual"`.
#' @param period Optional period identifier used for comparability warnings.
#' Typical values are `"2020-09"`, `"2021-05"` or `"2018"`.
#' @param standardize_names Logical. If `TRUE`, variable names are normalized to
#' lower snake case.
#' @param inform_scope Logical. If `TRUE`, emits a message describing the
#' representativity scope of the loaded survey type.
#' @param encoding Optional file encoding passed to `haven::read_dta()`.
#' @param ... Additional arguments passed to `haven::read_dta()`.
#'
#' @return A data frame with class `enemdu_tbl` and basic ENEMDU attributes.
#' @export
enemdu_read_data <- function(path,
                             survey_type,
                             period = NULL,
                             standardize_names = TRUE,
                             inform_scope = TRUE,
                             encoding = NULL,
                             ...) {
  if (missing(path) || is.null(path) || !nzchar(path)) {
    .enemdu_abort_missing_argument("path", caller = "enemdu_read_data")
  }

  if (!file.exists(path)) {
    rlang::abort(
      message = glue::glue("File does not exist: `{path}`."),
      class = c("enemdu_error_missing_file", "enemdu_error")
    )
  }

  extension <- tolower(tools::file_ext(path))
  if (!identical(extension, "dta")) {
    rlang::abort(
      message = glue::glue(
        "Unsupported file format `{extension}`. ",
        "In this phase `enemdu_read_data()` only supports `.dta` files."
      ),
      class = c("enemdu_error_invalid_file_format", "enemdu_error")
    )
  }

  survey_type <- .enemdu_normalize_survey_type(
    survey_type,
    caller = "enemdu_read_data"
  )

  data <- haven::read_dta(file = path, encoding = encoding, ...)

  if (isTRUE(standardize_names)) {
    data <- enemdu_standardize_names(data)
  }

  attrs_to_set <- list(
    survey_type = survey_type,
    period = period,
    source_path = normalizePath(path, winslash = "/", mustWork = FALSE),
    input_format = "dta",
    design_vars = .enemdu_default_design_vars()
  )

  for (nm in names(attrs_to_set)) {
    attr(data, nm) <- attrs_to_set[[nm]]
  }

  class(data) <- unique(c("enemdu_tbl", class(data)))

  if (isTRUE(inform_scope)) {
    scope <- enemdu_representativity_scope(
      survey_type = survey_type,
      emit = TRUE
    )
    attr(data, "representativity_scope") <- scope
  }

  notes <- .enemdu_find_comparability_notes(
    period = period,
    survey_type = survey_type
  )

  if (length(notes) > 0) {
    for (note in notes) {
      .enemdu_warn_comparability(note, caller = "enemdu_read_data")
    }
  }

  data
}
