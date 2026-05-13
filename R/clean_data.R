#' Apply phase-1 minimal cleaning to ENEMDU data
#'
#' This function performs only structural cleaning steps suitable for the current
#' implementation phase. It does not yet apply substantive harmonization rules.
#'
#' @param data A data frame previously read by `enemdu_read_data()` or another
#' data frame compatible with the ENEMDU workflow.
#' @param survey_type Optional survey type. If omitted, the function uses the
#' `survey_type` attribute when available.
#' @param standardize_names Logical. If `TRUE`, variable names are standardized.
#'
#' @return A minimally cleaned data frame.
#' @export
enemdu_clean_data <- function(data,
                              survey_type = NULL,
                              standardize_names = TRUE) {
  if (!is.data.frame(data)) {
    rlang::abort(
      message = "`data` must be a data frame in `enemdu_clean_data()`.",
      class = c("enemdu_error_invalid_data", "enemdu_error")
    )
  }

  if (is.null(survey_type)) {
    survey_type <- attr(data, "survey_type")
  }

  if (!is.null(survey_type)) {
    survey_type <- .enemdu_normalize_survey_type(survey_type)
  }

  out <- data

  if (isTRUE(standardize_names)) {
    out <- enemdu_standardize_names(out)
  }

  attr(out, "survey_type") <- survey_type
  attr(out, "clean_stage") <- "phase_1_minimal"

  class(out) <- unique(c("enemdu_tbl", class(out)))

  out
}
