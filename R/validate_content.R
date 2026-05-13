#' Validate ENEMDU content rules
#'
#' This function is declared in phase 1 but substantive content rules are not yet
#' implemented. The phase-1 version returns a standardized placeholder report.
#'
#' @param data A data frame.
#'
#' @return A tibble placeholder describing the current implementation status.
#' @export
enemdu_validate_content <- function(data) {
  if (!is.data.frame(data)) {
    rlang::abort(
      message = "`data` must be a data frame in `enemdu_validate_content()`.",
      class = c("enemdu_error_invalid_data", "enemdu_error")
    )
  }

  result <- tibble::tibble(
    check_id = "content_phase_1_placeholder",
    check_type = "content",
    severity = "warning",
    status = "pending",
    variable = "",
    message = paste(
      "Content validation rules are not implemented yet.",
      "They will be formalized in phase 2 once variable contracts and",
      "survey-specific rule catalogs are closed."
    ),
    n_affected = NA_integer_,
    details = ""
  )

  class(result) <- unique(c("enemdu_validation_report", class(result)))
  result
}
