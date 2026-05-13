#' Validate basic ENEMDU structural requirements
#'
#' Performs a phase-1 structural validation focused on object type, presence of
#' design variables, and presence of any user-declared required variables.
#'
#' @param data A data frame.
#' @param required_vars Optional character vector of required variables.
#' @param design_vars Character vector of survey design variables.
#'
#' @return A tibble with structural validation results.
#' @export
enemdu_validate_structure <- function(data,
                                      required_vars = NULL,
                                      design_vars = .enemdu_default_design_vars()) {
  is_df <- is.data.frame(data)
  data_names <- if (is_df) names(data) else character(0)

  missing_design_vars <- if (is_df) setdiff(design_vars, data_names) else design_vars
  missing_required_vars <- if (is_df && !is.null(required_vars)) {
    setdiff(required_vars, data_names)
  } else {
    character(0)
  }

  result <- tibble::tibble(
    check_id = c(
      "structure_is_data_frame",
      "structure_design_vars",
      "structure_required_vars"
    ),
    check_type = c("structure", "structure", "structure"),
    severity = c(
      if (is_df) "info" else "error",
      if (length(missing_design_vars) == 0) "info" else "error",
      if (length(missing_required_vars) == 0) "info" else "error"
    ),
    status = c(
      if (is_df) "pass" else "fail",
      if (length(missing_design_vars) == 0) "pass" else "fail",
      if (length(missing_required_vars) == 0) "pass" else "fail"
    ),
    variable = c(
      "data",
      paste(design_vars, collapse = ", "),
      if (is.null(required_vars)) "" else paste(required_vars, collapse = ", ")
    ),
    message = c(
      if (is_df) "Input is a data frame." else "Input is not a data frame.",
      if (length(missing_design_vars) == 0) {
        "All declared design variables are present."
      } else {
        glue::glue(
          "Missing design variables: {paste(missing_design_vars, collapse = ', ')}."
        )
      },
      if (is.null(required_vars)) {
        "No user-defined required variables were requested."
      } else if (length(missing_required_vars) == 0) {
        "All user-defined required variables are present."
      } else {
        glue::glue(
          "Missing required variables: {paste(missing_required_vars, collapse = ', ')}."
        )
      }
    ),
    n_affected = c(
      if (is_df) 0L else 1L,
      length(missing_design_vars),
      length(missing_required_vars)
    ),
    details = c(
      "",
      paste(missing_design_vars, collapse = ", "),
      paste(missing_required_vars, collapse = ", ")
    )
  )

  class(result) <- unique(c("enemdu_validation_report", class(result)))
  result
}
