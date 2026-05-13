#' Diagnose a data set for ENEMDU workflow readiness
#'
#' Produces a compact structured diagnostic summary useful in early pipeline
#' checks. The diagnostic includes survey type, design-variable availability,
#' and representativity scope when the survey type is known.
#'
#' @param data A data frame.
#'
#' @return A structured list with metadata and design-variable availability.
#' @export
enemdu_diagnose_data <- function(data) {
  if (!is.data.frame(data)) {
    .enemdu_abort_invalid_data(caller = "enemdu_diagnose_data")
  }

  design_vars <- .enemdu_default_design_vars()
  present <- design_vars %in% names(data)

  survey_type <- attr(data, "survey_type") %||% NA_character_
  period <- attr(data, "period") %||% NA_character_

  representativity_scope <- NULL
  if (!is.na(survey_type)) {
    representativity_scope <- enemdu_representativity_scope(
      survey_type = survey_type,
      emit = FALSE
    )
  }

  diagnostic <- list(
    metadata = tibble::tibble(
      n_rows = nrow(data),
      n_cols = ncol(data),
      survey_type = survey_type,
      period = period,
      input_format = attr(data, "input_format") %||% NA_character_
    ),
    design_variables = tibble::tibble(
      variable = design_vars,
      present = present
    ),
    representativity_scope = representativity_scope,
    names_preview = utils::head(names(data), 25),
    classes_preview = utils::head(vapply(data, function(x) class(x)[1], character(1)), 25)
  )

  class(diagnostic) <- c("enemdu_diagnosis", "list")
  diagnostic
}
