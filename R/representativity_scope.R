#' Return the representativity scope of an ENEMDU survey type
#'
#' This function reports the official design-domain scope associated with the
#' ENEMDU survey type loaded into the workflow.
#'
#' @param survey_type One of `"mensual"`, `"trimestral"` or `"anual"`.
#' @param emit Logical. If `TRUE`, prints an informative message.
#'
#' @return A one-row tibble with the representativity scope.
#' @export
enemdu_representativity_scope <- function(survey_type, emit = TRUE) {
  survey_type <- .enemdu_normalize_survey_type(
    survey_type,
    caller = "enemdu_representativity_scope"
  )

  row <- .enemdu_get_survey_registry_row(survey_type)

  out <- tibble::tibble(
    survey_type = row$survey_type,
    description = row$description,
    design_domains = row$design_domains,
    representativity_message = row$representativity_message,
    default_weight = row$default_weight,
    default_psu = row$default_psu,
    default_strata = row$default_strata
  )

  if (isTRUE(emit)) {
    .enemdu_inform_representativity_scope(
      message = out$representativity_message,
      caller = "enemdu_representativity_scope"
    )
  }

  class(out) <- unique(c("enemdu_representativity_scope", class(out)))
  out
}
