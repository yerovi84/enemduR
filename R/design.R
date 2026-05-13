#' Declare an ENEMDU complex survey design
#'
#' Declares the ENEMDU complex survey design using the canonical design
#' variables: primary sampling unit, strata, and expansion factor.
#'
#' @param data A data frame.
#' @param ids Name of the primary sampling unit variable. Defaults to `"upm"`.
#' @param strata Name of the strata variable. Defaults to `"estrato"`.
#' @param weights Name of the expansion factor variable. Defaults to `"fexp"`.
#' @param nest Logical passed to `survey::svydesign()`.
#' @param lonely_psu Option used by the survey package for lonely PSUs.
#'
#' @return A `survey.design` object.
#' @export
enemdu_declare_design <- function(data,
                                  ids = "upm",
                                  strata = "estrato",
                                  weights = "fexp",
                                  nest = TRUE,
                                  lonely_psu = "adjust") {
  if (!is.data.frame(data)) {
    .enemdu_abort_invalid_data(caller = "enemdu_declare_design")
  }

  required <- c(ids, strata, weights)
  .enemdu_abort_missing_vars(
    vars = required,
    names_data = names(data),
    caller = "enemdu_declare_design"
  )

  old_lonely_psu <- getOption("survey.lonely.psu")
  options(survey.lonely.psu = lonely_psu)
  on.exit(options(survey.lonely.psu = old_lonely_psu), add = TRUE)

  design <- survey::svydesign(
    ids = stats::as.formula(paste0("~", ids)),
    strata = stats::as.formula(paste0("~", strata)),
    weights = stats::as.formula(paste0("~", weights)),
    data = data,
    nest = nest
  )

  attr(design, "enemdu_design_vars") <- list(
    ids = ids,
    strata = strata,
    weights = weights
  )

  attr(design, "survey_type") <- attr(data, "survey_type") %||% NA_character_
  attr(design, "period") <- attr(data, "period") %||% NA_character_

  design
}
