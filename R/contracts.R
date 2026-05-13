#' Return a standard output contract
#'
#' Provides stable column contracts for core output types. These contracts are
#' used to keep all public analytical outputs consistent across modules.
#'
#' @param type One of `"kpi"`, `"validation"`, `"representativity"`,
#' `"tabulation"` or `"diagnosis"`.
#'
#' @return A tibble describing the expected output columns.
#' @export
enemdu_output_contract <- function(type = c(
  "kpi",
  "validation",
  "representativity",
  "tabulation",
  "diagnosis"
)) {
  type <- match.arg(type)

  contracts <- list(
    kpi = tibble::tibble(
      column = c(
        "indicator_id",
        "indicator_label",
        "unit",
        "analysis_unit",
        "universe",
        "domain_type",
        "domain_value",
        "estimate",
        "weighted_n",
        "unweighted_n",
        "standard_error",
        "cv",
        "quality_flag",
        "warning_flag",
        "source_note"
      ),
      required = c(
        TRUE,
        TRUE,
        TRUE,
        TRUE,
        TRUE,
        FALSE,
        FALSE,
        TRUE,
        FALSE,
        FALSE,
        FALSE,
        FALSE,
        TRUE,
        FALSE,
        FALSE
      ),
      description = c(
        "Stable indicator identifier.",
        "Human-readable indicator label.",
        "Measurement unit.",
        "Analytical unit of the estimate.",
        "Universe used for the indicator.",
        "Domain level used for disaggregation.",
        "Domain value used for disaggregation.",
        "Main estimated value.",
        "Weighted population size when available.",
        "Unweighted sample size when available.",
        "Standard error when available.",
        "Coefficient of variation when available.",
        "Quality flag assigned to the estimate.",
        "Warning flag for relevant methodological cautions.",
        "Source or methodological note."
      )
    ),
    validation = tibble::tibble(
      column = c(
        "check_id",
        "check_type",
        "severity",
        "status",
        "variable",
        "message",
        "n_affected",
        "details"
      ),
      required = c(TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, FALSE, FALSE),
      description = c(
        "Stable validation rule identifier.",
        "Validation type.",
        "Severity level.",
        "Validation status.",
        "Variable affected by the validation.",
        "Human-readable validation message.",
        "Number of affected records if available.",
        "Additional details."
      )
    ),
    representativity = tibble::tibble(
      column = c(
        "indicator_id",
        "survey_type",
        "domain_type",
        "domain_value",
        "analysis_level",
        "estimator_type",
        "household_scale_adjustment",
        "estimate",
        "standard_error",
        "cv",
        "n",
        "effective_n",
        "deff",
        "degrees_freedom",
        "ci_lower",
        "ci_upper",
        "decision",
        "failed_reasons",
        "method_note"
      ),
      required = c(
        FALSE,
        TRUE,
        FALSE,
        FALSE,
        FALSE,
        TRUE,
        FALSE,
        TRUE,
        TRUE,
        FALSE,
        FALSE,
        TRUE,
        FALSE,
        TRUE,
        FALSE,
        FALSE,
        TRUE,
        FALSE,
        FALSE
      ),
      description = c(
        "Stable indicator identifier.",
        "ENEMDU survey type.",
        "Domain level.",
        "Domain value.",
        "Analysis level.",
        "Estimator type used by the decision rule.",
        "Whether internal household scale adjustment was applied.",
        "Main estimate.",
        "Standard error.",
        "Coefficient of variation.",
        "Raw sample size.",
        "Effective sample size.",
        "Design effect.",
        "Degrees of freedom.",
        "Lower confidence interval bound.",
        "Upper confidence interval bound.",
        "Precision decision.",
        "Failed criteria if any.",
        "Methodological note."
      )
    ),
    tabulation = tibble::tibble(
      column = c(
        "group_var",
        "group_value",
        "measure",
        "estimate",
        "weighted_n",
        "unweighted_n",
        "quality_flag"
      ),
      required = c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE),
      description = c(
        "Grouping variable.",
        "Grouping value.",
        "Measure name.",
        "Estimated value.",
        "Weighted count when available.",
        "Unweighted count when available.",
        "Quality flag."
      )
    ),
    diagnosis = tibble::tibble(
      column = c(
        "component",
        "status",
        "message",
        "details"
      ),
      required = c(TRUE, TRUE, TRUE, FALSE),
      description = c(
        "Diagnostic component.",
        "Diagnostic status.",
        "Diagnostic message.",
        "Additional details."
      )
    )
  )

  out <- contracts[[type]]
  attr(out, "contract_type") <- type
  class(out) <- unique(c("enemdu_output_contract", class(out)))
  out
}
