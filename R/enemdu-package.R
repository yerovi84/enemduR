#' enemduR: Analytical Infrastructure for ENEMDU
#'
#' `enemduR` provides a reusable analytical foundation for ENEMDU workflows.
#' The package is designed to support a phased implementation strategy:
#'
#' - reading and standardizing ENEMDU microdata,
#' - validating structural and substantive requirements,
#' - building derived variables and registry-backed indicators,
#' - declaring complex survey designs and estimating survey-aware outputs,
#' - producing labor indicators from the consolidated `condact` variable,
#' - comparing implemented labor indicators with official tabulations,
#' - and producing stable analytical outputs for downstream Quarto reporting.
#'
#' The package intentionally separates:
#'
#' - data and metadata handling,
#' - analytical logic,
#' - and presentation helpers.
#'
#' Presentation code is downstream of the analytical engine. Quarto-facing
#' helpers are intentionally lightweight and must not govern indicator
#' computation.
#'
#' @keywords internal
"_PACKAGE"
