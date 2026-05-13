#' Evaluate precision and representativity decision rules
#'
#' Applies the phase-2 precision classification contract based on ENEMDU
#' methodological criteria. This function evaluates metrics already computed
#' elsewhere. Full indicator estimation is implemented in later phases.
#'
#' @param estimate Numeric estimate.
#' @param standard_error Numeric standard error.
#' @param cv Optional coefficient of variation. If omitted and possible, it is
#' computed as `standard_error / abs(estimate)`.
#' @param n Optional raw sample size.
#' @param effective_n Optional effective sample size.
#' @param deff Optional design effect. If `effective_n` is missing and both `n`
#' and `deff` are available, `effective_n = n / deff`.
#' @param degrees_freedom Degrees of freedom.
#' @param estimator_type One of `"proportion_0_1"`, `"ratio_0_1"`,
#' `"mean"`, `"total"` or `"other"`.
#'
#' @return A one-row tibble with the precision decision.
#' @export
enemdu_evaluate_precision <- function(estimate,
                                      standard_error,
                                      cv = NULL,
                                      n = NULL,
                                      effective_n = NULL,
                                      deff = NULL,
                                      degrees_freedom,
                                      estimator_type = c(
                                        "proportion_0_1",
                                        "ratio_0_1",
                                        "mean",
                                        "total",
                                        "other"
                                      )) {
  estimator_type <- match.arg(estimator_type)

  if (missing(estimate) || length(estimate) != 1 || is.na(estimate)) {
    .enemdu_abort_missing_argument("estimate", caller = "enemdu_evaluate_precision")
  }

  if (missing(standard_error) || length(standard_error) != 1 || is.na(standard_error)) {
    .enemdu_abort_missing_argument("standard_error", caller = "enemdu_evaluate_precision")
  }

  if (missing(degrees_freedom) || length(degrees_freedom) != 1 || is.na(degrees_freedom)) {
    .enemdu_abort_missing_argument("degrees_freedom", caller = "enemdu_evaluate_precision")
  }

  if (is.null(effective_n) && !is.null(n) && !is.null(deff) && !is.na(deff) && deff > 0) {
    effective_n <- n / deff
  }

  if (is.null(effective_n) && !is.null(n)) {
    effective_n <- n
  }

  if (is.null(effective_n) || is.na(effective_n)) {
    .enemdu_abort_missing_argument("effective_n", caller = "enemdu_evaluate_precision")
  }

  if (is.null(cv)) {
    if (isTRUE(abs(estimate) > 0)) {
      cv <- standard_error / abs(estimate)
    } else {
      cv <- NA_real_
    }
  }

  min_effective_n <- .enemdu_get_threshold("min_effective_n")$value
  min_degrees_freedom <- .enemdu_get_threshold("min_degrees_freedom")$value
  cv_reliable_max <- .enemdu_get_threshold("cv_reliable_max")$value
  cv_reduced_max <- .enemdu_get_threshold("cv_reduced_max")$value

  failed_reasons <- character(0)

  if (effective_n < min_effective_n) {
    failed_reasons <- c(
      failed_reasons,
      glue::glue("effective_n < {min_effective_n}")
    )
  }

  if (degrees_freedom < min_degrees_freedom) {
    failed_reasons <- c(
      failed_reasons,
      glue::glue("degrees_freedom < {min_degrees_freedom}")
    )
  }

  is_unit_interval_estimator <- estimator_type %in% c("proportion_0_1", "ratio_0_1")

  if (length(failed_reasons) > 0) {
    decision <- "no_recommended_inference"
  } else if (is_unit_interval_estimator) {
    max_tolerable_se <- .enemdu_max_tolerable_se_unit_interval(estimate)

    if (standard_error <= max_tolerable_se) {
      decision <- "reliable"
    } else {
      decision <- "reduced_precision"
    }
  } else {
    if (is.na(cv)) {
      decision <- "no_recommended_inference"
      failed_reasons <- c(failed_reasons, "cv_not_computable")
    } else if (cv <= cv_reliable_max) {
      decision <- "reliable"
    } else if (cv <= cv_reduced_max) {
      decision <- "reduced_precision"
    } else {
      decision <- "no_recommended_inference"
      failed_reasons <- c(
        failed_reasons,
        glue::glue("cv > {cv_reduced_max}")
      )
    }
  }

  out <- tibble::tibble(
    estimate = estimate,
    standard_error = standard_error,
    cv = cv,
    n = n %||% NA_real_,
    effective_n = effective_n,
    deff = deff %||% NA_real_,
    degrees_freedom = degrees_freedom,
    estimator_type = estimator_type,
    decision = decision,
    failed_reasons = paste(failed_reasons, collapse = "; ")
  )

  class(out) <- unique(c("enemdu_precision_decision", class(out)))
  out
}

.enemdu_max_tolerable_se_unit_interval <- function(p) {
  if (is.na(p) || p < 0 || p > 1) {
    return(NA_real_)
  }

  if (p < 0.5) {
    sqrt((p^(2 / 3)) / 9)
  } else {
    sqrt(((1 - p)^(2 / 3)) / 9)
  }
}

#' Check representativity for a precomputed estimate
#'
#' Phase-2 contract function. It evaluates precision metrics already computed by
#' an estimation layer and attaches survey-type representativity scope metadata.
#'
#' @param estimate Numeric estimate.
#' @param standard_error Numeric standard error.
#' @param degrees_freedom Degrees of freedom.
#' @param survey_type ENEMDU survey type.
#' @param estimator_type Estimator type.
#' @param n Optional raw sample size.
#' @param effective_n Optional effective sample size.
#' @param deff Optional design effect.
#' @param cv Optional coefficient of variation.
#'
#' @return A one-row tibble with representativity and precision metadata.
#' @export
enemdu_check_representativity <- function(estimate,
                                          standard_error,
                                          degrees_freedom,
                                          survey_type,
                                          estimator_type = c(
                                            "proportion_0_1",
                                            "ratio_0_1",
                                            "mean",
                                            "total",
                                            "other"
                                          ),
                                          n = NULL,
                                          effective_n = NULL,
                                          deff = NULL,
                                          cv = NULL) {
  survey_type <- .enemdu_normalize_survey_type(
    survey_type,
    caller = "enemdu_check_representativity"
  )

  scope <- enemdu_representativity_scope(
    survey_type = survey_type,
    emit = FALSE
  )

  decision <- enemdu_evaluate_precision(
    estimate = estimate,
    standard_error = standard_error,
    cv = cv,
    n = n,
    effective_n = effective_n,
    deff = deff,
    degrees_freedom = degrees_freedom,
    estimator_type = estimator_type
  )

  out <- cbind(
    tibble::tibble(
      survey_type = survey_type,
      design_domains = scope$design_domains
    ),
    decision
  )

  class(out) <- unique(c("enemdu_representativity_report", class(out)))
  out
}

#' Produce a representativity report
#'
#' Phase-2 alias for representativity checks over precomputed metrics.
#'
#' @param ... Arguments passed to `enemdu_check_representativity()`.
#'
#' @return A representativity report.
#' @export
enemdu_representativity_report <- function(...) {
  enemdu_check_representativity(...)
}
