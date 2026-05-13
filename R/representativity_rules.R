#' Evaluate precision and representativity decision rules
#'
#' Applies the phase-2 precision classification contract based on ENEMDU
#' methodological criteria. This function evaluates metrics already computed
#' elsewhere.
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

    if (!is.na(max_tolerable_se) && standard_error <= max_tolerable_se) {
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
#' Evaluates precision metrics already computed by an estimation layer and
#' attaches survey-type representativity scope metadata.
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

#' Build an integrated representativity report
#'
#' Produces a representativity report from an estimate table generated by
#' `enemdu_indicator_estimate()` or the `enemdu_survey_*()` functions. The report
#' combines:
#'
#' - survey type,
#' - design-domain scope,
#' - requested domain validation,
#' - precision decision,
#' - and a final representativity flag.
#'
#' For backward compatibility, when `estimate` is numeric, the function delegates
#' to `enemdu_check_representativity()`.
#'
#' @param estimate A data frame of estimates or a numeric estimate.
#' @param survey_type Optional survey type.
#' @param domain_level Optional domain level.
#' @param domain_var Optional domain variable.
#' @param group_vars Optional grouping variables.
#' @param strict_domain Logical. If `TRUE`, errors when the domain is outside
#' design scope.
#' @param ... Additional arguments passed to `enemdu_check_representativity()`
#' when `estimate` is numeric.
#'
#' @return A tibble with integrated representativity metadata.
#' @export
enemdu_representativity_report <- function(estimate,
                                           survey_type = NULL,
                                           domain_level = NULL,
                                           domain_var = NULL,
                                           group_vars = NULL,
                                           strict_domain = FALSE,
                                           ...) {
  if (!is.data.frame(estimate)) {
    return(enemdu_check_representativity(
      estimate = estimate,
      survey_type = survey_type,
      ...
    ))
  }

  if (nrow(estimate) == 0) {
    return(tibble::as_tibble(estimate))
  }

  survey_type <- .enemdu_resolve_report_survey_type(
    estimate = estimate,
    survey_type = survey_type
  )

  domain_scope <- enemdu_validate_domain_scope(
    survey_type = survey_type,
    domain_level = domain_level,
    domain_var = domain_var,
    group_vars = group_vars,
    strict = strict_domain,
    emit = FALSE
  )

  domain_is_design <- all(domain_scope$is_design_domain)
  domain_requires_precision <- any(domain_scope$requires_precision_evaluation)

  domain_scope_flag <- if (isTRUE(domain_is_design)) {
    "design_domain"
  } else {
    "analysis_domain_requires_precision"
  }

  domain_scope_message <- paste(domain_scope$message, collapse = " | ")

  out <- estimate

  out[["domain_scope_flag"]] <- domain_scope_flag
  out[["domain_is_design_domain"]] <- domain_is_design
  out[["domain_requires_precision_evaluation"]] <- domain_requires_precision
  out[["domain_scope_message"]] <- domain_scope_message

  if (!"decision" %in% names(out)) {
    out[["decision"]] <- NA_character_
  }

  out[["representativity_flag"]] <- vapply(
    out[["decision"]],
    function(decision) {
      .enemdu_final_representativity_flag(
        precision_decision = decision,
        is_design_domain = domain_is_design
      )
    },
    character(1)
  )

  out[["representativity_note"]] <- vapply(
    seq_len(nrow(out)),
    function(i) {
      .enemdu_final_representativity_note(
        precision_decision = out$decision[[i]],
        is_design_domain = domain_is_design,
        domain_scope_message = domain_scope_message
      )
    },
    character(1)
  )

  attr(out, "domain_scope_report") <- domain_scope
  attr(out, "representativity_report_policy") <- list(
    survey_type = survey_type,
    domain_level = domain_level,
    domain_var = domain_var,
    group_vars = group_vars,
    strict_domain = strict_domain,
    note = paste(
      "The report combines domain-scope validation and precision decision.",
      "A design domain does not remove the need for precision evaluation;",
      "an analysis domain must be reported with explicit caution."
    )
  )

  class(out) <- unique(c("enemdu_integrated_representativity_report", class(out)))
  out
}

.enemdu_resolve_report_survey_type <- function(estimate,
                                               survey_type = NULL) {
  if (!is.null(survey_type)) {
    return(.enemdu_normalize_survey_type(
      survey_type = survey_type,
      caller = ".enemdu_resolve_report_survey_type"
    ))
  }

  if ("survey_type" %in% names(estimate)) {
    values <- unique(as.character(estimate$survey_type))
    values <- values[!is.na(values) & nzchar(values)]

    if (length(values) >= 1) {
      return(.enemdu_normalize_survey_type(
        survey_type = values[[1]],
        caller = ".enemdu_resolve_report_survey_type"
      ))
    }
  }

  attr_survey_type <- attr(estimate, "survey_type")

  if (!is.null(attr_survey_type)) {
    return(.enemdu_normalize_survey_type(
      survey_type = attr_survey_type,
      caller = ".enemdu_resolve_report_survey_type"
    ))
  }

  .enemdu_abort_missing_argument(
    "survey_type",
    caller = "enemdu_representativity_report"
  )
}

.enemdu_final_representativity_flag <- function(precision_decision,
                                                is_design_domain) {
  if (is.na(precision_decision) || !nzchar(precision_decision)) {
    return("precision_not_available")
  }

  if (isTRUE(is_design_domain) && identical(precision_decision, "reliable")) {
    return("design_domain_reliable")
  }

  if (isTRUE(is_design_domain) && identical(precision_decision, "reduced_precision")) {
    return("design_domain_reduced_precision")
  }

  if (isTRUE(is_design_domain) && identical(precision_decision, "no_recommended_inference")) {
    return("design_domain_inference_not_recommended")
  }

  if (!isTRUE(is_design_domain) && identical(precision_decision, "reliable")) {
    return("analysis_domain_reliable_but_not_design_domain")
  }

  if (!isTRUE(is_design_domain) && identical(precision_decision, "reduced_precision")) {
    return("analysis_domain_reduced_precision_not_design_domain")
  }

  if (!isTRUE(is_design_domain) && identical(precision_decision, "no_recommended_inference")) {
    return("analysis_domain_inference_not_recommended")
  }

  if (identical(precision_decision, "precision_evaluation_error")) {
    return("precision_evaluation_error")
  }

  "representativity_not_classified"
}

.enemdu_final_representativity_note <- function(precision_decision,
                                                is_design_domain,
                                                domain_scope_message) {
  if (isTRUE(is_design_domain)) {
    return(glue::glue(
      "{domain_scope_message} La estimación fue evaluada con decisión de precisión `{precision_decision}`."
    ))
  }

  glue::glue(
    "{domain_scope_message} Aunque la estimación tenga decisión de precisión `{precision_decision}`, ",
    "no debe presentarse como dominio de diseño de la encuesta."
  )
}
