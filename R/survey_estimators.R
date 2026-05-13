#' Estimate a survey mean using ENEMDU complex survey design
#'
#' Estimates a weighted mean using ENEMDU design variables and returns a stable
#' analytical output with precision metrics. The function uses `survey::svymean()`
#' internally and connects the result with the package precision-decision rules.
#'
#' @param data A data frame.
#' @param value Numeric variable to estimate.
#' @param group_vars Optional grouping variables. If supplied, estimates are
#' computed separately for each observed group.
#' @param ids Primary sampling unit variable. Defaults to `"upm"`.
#' @param strata Strata variable. Defaults to `"estrato"`.
#' @param weight Expansion factor variable. Defaults to `"fexp"`.
#' @param survey_type Optional ENEMDU survey type. If omitted, uses the
#' `survey_type` attribute when available.
#' @param indicator_id Optional stable indicator identifier.
#' @param measure Optional human-readable measure name.
#' @param na_rm Logical. If `TRUE`, missing values in `value` are excluded.
#' @param conf_level Confidence level for confidence intervals.
#' @param lonely_psu Option passed to `survey.lonely.psu`.
#' @param sample_n_min Minimum unweighted sample size for preliminary quality
#' flag.
#'
#' @return A tibble with survey mean estimates and precision metadata.
#' @export
enemdu_survey_mean <- function(data,
                               value,
                               group_vars = NULL,
                               ids = "upm",
                               strata = "estrato",
                               weight = "fexp",
                               survey_type = NULL,
                               indicator_id = NULL,
                               measure = NULL,
                               na_rm = TRUE,
                               conf_level = 0.95,
                               lonely_psu = "adjust",
                               sample_n_min = 60) {
  .enemdu_survey_estimate(
    data = data,
    value = value,
    group_vars = group_vars,
    ids = ids,
    strata = strata,
    weight = weight,
    survey_type = survey_type,
    indicator_id = indicator_id,
    measure = measure,
    statistic = "survey_mean",
    estimator_type = "mean",
    na_rm = na_rm,
    conf_level = conf_level,
    lonely_psu = lonely_psu,
    sample_n_min = sample_n_min
  )
}

#' Estimate a survey total using ENEMDU complex survey design
#'
#' Estimates a weighted total using ENEMDU design variables and returns a stable
#' analytical output with precision metrics. The function uses
#' `survey::svytotal()` internally.
#'
#' @param data A data frame.
#' @param value Numeric variable to total.
#' @param group_vars Optional grouping variables.
#' @param ids Primary sampling unit variable. Defaults to `"upm"`.
#' @param strata Strata variable. Defaults to `"estrato"`.
#' @param weight Expansion factor variable. Defaults to `"fexp"`.
#' @param survey_type Optional ENEMDU survey type.
#' @param indicator_id Optional stable indicator identifier.
#' @param measure Optional human-readable measure name.
#' @param na_rm Logical. If `TRUE`, missing values in `value` are excluded.
#' @param conf_level Confidence level for confidence intervals.
#' @param lonely_psu Option passed to `survey.lonely.psu`.
#' @param sample_n_min Minimum unweighted sample size for preliminary quality
#' flag.
#'
#' @return A tibble with survey total estimates and precision metadata.
#' @export
enemdu_survey_total <- function(data,
                                value,
                                group_vars = NULL,
                                ids = "upm",
                                strata = "estrato",
                                weight = "fexp",
                                survey_type = NULL,
                                indicator_id = NULL,
                                measure = NULL,
                                na_rm = TRUE,
                                conf_level = 0.95,
                                lonely_psu = "adjust",
                                sample_n_min = 60) {
  .enemdu_survey_estimate(
    data = data,
    value = value,
    group_vars = group_vars,
    ids = ids,
    strata = strata,
    weight = weight,
    survey_type = survey_type,
    indicator_id = indicator_id,
    measure = measure,
    statistic = "survey_total",
    estimator_type = "total",
    na_rm = na_rm,
    conf_level = conf_level,
    lonely_psu = lonely_psu,
    sample_n_min = sample_n_min
  )
}

#' Estimate a survey proportion using ENEMDU complex survey design
#'
#' Estimates a weighted proportion for a binary 0/1 variable using ENEMDU design
#' variables. This function is appropriate for variables such as poverty flags
#' once they have been built from validated inputs.
#'
#' @param data A data frame.
#' @param value Binary 0/1 variable.
#' @param group_vars Optional grouping variables.
#' @param ids Primary sampling unit variable. Defaults to `"upm"`.
#' @param strata Strata variable. Defaults to `"estrato"`.
#' @param weight Expansion factor variable. Defaults to `"fexp"`.
#' @param survey_type Optional ENEMDU survey type.
#' @param indicator_id Optional stable indicator identifier.
#' @param measure Optional human-readable measure name.
#' @param na_rm Logical. If `TRUE`, missing values in `value` are excluded.
#' @param conf_level Confidence level for confidence intervals.
#' @param lonely_psu Option passed to `survey.lonely.psu`.
#' @param sample_n_min Minimum unweighted sample size for preliminary quality
#' flag.
#' @param strict_binary Logical. If `TRUE`, errors when non-missing values other
#' than 0 or 1 are found.
#'
#' @return A tibble with survey proportion estimates and precision metadata.
#' @export
enemdu_survey_proportion <- function(data,
                                     value,
                                     group_vars = NULL,
                                     ids = "upm",
                                     strata = "estrato",
                                     weight = "fexp",
                                     survey_type = NULL,
                                     indicator_id = NULL,
                                     measure = NULL,
                                     na_rm = TRUE,
                                     conf_level = 0.95,
                                     lonely_psu = "adjust",
                                     sample_n_min = 60,
                                     strict_binary = TRUE) {
  if (isTRUE(strict_binary)) {
    if (!is.data.frame(data)) {
      .enemdu_abort_invalid_data(caller = "enemdu_survey_proportion")
    }

    .enemdu_abort_missing_vars(
      vars = value,
      names_data = names(data),
      caller = "enemdu_survey_proportion"
    )

    invalid <- !is.na(data[[value]]) & !data[[value]] %in% c(0, 1)

    if (any(invalid)) {
      rlang::abort(
        message = glue::glue(
          "Variable `{value}` must be binary 0/1 in `enemdu_survey_proportion()`. ",
          "{sum(invalid)} non-missing values are outside 0/1."
        ),
        class = c("enemdu_error_non_binary_proportion", "enemdu_error")
      )
    }
  }

  .enemdu_survey_estimate(
    data = data,
    value = value,
    group_vars = group_vars,
    ids = ids,
    strata = strata,
    weight = weight,
    survey_type = survey_type,
    indicator_id = indicator_id,
    measure = measure,
    statistic = "survey_proportion",
    estimator_type = "proportion_0_1",
    na_rm = na_rm,
    conf_level = conf_level,
    lonely_psu = lonely_psu,
    sample_n_min = sample_n_min
  )
}

.enemdu_survey_estimate <- function(data,
                                    value,
                                    group_vars = NULL,
                                    ids = "upm",
                                    strata = "estrato",
                                    weight = "fexp",
                                    survey_type = NULL,
                                    indicator_id = NULL,
                                    measure = NULL,
                                    statistic = c(
                                      "survey_mean",
                                      "survey_total",
                                      "survey_proportion"
                                    ),
                                    estimator_type = c(
                                      "mean",
                                      "total",
                                      "proportion_0_1"
                                    ),
                                    na_rm = TRUE,
                                    conf_level = 0.95,
                                    lonely_psu = "adjust",
                                    sample_n_min = 60) {
  if (!is.data.frame(data)) {
    .enemdu_abort_invalid_data(caller = ".enemdu_survey_estimate")
  }

  statistic <- match.arg(statistic)
  estimator_type <- match.arg(estimator_type)

  if (missing(value) || is.null(value) || length(value) != 1) {
    .enemdu_abort_missing_argument(
      "value",
      caller = ".enemdu_survey_estimate"
    )
  }

  .enemdu_abort_missing_vars(
    vars = value,
    names_data = names(data),
    caller = ".enemdu_survey_estimate"
  )

  if (!is.numeric(data[[value]])) {
    .enemdu_abort_invalid_numeric_var(
      var = value,
      caller = ".enemdu_survey_estimate"
    )
  }

  if (!is.null(group_vars)) {
    .enemdu_abort_missing_vars(
      vars = group_vars,
      names_data = names(data),
      caller = ".enemdu_survey_estimate"
    )
  }

  .enemdu_abort_missing_vars(
    vars = c(ids, strata, weight),
    names_data = names(data),
    caller = ".enemdu_survey_estimate"
  )

  if (!is.numeric(data[[weight]])) {
    .enemdu_abort_invalid_numeric_var(
      var = weight,
      caller = ".enemdu_survey_estimate"
    )
  }

  if (is.null(survey_type)) {
    survey_type <- attr(data, "survey_type") %||% NA_character_
  }

  design_domains <- NA_character_

  if (!is.na(survey_type)) {
    survey_type <- .enemdu_normalize_survey_type(
      survey_type = survey_type,
      caller = ".enemdu_survey_estimate"
    )

    scope <- enemdu_representativity_scope(
      survey_type = survey_type,
      emit = FALSE
    )

    design_domains <- scope$design_domains[[1]]
  }

  if (is.null(indicator_id) || !nzchar(indicator_id)) {
    indicator_id <- paste(statistic, value, sep = "_")
  }

  if (is.null(measure) || !nzchar(measure)) {
    measure <- indicator_id
  }

  design <- enemdu_declare_design(
    data = data,
    ids = ids,
    strata = strata,
    weights = weight,
    lonely_psu = lonely_psu
  )

  group_info <- .enemdu_group_index(
    data = data,
    group_vars = group_vars,
    drop_na_groups = TRUE
  )

  pieces <- vector("list", length(group_info$groups))
  i <- 1L

  for (group_name in names(group_info$groups)) {
    idx <- group_info$groups[[group_name]]

    if (length(idx) == 0) {
      next
    }

    sub_design <- design[idx, ]

    estimate_row <- .enemdu_survey_estimate_one(
      design = sub_design,
      value = value,
      statistic = statistic,
      estimator_type = estimator_type,
      na_rm = na_rm,
      conf_level = conf_level,
      sample_n_min = sample_n_min
    )

    out_row <- tibble::tibble(
      indicator_id = indicator_id,
      measure = measure,
      statistic = statistic,
      estimator_type = estimator_type,
      survey_type = survey_type %||% NA_character_,
      design_domains = design_domains
    )

    group_values <- group_info$group_values[[group_name]]

    if (length(group_values) > 0) {
      group_df <- as.data.frame(
        group_values,
        stringsAsFactors = FALSE
      )

      out_row <- cbind(out_row, group_df)
    }

    out_row <- cbind(out_row, estimate_row)

    pieces[[i]] <- out_row
    i <- i + 1L
  }

  pieces <- pieces[seq_len(i - 1L)]

  if (length(pieces) == 0) {
    out <- .enemdu_empty_survey_estimate()
  } else {
    out <- do.call(rbind, pieces)
    row.names(out) <- NULL
    out <- tibble::as_tibble(out)
  }

  attr(out, "survey_estimation_policy") <- list(
    ids = ids,
    strata = strata,
    weight = weight,
    na_rm = na_rm,
    conf_level = conf_level,
    lonely_psu = lonely_psu,
    sample_n_min = sample_n_min,
    note = paste(
      "Estimates were computed using the survey package and ENEMDU design variables.",
      "Precision decisions are generated through enemdu_evaluate_precision()."
    )
  )

  class(out) <- unique(c("enemdu_survey_estimate", class(out)))
  out
}

.enemdu_survey_estimate_one <- function(design,
                                        value,
                                        statistic,
                                        estimator_type,
                                        na_rm,
                                        conf_level,
                                        sample_n_min) {
  x <- design$variables[[value]]
  w <- survey::weights(design, type = "sampling")

  valid <- !is.na(x) & !is.na(w) & w > 0

  if (isTRUE(na_rm)) {
    design_eval <- design[valid, ]
  } else {
    design_eval <- design
  }

  unweighted_n <- sum(valid, na.rm = TRUE)
  weighted_n <- sum(w[valid], na.rm = TRUE)

  if (unweighted_n == 0 || weighted_n <= 0) {
    return(.enemdu_failed_survey_estimate_row(
      unweighted_n = unweighted_n,
      weighted_n = weighted_n,
      degrees_freedom = NA_real_,
      reason = "no_valid_observations"
    ))
  }

  formula <- stats::reformulate(value)

  estimate_object <- tryCatch(
    {
      if (identical(statistic, "survey_total")) {
        survey::svytotal(
          formula,
          design = design_eval,
          na.rm = na_rm,
          deff = TRUE
        )
      } else {
        survey::svymean(
          formula,
          design = design_eval,
          na.rm = na_rm,
          deff = TRUE
        )
      }
    },
    error = function(e) {
      e
    }
  )

  if (inherits(estimate_object, "error")) {
    return(.enemdu_failed_survey_estimate_row(
      unweighted_n = unweighted_n,
      weighted_n = weighted_n,
      degrees_freedom = suppressWarnings(survey::degf(design_eval)),
      reason = paste("survey_estimation_error:", estimate_object$message)
    ))
  }

  estimate <- as.numeric(stats::coef(estimate_object)[1])
  standard_error <- as.numeric(survey::SE(estimate_object)[1])

  ci <- tryCatch(
    {
      stats::confint(
        estimate_object,
        level = conf_level
      )
    },
    error = function(e) {
      matrix(c(NA_real_, NA_real_), nrow = 1)
    }
  )

  ci_lower <- as.numeric(ci[1, 1])
  ci_upper <- as.numeric(ci[1, 2])

  deff <- tryCatch(
    {
      as.numeric(survey::deff(estimate_object)[1])
    },
    error = function(e) {
      NA_real_
    }
  )

  if (is.nan(deff) || is.infinite(deff) || !is.numeric(deff)) {
    deff <- NA_real_
  }

  cv <- if (!is.na(estimate) && abs(estimate) > 0) {
    standard_error / abs(estimate)
  } else {
    NA_real_
  }

  degrees_freedom <- suppressWarnings(survey::degf(design_eval))

  effective_n <- if (!is.na(deff) && deff > 0) {
    unweighted_n / deff
  } else {
    unweighted_n
  }

  precision <- tryCatch(
    {
      enemdu_evaluate_precision(
        estimate = estimate,
        standard_error = standard_error,
        cv = cv,
        n = unweighted_n,
        effective_n = effective_n,
        deff = deff,
        degrees_freedom = degrees_freedom,
        estimator_type = estimator_type
      )
    },
    error = function(e) {
      tibble::tibble(
        decision = "precision_evaluation_error",
        failed_reasons = e$message
      )
    }
  )

  decision <- precision$decision[[1]]
  failed_reasons <- precision$failed_reasons[[1]]

  tibble::tibble(
    estimate = estimate,
    standard_error = standard_error,
    cv = cv,
    ci_lower = ci_lower,
    ci_upper = ci_upper,
    unweighted_n = as.integer(unweighted_n),
    weighted_n = as.numeric(weighted_n),
    deff = as.numeric(deff),
    effective_n = as.numeric(effective_n),
    degrees_freedom = as.numeric(degrees_freedom),
    decision = decision,
    failed_reasons = failed_reasons,
    quality_flag = .enemdu_precision_quality_flag(decision, unweighted_n, sample_n_min),
    warning_flag = .enemdu_precision_warning_flag(decision),
    method_note = paste(
      "Survey-design estimate computed with survey package.",
      "Effective sample size is approximated as unweighted_n / deff when deff is available;",
      "otherwise unweighted_n is used."
    )
  )
}

.enemdu_failed_survey_estimate_row <- function(unweighted_n,
                                               weighted_n,
                                               degrees_freedom,
                                               reason) {
  tibble::tibble(
    estimate = NA_real_,
    standard_error = NA_real_,
    cv = NA_real_,
    ci_lower = NA_real_,
    ci_upper = NA_real_,
    unweighted_n = as.integer(unweighted_n),
    weighted_n = as.numeric(weighted_n),
    deff = NA_real_,
    effective_n = NA_real_,
    degrees_freedom = as.numeric(degrees_freedom),
    decision = "no_recommended_inference",
    failed_reasons = reason,
    quality_flag = "not_evaluable",
    warning_flag = "inference_not_recommended",
    method_note = "Survey-design estimate could not be computed for this domain."
  )
}

.enemdu_empty_survey_estimate <- function() {
  tibble::tibble(
    indicator_id = character(),
    measure = character(),
    statistic = character(),
    estimator_type = character(),
    survey_type = character(),
    design_domains = character(),
    estimate = numeric(),
    standard_error = numeric(),
    cv = numeric(),
    ci_lower = numeric(),
    ci_upper = numeric(),
    unweighted_n = integer(),
    weighted_n = numeric(),
    deff = numeric(),
    effective_n = numeric(),
    degrees_freedom = numeric(),
    decision = character(),
    failed_reasons = character(),
    quality_flag = character(),
    warning_flag = character(),
    method_note = character()
  )
}

.enemdu_precision_quality_flag <- function(decision,
                                           unweighted_n,
                                           sample_n_min = 60) {
  if (!is.na(unweighted_n) && unweighted_n < sample_n_min) {
    return("low_sample_size")
  }

  if (identical(decision, "reliable")) {
    return("reliable")
  }

  if (identical(decision, "reduced_precision")) {
    return("reduced_precision")
  }

  if (identical(decision, "no_recommended_inference")) {
    return("not_recommended_for_inference")
  }

  if (identical(decision, "precision_evaluation_error")) {
    return("precision_evaluation_error")
  }

  "not_evaluated"
}

.enemdu_precision_warning_flag <- function(decision) {
  if (identical(decision, "reliable")) {
    return("none")
  }

  if (identical(decision, "reduced_precision")) {
    return("use_with_caution")
  }

  if (identical(decision, "no_recommended_inference")) {
    return("inference_not_recommended")
  }

  if (identical(decision, "precision_evaluation_error")) {
    return("precision_evaluation_error")
  }

  "precision_not_evaluated"
}
