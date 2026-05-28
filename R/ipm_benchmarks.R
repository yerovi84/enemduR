#' Load official IPM benchmark values
#'
#' Loads published Ecuador IPM benchmark values for analytical comparison with
#' local `enemduR` estimates. The returned values are published comparison
#' benchmarks only. They are not an institutional validation of package output.
#'
#' @param period Benchmark period. Defaults to `"2025-12"`.
#' @param survey_type ENEMDU survey type. Defaults to `"anual"`.
#'
#' @return A tibble with official IPM benchmark metadata.
#' @export
enemdu_official_ipm_benchmarks <- function(
  period = "2025-12",
  survey_type = "anual"
) {
  path <- .enemdu_extdata_path("ipm_official_benchmarks.csv")

  if (!nzchar(path)) {
    rlang::abort(
      message = "Official IPM benchmark file was not found in inst/extdata.",
      class = c("enemdu_error_missing_official_ipm_benchmarks", "enemdu_error")
    )
  }

  benchmarks <- readr::read_csv(path, show_col_types = FALSE, progress = FALSE)
  .enemdu_validate_official_ipm_benchmarks(benchmarks)
  benchmarks <- .enemdu_normalize_official_ipm_benchmarks(benchmarks)

  if (!is.null(period)) {
    benchmarks <- benchmarks[
      as.character(benchmarks$period) %in% as.character(period),
      ,
      drop = FALSE
    ]
  }

  if (!is.null(survey_type)) {
    survey_type <- .enemdu_normalize_survey_type(
      survey_type,
      caller = "enemdu_official_ipm_benchmarks"
    )
    benchmarks <- benchmarks[
      as.character(benchmarks$survey_type) %in% survey_type,
      ,
      drop = FALSE
    ]
  }

  tibble::as_tibble(benchmarks)
}

#' Compare IPM estimates against official published benchmarks
#'
#' Compares local package estimates with published IPM benchmark values. The
#' comparison is expressed in percentage points. For the official IPM points
#' series, the internal proportion scale is used, so `0.205` corresponds to
#' `20.5` reported points.
#'
#' This is a local reproducibility comparison, not official institutional
#' validation.
#'
#' @param estimates A data frame containing local IPM estimates.
#' @param benchmarks Benchmark table. Defaults to
#' `enemdu_official_ipm_benchmarks()`.
#' @param tolerance_pp Tolerance in percentage points.
#'
#' @return A tibble with comparison differences and status metadata.
#' @export
enemdu_compare_official_ipm <- function(
  estimates,
  benchmarks = enemdu_official_ipm_benchmarks(),
  tolerance_pp = 0.5
) {
  if (!is.data.frame(estimates)) {
    .enemdu_abort_invalid_data(caller = "enemdu_compare_official_ipm")
  }

  if (!is.data.frame(benchmarks)) {
    rlang::abort(
      message = "`benchmarks` must be a data frame in `enemdu_compare_official_ipm()`.",
      class = c("enemdu_error_invalid_official_ipm_benchmarks", "enemdu_error")
    )
  }

  if (!is.numeric(tolerance_pp) ||
      length(tolerance_pp) != 1 ||
      is.na(tolerance_pp) ||
      tolerance_pp < 0) {
    rlang::abort(
      message = "`tolerance_pp` must be a single non-negative numeric value.",
      class = c("enemdu_error_invalid_tolerance", "enemdu_error")
    )
  }

  .enemdu_validate_official_ipm_benchmarks(benchmarks)
  benchmark_work <- .enemdu_normalize_official_ipm_benchmarks(benchmarks)

  estimate_work <- .enemdu_prepare_ipm_estimates_for_comparison(
    estimates = estimates,
    benchmarks = benchmark_work
  )

  .enemdu_abort_duplicate_ipm_comparison_keys(
    estimate_work,
    side = "package"
  )
  .enemdu_abort_duplicate_ipm_comparison_keys(
    benchmark_work,
    side = "official"
  )

  comparison <- merge(
    benchmark_work,
    estimate_work,
    by = c(
      "period",
      "survey_type",
      "indicator_id",
      "domain_type",
      "domain_value"
    ),
    all = TRUE,
    sort = FALSE
  )

  comparison[["official_estimate_proportion"]] <-
    .enemdu_ipm_official_estimate_proportion(comparison)
  comparison[["official_estimate_percent"]] <-
    comparison[["official_estimate_proportion"]] * 100
  comparison[["estimate"]] <- comparison[["package_estimate"]]
  comparison[["estimate_percent"]] <- comparison[["estimate"]] * 100
  comparison[["difference"]] <-
    comparison[["estimate"]] - comparison[["official_estimate_proportion"]]
  comparison[["difference_pp"]] <-
    comparison[["estimate_percent"]] - comparison[["official_estimate_percent"]]
  comparison[["abs_difference_pp"]] <- abs(comparison[["difference_pp"]])
  comparison[["tolerance_pp"]] <- tolerance_pp
  comparison[["comparison_status"]] <- .enemdu_ipm_comparison_status(
    comparison = comparison,
    tolerance_pp = tolerance_pp
  )
  comparison[["comparison_validation_status"]] <-
    "comparison_only_not_official_validation"
  comparison[["official_validation_status"]] <- "not_officially_validated"
  comparison[["comparison_note"]] <- paste(
    "Analytical comparison against published IPM benchmarks.",
    "This output is not an official validation claim."
  )

  front_cols <- c(
    "period",
    "survey_type",
    "indicator_id",
    "indicator_label",
    "domain_type",
    "domain_value",
    "domain_label",
    "estimate",
    "official_estimate",
    "official_estimate_proportion",
    "estimate_percent",
    "official_estimate_percent",
    "official_reported_value",
    "official_scale",
    "reported_unit",
    "difference",
    "difference_pp",
    "abs_difference_pp",
    "tolerance_pp",
    "comparison_status",
    "comparison_validation_status",
    "official_validation_status",
    "comparison_note"
  )

  comparison <- comparison[c(front_cols, setdiff(names(comparison), front_cols))]
  comparison <- tibble::as_tibble(comparison)

  attr(comparison, "comparison_policy") <- list(
    tolerance_pp = tolerance_pp,
    official_validation_status = "not_officially_validated",
    note = paste(
      "Comparison uses published IPM benchmark values.",
      "It does not imply official validation without reviewed evidence."
    )
  )

  class(comparison) <- unique(c("enemdu_official_ipm_comparison", class(comparison)))
  comparison
}

.enemdu_validate_official_ipm_benchmarks <- function(benchmarks) {
  required_cols <- c(
    "period",
    "survey_type",
    "benchmark_set",
    "indicator_id",
    "indicator_label",
    "domain_type",
    "domain_value",
    "domain_label",
    "official_estimate",
    "official_reported_value",
    "official_scale",
    "reported_unit",
    "source_name",
    "source_note",
    "official_validation_status"
  )

  missing_cols <- setdiff(required_cols, names(benchmarks))

  if (length(missing_cols) > 0) {
    rlang::abort(
      message = glue::glue(
        "Official IPM benchmark registry is missing columns: ",
        "{paste(missing_cols, collapse = ', ')}."
      ),
      class = c("enemdu_error_invalid_official_ipm_benchmarks", "enemdu_error")
    )
  }

  invisible(TRUE)
}

.enemdu_normalize_official_ipm_benchmarks <- function(benchmarks) {
  benchmarks <- tibble::as_tibble(benchmarks)

  numeric_cols <- c("official_estimate", "official_reported_value")

  for (col in numeric_cols) {
    benchmarks[[col]] <- suppressWarnings(as.numeric(benchmarks[[col]]))
  }

  character_cols <- setdiff(names(benchmarks), numeric_cols)

  for (col in character_cols) {
    benchmarks[[col]] <- as.character(benchmarks[[col]])
  }

  benchmarks
}

.enemdu_prepare_ipm_estimates_for_comparison <- function(estimates,
                                                        benchmarks) {
  .enemdu_abort_missing_vars(
    vars = c("indicator_id", "estimate"),
    names_data = names(estimates),
    caller = "enemdu_compare_official_ipm"
  )

  raw_estimate <- if (is.factor(estimates[["estimate"]])) {
    as.character(estimates[["estimate"]])
  } else {
    estimates[["estimate"]]
  }
  estimate_value <- suppressWarnings(as.numeric(raw_estimate))

  invalid_estimate <- !is.na(raw_estimate) & is.na(estimate_value)

  if (any(invalid_estimate)) {
    rlang::abort(
      message = "IPM comparison estimates must be numeric or safely coercible to numeric.",
      class = c("enemdu_error_invalid_ipm_comparison_estimate", "enemdu_error")
    )
  }

  if (all(c("domain_type", "domain_value") %in% names(estimates))) {
    domain_type <- as.character(estimates[["domain_type"]])
    domain_value <- as.character(estimates[["domain_value"]])
  } else if ("area" %in% names(estimates)) {
    domain_type <- rep("area", nrow(estimates))
    domain_value <- .enemdu_ipm_map_domain_values(estimates[["area"]])
  } else {
    domain_type <- rep("national", nrow(estimates))
    domain_value <- rep("national", nrow(estimates))
  }

  period <- .enemdu_ipm_comparison_key_or_default(
    estimates = estimates,
    col = "period",
    default = unique(benchmarks$period)
  )
  survey_type <- .enemdu_ipm_comparison_key_or_default(
    estimates = estimates,
    col = "survey_type",
    default = unique(benchmarks$survey_type)
  )

  tibble::tibble(
    period = period,
    survey_type = survey_type,
    indicator_id = as.character(estimates[["indicator_id"]]),
    domain_type = domain_type,
    domain_value = domain_value,
    package_estimate = estimate_value,
    package_estimate_present = TRUE
  )
}

.enemdu_ipm_comparison_key_or_default <- function(estimates, col, default) {
  if (col %in% names(estimates)) {
    return(as.character(estimates[[col]]))
  }

  default <- as.character(default)
  default <- default[!is.na(default) & nzchar(default)]

  if (length(default) == 1) {
    return(rep(default, nrow(estimates)))
  }

  rep(NA_character_, nrow(estimates))
}

.enemdu_ipm_official_estimate_proportion <- function(comparison) {
  scale <- as.character(comparison$official_scale)
  estimate <- suppressWarnings(as.numeric(comparison$official_estimate))
  out <- rep(NA_real_, length(estimate))

  missing_estimate <- is.na(estimate)
  ok_proportion <- !missing_estimate & scale == "proportion"
  ok_percent <- !missing_estimate & scale %in% c("percent", "points")
  invalid_scale <- !missing_estimate & !scale %in% c("proportion", "percent", "points")

  out[ok_proportion] <- estimate[ok_proportion]
  out[ok_percent] <- estimate[ok_percent] / 100

  if (any(invalid_scale | (!missing_estimate & is.na(scale)))) {
    rlang::abort(
      message = "Official IPM benchmark scale must be proportion, percent, or points.",
      class = c("enemdu_error_invalid_official_ipm_scale", "enemdu_error")
    )
  }

  out
}

.enemdu_ipm_comparison_status <- function(comparison, tolerance_pp) {
  out <- rep(NA_character_, nrow(comparison))

  benchmark_missing <- is.na(comparison$official_estimate_proportion)
  package_missing <- is.na(comparison$package_estimate_present) |
    is.na(comparison$package_estimate)

  out[benchmark_missing] <- "missing_official_benchmark"
  out[!benchmark_missing & package_missing] <- "missing_package_estimate"

  comparable <- is.na(out)
  reported_rounding_pp <- min(0.05, tolerance_pp)

  within_rounding <- comparable &
    comparison$abs_difference_pp <= reported_rounding_pp + .Machine$double.eps
  within_tolerance <- comparable &
    comparison$abs_difference_pp > reported_rounding_pp + .Machine$double.eps &
    comparison$abs_difference_pp <= tolerance_pp + .Machine$double.eps
  outside_tolerance <- comparable &
    comparison$abs_difference_pp > tolerance_pp + .Machine$double.eps

  out[within_rounding] <- "matched_reported_rounding"
  out[within_tolerance] <- "benchmark_comparison_within_tolerance"
  out[outside_tolerance] <- "benchmark_comparison_outside_tolerance"

  out
}

.enemdu_abort_duplicate_ipm_comparison_keys <- function(x, side) {
  key <- paste(
    as.character(x$period),
    as.character(x$survey_type),
    as.character(x$indicator_id),
    as.character(x$domain_type),
    as.character(x$domain_value),
    sep = "\r"
  )

  duplicated_keys <- unique(key[duplicated(key)])

  if (length(duplicated_keys) > 0) {
    rlang::abort(
      message = glue::glue(
        "Duplicated {side} IPM comparison keys were found. ",
        "Filter to one estimate or benchmark row per indicator and domain."
      ),
      class = c("enemdu_error_duplicate_official_ipm_comparison_key", "enemdu_error")
    )
  }

  invisible(TRUE)
}
