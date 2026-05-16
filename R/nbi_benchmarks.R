#' Load official NBI benchmark metadata
#'
#' Loads the package NBI benchmark registry. The initial registry can be empty
#' because this phase does not add official NBI benchmark values.
#'
#' @param period Optional period filter.
#' @param benchmark_set Optional benchmark set filter.
#' @param indicator_id Optional indicator id filter.
#' @param domain_type Optional domain type filter.
#' @param domain_value Optional domain value filter.
#'
#' @return A tibble with official NBI benchmark metadata.
#' @export
enemdu_official_nbi_benchmarks <- function(period = NULL,
                                           benchmark_set = NULL,
                                           indicator_id = NULL,
                                           domain_type = NULL,
                                           domain_value = NULL) {
  path <- .enemdu_extdata_path("official_nbi_benchmarks.csv")

  if (!nzchar(path)) {
    rlang::abort(
      message = "Official NBI benchmark file was not found in inst/extdata.",
      class = c("enemdu_error_missing_official_nbi_benchmarks", "enemdu_error")
    )
  }

  benchmarks <- readr::read_csv(path, show_col_types = FALSE, progress = FALSE)
  .enemdu_validate_official_nbi_benchmarks(benchmarks)
  benchmarks <- .enemdu_normalize_official_nbi_benchmarks(benchmarks)

  if (!is.null(period)) {
    benchmarks <- benchmarks[as.character(benchmarks$period) %in% as.character(period), , drop = FALSE]
  }

  if (!is.null(benchmark_set)) {
    benchmarks <- benchmarks[
      as.character(benchmarks$benchmark_set) %in% as.character(benchmark_set),
      ,
      drop = FALSE
    ]
  }

  if (!is.null(indicator_id)) {
    benchmarks <- benchmarks[
      as.character(benchmarks$indicator_id) %in% as.character(indicator_id),
      ,
      drop = FALSE
    ]
  }

  if (!is.null(domain_type)) {
    benchmarks <- benchmarks[
      as.character(benchmarks$domain_type) %in% as.character(domain_type),
      ,
      drop = FALSE
    ]
  }

  if (!is.null(domain_value)) {
    benchmarks <- benchmarks[
      as.character(benchmarks$domain_value) %in% as.character(domain_value),
      ,
      drop = FALSE
    ]
  }

  tibble::as_tibble(benchmarks)
}

#' Compare NBI estimates against official benchmarks
#'
#' Compares package NBI estimates against official benchmark rows when those
#' rows are available. This helper is a readiness layer and does not claim
#' official validation.
#'
#' @param estimates A data frame with NBI estimates.
#' @param benchmarks Official NBI benchmark table.
#' @param period Optional benchmark period filter.
#' @param benchmark_set Optional benchmark set filter.
#' @param domain_vars Optional package estimate domain variables. The first
#' variable is used as the benchmark `domain_type`.
#' @param domain_map Optional named vector mapping package domain values to
#' benchmark domain values.
#' @param estimate_col Estimate column in `estimates`.
#' @param indicator_col Indicator id column in `estimates`.
#' @param tolerance_pp Tolerance in percentage points.
#' @param strict Logical. If `TRUE`, error when comparisons are missing or
#' outside tolerance.
#'
#' @return A tibble with comparison metadata.
#' @export
enemdu_compare_official_nbi <- function(estimates,
                                        benchmarks = enemdu_official_nbi_benchmarks(),
                                        period = NULL,
                                        benchmark_set = NULL,
                                        domain_vars = NULL,
                                        domain_map = NULL,
                                        estimate_col = "estimate",
                                        indicator_col = "indicator_id",
                                        tolerance_pp = 0.10,
                                        strict = FALSE) {
  if (!is.data.frame(estimates)) {
    .enemdu_abort_invalid_data(caller = "enemdu_compare_official_nbi")
  }

  if (!is.data.frame(benchmarks)) {
    rlang::abort(
      message = "`benchmarks` must be a data frame in `enemdu_compare_official_nbi()`.",
      class = c("enemdu_error_invalid_official_nbi_benchmarks", "enemdu_error")
    )
  }

  .enemdu_validate_official_nbi_benchmarks(benchmarks)
  benchmarks <- .enemdu_normalize_official_nbi_benchmarks(benchmarks)

  .enemdu_abort_missing_vars(
    vars = c(indicator_col, estimate_col),
    names_data = names(estimates),
    caller = "enemdu_compare_official_nbi"
  )

  if (!is.numeric(tolerance_pp) || length(tolerance_pp) != 1 || is.na(tolerance_pp) || tolerance_pp < 0) {
    rlang::abort(
      message = "`tolerance_pp` must be a single non-negative numeric value.",
      class = c("enemdu_error_invalid_tolerance", "enemdu_error")
    )
  }

  benchmark_work <- benchmarks

  if (!is.null(period)) {
    benchmark_work <- benchmark_work[
      as.character(benchmark_work$period) %in% as.character(period),
      ,
      drop = FALSE
    ]
  }

  if (!is.null(benchmark_set)) {
    benchmark_work <- benchmark_work[
      as.character(benchmark_work$benchmark_set) %in% as.character(benchmark_set),
      ,
      drop = FALSE
    ]
  }

  estimate_work <- .enemdu_prepare_nbi_estimates_for_comparison(
    estimates = estimates,
    indicator_col = indicator_col,
    estimate_col = estimate_col,
    domain_vars = domain_vars,
    domain_map = domain_map
  )

  if (is.null(domain_vars) || length(domain_vars) == 0) {
    benchmark_work <- benchmark_work[
      benchmark_work$domain_type == "national" &
        benchmark_work$domain_value == "national",
      ,
      drop = FALSE
    ]
  } else {
    benchmark_domain_type <- as.character(domain_vars[[1]])
    benchmark_work <- benchmark_work[
      benchmark_work$domain_type == benchmark_domain_type,
      ,
      drop = FALSE
    ]
  }

  .enemdu_abort_duplicate_nbi_comparison_keys(estimate_work, side = "package")
  .enemdu_abort_duplicate_nbi_comparison_keys(benchmark_work, side = "official")

  comparison <- merge(
    benchmark_work,
    estimate_work,
    by = c("indicator_id", "domain_type", "domain_value"),
    all = TRUE,
    sort = FALSE
  )

  official_scale <- .enemdu_normalize_nbi_official_estimates(
    official_estimate = comparison[["official_estimate"]],
    official_scale = comparison[["official_scale"]]
  )
  comparison[["official_estimate_proportion"]] <- official_scale$proportion
  comparison[["official_estimate_percent"]] <- official_scale$percent
  comparison[["official_percent"]] <- comparison[["official_estimate_percent"]]
  comparison[["calculated_proportion"]] <- comparison[["calculated_estimate"]]
  comparison[["calculated_percent"]] <- comparison[["calculated_estimate"]] * 100
  comparison[["difference"]] <- comparison[["calculated_proportion"]] -
    comparison[["official_estimate_proportion"]]
  comparison[["difference_pp"]] <- comparison[["calculated_percent"]] -
    comparison[["official_estimate_percent"]]
  comparison[["abs_difference_pp"]] <- abs(comparison[["difference_pp"]])
  comparison[["tolerance_pp"]] <- tolerance_pp
  comparison[["comparison_status"]] <- .enemdu_official_nbi_comparison_status(
    comparison = comparison,
    tolerance_pp = tolerance_pp
  )
  comparison[["official_validation_status"]] <- "not_officially_validated"
  comparison[["official_validation_note"]] <- paste(
    "This is a benchmark comparison readiness output.",
    "It is not an official validation claim."
  )

  front_cols <- c(
    "indicator_id",
    "domain_type",
    "domain_value",
    "period",
    "benchmark_set",
    "calculated_estimate",
    "calculated_proportion",
    "official_estimate",
    "official_estimate_proportion",
    "official_estimate_percent",
    "calculated_percent",
    "official_percent",
    "difference",
    "difference_pp",
    "abs_difference_pp",
    "tolerance_pp",
    "comparison_status",
    "official_validation_status",
    "official_validation_note"
  )

  comparison <- comparison[c(front_cols, setdiff(names(comparison), front_cols))]
  comparison <- tibble::as_tibble(comparison)
  class(comparison) <- unique(c("enemdu_official_nbi_comparison", class(comparison)))

  attr(comparison, "comparison_policy") <- list(
    tolerance_pp = tolerance_pp,
    strict = strict,
    period = period,
    benchmark_set = benchmark_set,
    note = paste(
      "NBI benchmark comparison does not imply official validation.",
      "Official validation requires reviewed comparison against published INEC outputs."
    )
  )

  bad <- comparison$comparison_status %in% c(
    "outside_tolerance",
    "missing_official_benchmark",
    "missing_package_estimate"
  )

  if (isTRUE(strict) && any(bad, na.rm = TRUE)) {
    rlang::abort(
      message = glue::glue(
        "Official NBI comparison found {sum(bad, na.rm = TRUE)} issue(s)."
      ),
      class = c("enemdu_error_official_nbi_comparison_mismatch", "enemdu_error"),
      comparison = comparison
    )
  }

  comparison
}

.enemdu_validate_official_nbi_benchmarks <- function(benchmarks) {
  required_cols <- c(
    "benchmark_set",
    "period",
    "indicator_id",
    "domain_type",
    "domain_value",
    "domain_label",
    "official_estimate",
    "official_scale",
    "source_file",
    "source_table",
    "source_note",
    "source_status"
  )

  missing_cols <- setdiff(required_cols, names(benchmarks))

  if (length(missing_cols) > 0) {
    rlang::abort(
      message = glue::glue(
        "Official NBI benchmark registry is missing columns: ",
        "{paste(missing_cols, collapse = ', ')}."
      ),
      class = c("enemdu_error_invalid_official_nbi_benchmarks", "enemdu_error")
    )
  }

  invisible(TRUE)
}

.enemdu_normalize_official_nbi_benchmarks <- function(benchmarks) {
  benchmarks <- tibble::as_tibble(benchmarks)
  benchmarks[["official_estimate"]] <- .enemdu_parse_nbi_numeric_column(
    values = benchmarks[["official_estimate"]],
    argument = "official_estimate"
  )

  character_cols <- setdiff(names(benchmarks), "official_estimate")

  for (col in character_cols) {
    benchmarks[[col]] <- as.character(benchmarks[[col]])
  }

  benchmarks
}

.enemdu_prepare_nbi_estimates_for_comparison <- function(estimates,
                                                         indicator_col,
                                                         estimate_col,
                                                         domain_vars,
                                                         domain_map) {
  if (is.null(domain_vars) || length(domain_vars) == 0) {
    domain_type <- rep("national", nrow(estimates))
    domain_value <- rep("national", nrow(estimates))
  } else {
    domain_var <- as.character(domain_vars[[1]])
    .enemdu_abort_missing_vars(
      vars = domain_var,
      names_data = names(estimates),
      caller = "enemdu_compare_official_nbi"
    )

    raw_domain_value <- as.character(estimates[[domain_var]])
    domain_type <- rep(domain_var, nrow(estimates))
    domain_value <- .enemdu_map_nbi_domain_values(
      values = raw_domain_value,
      domain_map = domain_map
    )
  }

  tibble::tibble(
    indicator_id = as.character(estimates[[indicator_col]]),
    domain_type = domain_type,
    domain_value = domain_value,
    calculated_estimate = .enemdu_parse_nbi_numeric_column(
      values = estimates[[estimate_col]],
      argument = estimate_col
    ),
    calculated_estimate_present = TRUE
  )
}

.enemdu_map_nbi_domain_values <- function(values, domain_map = NULL) {
  values <- as.character(values)

  if (is.null(domain_map) || is.null(names(domain_map))) {
    return(values)
  }

  mapped <- unname(domain_map[values])
  missing_map <- is.na(mapped)
  mapped[missing_map] <- values[missing_map]
  as.character(mapped)
}

.enemdu_abort_duplicate_nbi_comparison_keys <- function(x, side) {
  if (nrow(x) == 0) {
    return(invisible(TRUE))
  }

  key <- paste(
    as.character(x$indicator_id),
    as.character(x$domain_type),
    as.character(x$domain_value),
    sep = "\r"
  )

  duplicated_keys <- unique(key[duplicated(key)])

  if (length(duplicated_keys) > 0) {
    rlang::abort(
      message = glue::glue(
        "Duplicated {side} NBI comparison keys were found. ",
        "Filter to one estimate or benchmark row per indicator and domain."
      ),
      class = c("enemdu_error_duplicate_official_nbi_comparison_key", "enemdu_error")
    )
  }

  invisible(TRUE)
}

.enemdu_parse_nbi_numeric_column <- function(values,
                                             argument) {
  missing <- is.na(values)

  if (is.factor(values)) {
    raw <- trimws(as.character(values))
  } else if (is.logical(values)) {
    raw <- as.integer(values)
  } else if (is.numeric(values) || is.integer(values)) {
    raw <- values
  } else if (is.character(values)) {
    raw <- trimws(values)
  } else {
    raw <- trimws(as.character(values))
  }

  numeric_values <- suppressWarnings(as.numeric(raw))
  numeric_values[missing] <- NA_real_

  invalid_conversion <- !missing & is.na(numeric_values)

  if (any(invalid_conversion, na.rm = TRUE)) {
    rlang::abort(
      message = glue::glue(
        "NBI numeric column `{argument}` contains non-numeric non-missing values."
      ),
      class = c("enemdu_error_invalid_nbi_numeric_column", "enemdu_error")
    )
  }

  numeric_values
}


.enemdu_normalize_nbi_official_estimates <- function(official_estimate,
                                                     official_scale) {
  official_estimate <- .enemdu_parse_nbi_numeric_column(
    values = official_estimate,
    argument = "official_estimate"
  )
  official_scale <- tolower(trimws(as.character(official_scale)))

  proportion_scales <- c("proportion", "proportions", "rate", "rates", "ratio", "decimal")
  percent_scales <- c("percent", "percentage", "percentages", "pct")

  out <- tibble::tibble(
    proportion = rep(NA_real_, length(official_estimate)),
    percent = rep(NA_real_, length(official_estimate))
  )

  missing_benchmark <- is.na(official_estimate)
  has_benchmark <- !missing_benchmark

  proportion_rows <- has_benchmark & !is.na(official_scale) &
    official_scale %in% proportion_scales

  percent_rows <- has_benchmark & !is.na(official_scale) &
    official_scale %in% percent_scales

  unknown_rows <- has_benchmark & !(proportion_rows | percent_rows)
  unknown_rows[is.na(unknown_rows)] <- FALSE

  if (any(unknown_rows, na.rm = TRUE)) {
    bad_scale <- unique(official_scale[unknown_rows])
    bad_scale[is.na(bad_scale) | !nzchar(bad_scale)] <- "<missing>"

    rlang::abort(
      message = glue::glue(
        "Unknown official NBI benchmark scale: {paste(bad_scale, collapse = ', ')}."
      ),
      class = c("enemdu_error_invalid_official_nbi_scale", "enemdu_error")
    )
  }

  out$proportion[proportion_rows] <- official_estimate[proportion_rows]
  out$percent[proportion_rows] <- official_estimate[proportion_rows] * 100
  out$proportion[percent_rows] <- official_estimate[percent_rows] / 100
  out$percent[percent_rows] <- official_estimate[percent_rows]

  out
}

.enemdu_official_nbi_comparison_status <- function(comparison,
                                                   tolerance_pp) {
  out <- rep(NA_character_, nrow(comparison))

  benchmark_missing <- is.na(comparison$official_estimate)
  package_missing <- is.na(comparison$calculated_estimate_present) |
    is.na(comparison$calculated_estimate)

  out[benchmark_missing] <- "missing_official_benchmark"
  out[!benchmark_missing & package_missing] <- "missing_package_estimate"

  comparable <- is.na(out) & !is.na(comparison$abs_difference_pp)
  reported_rounding_pp <- min(0.05, tolerance_pp)

  within_rounding <- comparable &
    comparison$abs_difference_pp <= reported_rounding_pp + .Machine$double.eps

  within_tolerance <- comparable &
    comparison$abs_difference_pp > reported_rounding_pp + .Machine$double.eps &
    comparison$abs_difference_pp <= tolerance_pp + .Machine$double.eps

  outside_tolerance <- comparable &
    comparison$abs_difference_pp > tolerance_pp + .Machine$double.eps

  within_rounding[is.na(within_rounding)] <- FALSE
  within_tolerance[is.na(within_tolerance)] <- FALSE
  outside_tolerance[is.na(outside_tolerance)] <- FALSE

  out[within_rounding] <- "matched_reported_rounding"
  out[within_tolerance] <- "benchmark_comparison_within_tolerance"
  out[outside_tolerance] <- "outside_tolerance"

  unresolved <- is.na(out)
  out[unresolved] <- "not_comparable"

  out
}
