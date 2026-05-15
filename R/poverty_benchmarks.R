#' Load official income poverty benchmark values
#'
#' Loads published benchmark values that can be used for analytical comparison
#' against `enemduR` income-poverty estimates. These values are reference
#' benchmarks only; they are not operational poverty-line registry rows.
#'
#' @param period Optional period filter, for example `"2025-12"`.
#' @param benchmark_set Optional benchmark set identifier.
#' @param indicator_id Optional indicator id filter.
#' @param domain_type Optional domain type filter.
#' @param domain_value Optional domain value filter.
#'
#' @return A tibble with official poverty benchmark metadata.
#' @export
enemdu_official_poverty_benchmarks <- function(period = NULL,
                                               benchmark_set = NULL,
                                               indicator_id = NULL,
                                               domain_type = NULL,
                                               domain_value = NULL) {
  path <- .enemdu_extdata_path("official_poverty_benchmarks.csv")

  if (!nzchar(path)) {
    rlang::abort(
      message = "Official poverty benchmark file was not found in inst/extdata.",
      class = c("enemdu_error_missing_official_poverty_benchmarks", "enemdu_error")
    )
  }

  benchmarks <- readr::read_csv(path, show_col_types = FALSE, progress = FALSE)
  .enemdu_validate_official_poverty_benchmarks(benchmarks)
  benchmarks <- .enemdu_normalize_official_poverty_benchmarks(benchmarks)

  if (!is.null(period)) {
    benchmarks <- benchmarks[as.character(benchmarks$period) %in% as.character(period), , drop = FALSE]
  }

  if (!is.null(benchmark_set)) {
    benchmarks <- benchmarks[as.character(benchmarks$benchmark_set) %in% as.character(benchmark_set), , drop = FALSE]
  }

  if (!is.null(indicator_id)) {
    benchmarks <- benchmarks[as.character(benchmarks$indicator_id) %in% as.character(indicator_id), , drop = FALSE]
  }

  if (!is.null(domain_type)) {
    benchmarks <- benchmarks[as.character(benchmarks$domain_type) %in% as.character(domain_type), , drop = FALSE]
  }

  if (!is.null(domain_value)) {
    benchmarks <- benchmarks[as.character(benchmarks$domain_value) %in% as.character(domain_value), , drop = FALSE]
  }

  tibble::as_tibble(benchmarks)
}

#' Compare income poverty estimates against official published benchmarks
#'
#' Compares package estimates with published income-poverty benchmarks. This is
#' an analytical comparison layer, not an official validation claim. Real
#' reproducibility requires running the full pipeline on official ENEMDU
#' microdata with documented filters, weights, domains, and poverty-line inputs.
#'
#' @param estimates A data frame returned by `enemdu_kpi_income_poverty()` or an
#' equivalent table.
#' @param benchmarks Official benchmark table. Defaults to
#' `enemdu_official_poverty_benchmarks()`.
#' @param period Optional benchmark period filter.
#' @param benchmark_set Optional benchmark set filter.
#' @param domain_vars Optional package estimate domain variables. The first
#' variable is used as the benchmark `domain_type`.
#' @param domain_map Optional named character vector mapping package domain
#' values to benchmark domain values.
#' @param estimate_col Estimate column in `estimates`.
#' @param indicator_col Indicator id column in `estimates`.
#' @param tolerance_pp Tolerance in percentage points.
#' @param strict Logical. If `TRUE`, errors when a comparison is outside
#' tolerance or missing on either side.
#'
#' @return A tibble with comparison status and difference metadata.
#' @export
#'
#' @examples
#' estimates <- tibble::tibble(
#'   indicator_id = c("pobreza_ingresos", "pobreza_extrema_ingresos"),
#'   estimate = c(0.214, 0.083)
#' )
#'
#' enemdu_compare_official_poverty(
#'   estimates,
#'   period = "2025-12",
#'   benchmark_set = "income_poverty_december_2025"
#' )
enemdu_compare_official_poverty <- function(estimates,
                                            benchmarks = enemdu_official_poverty_benchmarks(),
                                            period = NULL,
                                            benchmark_set = NULL,
                                            domain_vars = NULL,
                                            domain_map = NULL,
                                            estimate_col = "estimate",
                                            indicator_col = "indicator_id",
                                            tolerance_pp = 0.10,
                                            strict = FALSE) {
  if (!is.data.frame(estimates)) {
    .enemdu_abort_invalid_data(caller = "enemdu_compare_official_poverty")
  }

  if (!is.data.frame(benchmarks)) {
    rlang::abort(
      message = "`benchmarks` must be a data frame in `enemdu_compare_official_poverty()`.",
      class = c("enemdu_error_invalid_official_poverty_benchmarks", "enemdu_error")
    )
  }

  .enemdu_validate_official_poverty_benchmarks(benchmarks)
  benchmarks <- .enemdu_normalize_official_poverty_benchmarks(benchmarks)

  .enemdu_abort_missing_vars(
    vars = c(indicator_col, estimate_col),
    names_data = names(estimates),
    caller = "enemdu_compare_official_poverty"
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

  estimate_work <- .enemdu_prepare_poverty_estimates_for_comparison(
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

  .enemdu_abort_duplicate_poverty_comparison_keys(
    estimate_work,
    side = "package"
  )
  .enemdu_abort_duplicate_poverty_comparison_keys(
    benchmark_work,
    side = "official"
  )

  comparison <- merge(
    benchmark_work,
    estimate_work,
    by = c("indicator_id", "domain_type", "domain_value"),
    all = TRUE,
    sort = FALSE
  )

  comparison[["estimate_percent"]] <- comparison[["package_estimate"]] * 100
  comparison[["difference"]] <- comparison[["package_estimate"]] - comparison[["official_estimate"]]
  comparison[["difference_pp"]] <- comparison[["estimate_percent"]] - comparison[["official_percent"]]
  comparison[["abs_difference_pp"]] <- abs(comparison[["difference_pp"]])
  comparison[["tolerance_pp"]] <- tolerance_pp
  comparison[["comparison_status"]] <- .enemdu_official_poverty_comparison_status(
    comparison = comparison,
    tolerance_pp = tolerance_pp
  )
  comparison[["comparison_validation_status"]] <- "comparison_only_not_official_validation"
  comparison[["comparison_note"]] <- paste(
    "Analytical comparison against published poverty benchmarks.",
    "This output is not an official package validation claim."
  )

  front_cols <- c(
    "indicator_id",
    "domain_type",
    "domain_value",
    "period",
    "benchmark_set",
    "survey_type",
    "package_estimate",
    "official_estimate",
    "estimate_percent",
    "official_percent",
    "difference",
    "difference_pp",
    "abs_difference_pp",
    "tolerance_pp",
    "comparison_status",
    "comparison_validation_status",
    "comparison_note"
  )

  comparison <- comparison[c(front_cols, setdiff(names(comparison), front_cols))]
  comparison <- tibble::as_tibble(comparison)
  class(comparison) <- unique(c("enemdu_official_poverty_comparison", class(comparison)))

  attr(comparison, "comparison_policy") <- list(
    tolerance_pp = tolerance_pp,
    strict = strict,
    period = period,
    benchmark_set = benchmark_set,
    note = paste(
      "Comparison uses published official poverty benchmarks.",
      "It does not imply official validation without reviewed reproducibility evidence."
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
        "Official poverty comparison found {sum(bad, na.rm = TRUE)} issue(s)."
      ),
      class = c("enemdu_error_official_poverty_comparison_mismatch", "enemdu_error"),
      comparison = comparison
    )
  }

  comparison
}

.enemdu_validate_official_poverty_benchmarks <- function(benchmarks) {
  required_cols <- c(
    "period",
    "survey_type",
    "benchmark_set",
    "indicator_id",
    "domain_type",
    "domain_value",
    "official_estimate",
    "official_percent",
    "poverty_line_value",
    "extreme_poverty_line_value",
    "currency",
    "source_institution",
    "source_title",
    "source_period",
    "source_page",
    "source_note",
    "official_validation_status"
  )

  missing_cols <- setdiff(required_cols, names(benchmarks))

  if (length(missing_cols) > 0) {
    rlang::abort(
      message = glue::glue(
        "Official poverty benchmark registry is missing columns: ",
        "{paste(missing_cols, collapse = ', ')}."
      ),
      class = c("enemdu_error_invalid_official_poverty_benchmarks", "enemdu_error")
    )
  }

  invisible(TRUE)
}

.enemdu_normalize_official_poverty_benchmarks <- function(benchmarks) {
  benchmarks <- tibble::as_tibble(benchmarks)

  numeric_cols <- c(
    "official_estimate",
    "official_percent",
    "poverty_line_value",
    "extreme_poverty_line_value"
  )

  for (col in numeric_cols) {
    benchmarks[[col]] <- suppressWarnings(as.numeric(benchmarks[[col]]))
  }

  character_cols <- setdiff(names(benchmarks), numeric_cols)

  for (col in character_cols) {
    benchmarks[[col]] <- as.character(benchmarks[[col]])
  }

  benchmarks
}

.enemdu_prepare_poverty_estimates_for_comparison <- function(estimates,
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
      caller = "enemdu_compare_official_poverty"
    )

    raw_domain_value <- as.character(estimates[[domain_var]])
    domain_type <- rep(domain_var, nrow(estimates))
    domain_value <- .enemdu_map_poverty_domain_values(
      values = raw_domain_value,
      domain_map = domain_map
    )
  }

  tibble::tibble(
    indicator_id = as.character(estimates[[indicator_col]]),
    domain_type = domain_type,
    domain_value = domain_value,
    package_estimate = suppressWarnings(as.numeric(estimates[[estimate_col]])),
    package_estimate_present = TRUE
  )
}

.enemdu_map_poverty_domain_values <- function(values, domain_map = NULL) {
  values <- as.character(values)

  if (is.null(domain_map)) {
    return(values)
  }

  if (is.null(names(domain_map))) {
    return(values)
  }

  mapped <- unname(domain_map[values])
  missing_map <- is.na(mapped)
  mapped[missing_map] <- values[missing_map]
  as.character(mapped)
}

.enemdu_abort_duplicate_poverty_comparison_keys <- function(x, side) {
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
        "Duplicated {side} poverty comparison keys were found. ",
        "Filter to one estimate or benchmark row per indicator and domain."
      ),
      class = c("enemdu_error_duplicate_official_poverty_comparison_key", "enemdu_error")
    )
  }

  invisible(TRUE)
}

.enemdu_official_poverty_comparison_status <- function(comparison,
                                                       tolerance_pp) {
  out <- rep(NA_character_, nrow(comparison))

  benchmark_missing <- is.na(comparison$official_estimate)
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
  out[within_tolerance] <- "within_tolerance"
  out[outside_tolerance] <- "outside_tolerance"

  out
}
