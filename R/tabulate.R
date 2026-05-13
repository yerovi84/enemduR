#' Tabulate ENEMDU data
#'
#' Produces a stable long-format tabulation using optional grouping variables,
#' a value variable, and an optional expansion factor.
#'
#' This function performs basic weighted estimation. It does not yet calculate
#' standard errors, coefficients of variation, effective sample sizes, degrees
#' of freedom, or confidence intervals. Those elements belong to the
#' representativity and precision module.
#'
#' @param data A data frame.
#' @param group_vars Optional character vector of grouping variables.
#' @param value Optional value variable. Required for `"sum"`, `"mean"` and
#' `"proportion"`.
#' @param weight Optional weight variable. Defaults to `"fexp"` if present.
#' If `NULL`, unweighted calculations are performed.
#' @param statistic One of `"count"`, `"sum"`, `"mean"` or `"proportion"`.
#' @param measure Optional name for the output measure.
#' @param na_rm Logical. If `TRUE`, missing values in `value` are excluded from
#' sum, mean, and proportion calculations.
#' @param drop_na_groups Logical. If `TRUE`, records with missing values in any
#' grouping variable are excluded.
#' @param sample_n_min Minimum unweighted sample size used for a preliminary
#' sample-size flag. Defaults to `60`.
#'
#' @return A tibble with one row per tabulated group.
#' @export
enemdu_tabulate <- function(data,
                            group_vars = NULL,
                            value = NULL,
                            weight = "fexp",
                            statistic = c("count", "sum", "mean", "proportion"),
                            measure = NULL,
                            na_rm = TRUE,
                            drop_na_groups = TRUE,
                            sample_n_min = 60) {
  if (!is.data.frame(data)) {
    .enemdu_abort_invalid_data(caller = "enemdu_tabulate")
  }

  statistic <- match.arg(statistic)

  if (!is.null(group_vars)) {
    .enemdu_abort_missing_vars(
      vars = group_vars,
      names_data = names(data),
      caller = "enemdu_tabulate"
    )
  }

  if (!statistic %in% "count") {
    if (is.null(value) || length(value) != 1) {
      .enemdu_abort_missing_argument(
        "value",
        caller = "enemdu_tabulate"
      )
    }

    .enemdu_abort_missing_vars(
      vars = value,
      names_data = names(data),
      caller = "enemdu_tabulate"
    )

    if (!is.numeric(data[[value]])) {
      .enemdu_abort_invalid_numeric_var(
        var = value,
        caller = "enemdu_tabulate"
      )
    }
  }

  use_weight <- !is.null(weight)

  if (isTRUE(use_weight)) {
    .enemdu_abort_missing_vars(
      vars = weight,
      names_data = names(data),
      caller = "enemdu_tabulate"
    )

    if (!is.numeric(data[[weight]])) {
      .enemdu_abort_invalid_numeric_var(
        var = weight,
        caller = "enemdu_tabulate"
      )
    }
  }

  if (is.null(measure) || !nzchar(measure)) {
    measure <- .enemdu_default_measure_name(
      statistic = statistic,
      value = value
    )
  }

  result <- .enemdu_tabulate_core(
    data = data,
    group_vars = group_vars,
    value = value,
    weight = weight,
    statistic = statistic,
    measure = measure,
    na_rm = na_rm,
    drop_na_groups = drop_na_groups,
    sample_n_min = sample_n_min
  )

  class(result) <- unique(c("enemdu_tabulation", class(result)))
  result
}

#' Create two-way ENEMDU tabulations
#'
#' Convenience wrapper around `enemdu_tabulate()` for two-dimensional
#' tabulations.
#'
#' @param data A data frame.
#' @param row_var Row grouping variable.
#' @param col_var Column grouping variable.
#' @param value Optional value variable.
#' @param weight Optional weight variable. Defaults to `"fexp"` if present.
#' @param statistic One of `"count"`, `"sum"`, `"mean"` or `"proportion"`.
#' @param measure Optional name for the output measure.
#' @param na_rm Logical. If `TRUE`, missing values in `value` are excluded.
#' @param drop_na_groups Logical. If `TRUE`, records with missing row/column
#' values are excluded.
#' @param sample_n_min Minimum unweighted sample size for preliminary flag.
#'
#' @return A tibble with one row per row/column combination.
#' @export
enemdu_tabulate_two_way <- function(data,
                                    row_var,
                                    col_var,
                                    value = NULL,
                                    weight = "fexp",
                                    statistic = c("count", "sum", "mean", "proportion"),
                                    measure = NULL,
                                    na_rm = TRUE,
                                    drop_na_groups = TRUE,
                                    sample_n_min = 60) {
  if (missing(row_var) || is.null(row_var) || length(row_var) != 1) {
    .enemdu_abort_missing_argument(
      "row_var",
      caller = "enemdu_tabulate_two_way"
    )
  }

  if (missing(col_var) || is.null(col_var) || length(col_var) != 1) {
    .enemdu_abort_missing_argument(
      "col_var",
      caller = "enemdu_tabulate_two_way"
    )
  }

  enemdu_tabulate(
    data = data,
    group_vars = c(row_var, col_var),
    value = value,
    weight = weight,
    statistic = statistic,
    measure = measure,
    na_rm = na_rm,
    drop_na_groups = drop_na_groups,
    sample_n_min = sample_n_min
  )
}

#' Check basic quality flags for ENEMDU analytical outputs
#'
#' Applies preliminary quality flags to a data frame containing tabulation or KPI
#' outputs. This function is intentionally limited: it checks sample-size and
#' optional CV rules when the relevant columns exist. Full representativity
#' assessment must use the dedicated precision module.
#'
#' @param data A data frame with analytical output columns.
#' @param sample_n_min Minimum unweighted sample size. Defaults to `60`.
#' @param cv_reliable_max CV threshold for reliable estimates. Defaults to
#' `0.15`.
#' @param cv_reduced_max CV threshold for reduced precision. Defaults to `0.30`.
#'
#' @return The input data frame with quality columns updated or added.
#' @export
enemdu_check_quality <- function(data,
                                 sample_n_min = 60,
                                 cv_reliable_max = 0.15,
                                 cv_reduced_max = 0.30) {
  if (!is.data.frame(data)) {
    .enemdu_abort_invalid_data(caller = "enemdu_check_quality")
  }

  out <- data

  if (!"quality_flag" %in% names(out)) {
    out[["quality_flag"]] <- NA_character_
  }

  if (!"warning_flag" %in% names(out)) {
    out[["warning_flag"]] <- NA_character_
  }

  if ("unweighted_n" %in% names(out)) {
    low_sample <- !is.na(out[["unweighted_n"]]) &
      out[["unweighted_n"]] < sample_n_min

    out[["quality_flag"]][low_sample] <- "low_sample_size"
    out[["warning_flag"]][low_sample] <- "sample_size_below_preliminary_threshold"

    ok_sample <- !is.na(out[["unweighted_n"]]) &
      out[["unweighted_n"]] >= sample_n_min &
      is.na(out[["quality_flag"]])

    out[["quality_flag"]][ok_sample] <- "sample_size_ok_precision_not_evaluated"
  }

  if ("cv" %in% names(out)) {
    cv <- out[["cv"]]

    reliable <- !is.na(cv) & cv <= cv_reliable_max
    reduced <- !is.na(cv) & cv > cv_reliable_max & cv <= cv_reduced_max
    not_recommended <- !is.na(cv) & cv > cv_reduced_max

    out[["quality_flag"]][reliable] <- "cv_reliable"
    out[["quality_flag"]][reduced] <- "cv_reduced_precision"
    out[["quality_flag"]][not_recommended] <- "cv_not_recommended"

    out[["warning_flag"]][reduced] <- "use_with_caution_cv"
    out[["warning_flag"]][not_recommended] <- "inference_not_recommended_cv"
  }

  out[["quality_flag"]][is.na(out[["quality_flag"]])] <- "not_evaluated"
  out[["warning_flag"]][is.na(out[["warning_flag"]])] <- "precision_not_evaluated"

  class(out) <- unique(c("enemdu_quality_checked", class(out)))
  out
}

.enemdu_tabulate_core <- function(data,
                                  group_vars,
                                  value,
                                  weight,
                                  statistic,
                                  measure,
                                  na_rm,
                                  drop_na_groups,
                                  sample_n_min) {
  n <- nrow(data)

  w <- rep(1, n)

  if (!is.null(weight)) {
    w <- data[[weight]]
  }

  group_info <- .enemdu_group_index(
    data = data,
    group_vars = group_vars,
    drop_na_groups = drop_na_groups
  )

  if (!is.null(value)) {
    x <- data[[value]]
  } else {
    x <- rep(NA_real_, n)
  }

  pieces <- vector("list", length(group_info$groups))
  i <- 1L

  for (group_name in names(group_info$groups)) {
    idx <- group_info$groups[[group_name]]

    if (length(idx) == 0) {
      next
    }

    x_i <- x[idx]
    w_i <- w[idx]

    valid_weight <- !is.na(w_i) & w_i > 0

    if (identical(statistic, "count")) {
      valid <- valid_weight
      estimate <- sum(w_i[valid], na.rm = TRUE)
    } else {
      valid_value <- !is.na(x_i)

      if (!isTRUE(na_rm)) {
        valid_value <- rep(TRUE, length(x_i))
      }

      valid <- valid_weight & valid_value

      estimate <- .enemdu_estimate_statistic(
        x = x_i[valid],
        w = w_i[valid],
        statistic = statistic
      )
    }

    unweighted_n <- sum(valid, na.rm = TRUE)
    weighted_n <- sum(w_i[valid], na.rm = TRUE)

    group_values <- group_info$group_values[[group_name]]

    row <- tibble::tibble(
      measure = measure,
      statistic = statistic,
      estimate = estimate,
      weighted_n = weighted_n,
      unweighted_n = as.integer(unweighted_n),
      weight = if (is.null(weight)) NA_character_ else weight,
      value = value %||% NA_character_,
      quality_flag = .enemdu_sample_quality_flag(
        unweighted_n = unweighted_n,
        sample_n_min = sample_n_min
      ),
      warning_flag = "precision_not_evaluated"
    )

    if (length(group_values) > 0) {
      group_df <- as.data.frame(
        group_values,
        stringsAsFactors = FALSE
      )
      row <- cbind(group_df, row)
    }

    pieces[[i]] <- row
    i <- i + 1L
  }

  pieces <- pieces[seq_len(i - 1L)]

  if (length(pieces) == 0) {
    return(tibble::tibble(
      measure = character(),
      statistic = character(),
      estimate = numeric(),
      weighted_n = numeric(),
      unweighted_n = integer(),
      weight = character(),
      value = character(),
      quality_flag = character(),
      warning_flag = character()
    ))
  }

  out <- do.call(rbind, pieces)
  row.names(out) <- NULL
  tibble::as_tibble(out)
}

.enemdu_group_index <- function(data,
                                group_vars = NULL,
                                drop_na_groups = TRUE) {
  n <- nrow(data)

  if (is.null(group_vars) || length(group_vars) == 0) {
    groups <- list(total = seq_len(n))
    group_values <- list(total = list())
    return(list(groups = groups, group_values = group_values))
  }

  group_data <- data[group_vars]

  valid_group <- rep(TRUE, n)

  if (isTRUE(drop_na_groups)) {
    valid_group <- stats::complete.cases(group_data)
  }

  valid_idx <- which(valid_group)

  if (length(valid_idx) == 0) {
    return(list(groups = list(), group_values = list()))
  }

  key_data <- group_data[valid_idx, , drop = FALSE]
  key <- do.call(
    paste,
    c(
      lapply(key_data, function(x) {
        x <- as.character(x)
        x[is.na(x)] <- "<NA>"
        x
      }),
      sep = "\r"
    )
  )

  split_idx <- split(valid_idx, key, drop = TRUE)

  group_values <- list()

  for (nm in names(split_idx)) {
    first_row <- split_idx[[nm]][[1]]
    values <- as.list(data[first_row, group_vars, drop = FALSE])
    names(values) <- group_vars
    group_values[[nm]] <- values
  }

  list(
    groups = split_idx,
    group_values = group_values
  )
}

.enemdu_estimate_statistic <- function(x, w, statistic) {
  if (length(x) == 0 || length(w) == 0) {
    return(NA_real_)
  }

  if (sum(w, na.rm = TRUE) <= 0) {
    return(NA_real_)
  }

  if (identical(statistic, "sum")) {
    return(sum(x * w, na.rm = TRUE))
  }

  if (identical(statistic, "mean")) {
    return(sum(x * w, na.rm = TRUE) / sum(w, na.rm = TRUE))
  }

  if (identical(statistic, "proportion")) {
    return(sum(x * w, na.rm = TRUE) / sum(w, na.rm = TRUE))
  }

  NA_real_
}

.enemdu_sample_quality_flag <- function(unweighted_n, sample_n_min = 60) {
  if (is.na(unweighted_n)) {
    return("not_evaluated")
  }

  if (unweighted_n < sample_n_min) {
    return("low_sample_size")
  }

  "sample_size_ok_precision_not_evaluated"
}

.enemdu_default_measure_name <- function(statistic, value) {
  if (identical(statistic, "count")) {
    return("weighted_count")
  }

  paste(statistic, value, sep = "_")
}
