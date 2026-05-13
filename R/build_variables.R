#' Build derived ENEMDU variables
#'
#' Builds the phase-3 core derived variables for ENEMDU analytical workflows:
#'
#' - household size (`hsize`),
#' - main labor income (`ingr`),
#' - secondary labor income (`ingrls`),
#' - total labor income (`ingrl`),
#' - individual total income before household aggregation (`ingrltot`),
#' - household total income (`ingtot`),
#' - household per-capita income (`ingtot_pc`).
#'
#' The function deliberately does not derive poverty or extreme poverty flags.
#' Poverty classification is handled by `enemdu_build_poverty_flags()` because it
#' requires explicit and auditable poverty-line parameters.
#'
#' @param data A data frame.
#' @param household_id Household identifier. Defaults to `"idhogar"` and falls
#' back to `"id_hogar"` when needed.
#' @param normalize_missing Logical. If `TRUE`, applies
#' `enemdu_normalize_missing_values()` to registered income components before
#' deriving income variables.
#' @param missing_applies_to Missing-code registry scope. Defaults to
#' `"income_derivation"`.
#' @param keep_raw_missing Logical passed to `enemdu_normalize_missing_values()`.
#' @param create_missing_flags Logical passed to
#' `enemdu_normalize_missing_values()`.
#' @param missing_vars_absent One of `"warn_as_na"` or `"error"`. If
#' `"warn_as_na"`, absent optional income variables are treated as all-missing
#' during income construction.
#' @param existing One of `"backup"`, `"overwrite"` or `"error"` for existing
#' output variables.
#' @param backup_suffix Suffix used when `existing = "backup"`.
#'
#' @return A data frame with derived variables added and derivation metadata in
#' attributes.
#' @export
enemdu_build_variables <- function(data,
                                   household_id = "idhogar",
                                   normalize_missing = TRUE,
                                   missing_applies_to = "income_derivation",
                                   keep_raw_missing = TRUE,
                                   create_missing_flags = TRUE,
                                   missing_vars_absent = c("warn_as_na", "error"),
                                   existing = c("backup", "overwrite", "error"),
                                   backup_suffix = "_source") {
  if (!is.data.frame(data)) {
    .enemdu_abort_invalid_data(caller = "enemdu_build_variables")
  }

  missing_vars_absent <- match.arg(missing_vars_absent)
  existing <- match.arg(existing)

  out <- data

  resolved_household_id <- .enemdu_resolve_household_id(
    data = out,
    household_id = household_id,
    caller = "enemdu_build_variables"
  )

  income_source_vars <- .enemdu_income_source_vars()
  present_income_vars <- intersect(income_source_vars, names(out))
  absent_income_vars <- setdiff(income_source_vars, names(out))

  if (length(absent_income_vars) > 0 && identical(missing_vars_absent, "error")) {
    .enemdu_abort_missing_vars(
      vars = income_source_vars,
      names_data = names(out),
      caller = "enemdu_build_variables"
    )
  }

  if (length(absent_income_vars) > 0 && identical(missing_vars_absent, "warn_as_na")) {
    rlang::warn(
      message = glue::glue(
        "Some income source variables are absent and will be treated as missing in `enemdu_build_variables()`: ",
        "{paste(absent_income_vars, collapse = ', ')}."
      ),
      class = c("enemdu_warning_absent_income_vars", "enemdu_warning")
    )
  }

  if (isTRUE(normalize_missing) && length(present_income_vars) > 0) {
    out <- enemdu_normalize_missing_values(
      data = out,
      vars = present_income_vars,
      applies_to = missing_applies_to,
      keep_raw = keep_raw_missing,
      create_flags = create_missing_flags,
      strict = FALSE
    )
  }

  output_vars <- c(
    "hsize",
    "ingr",
    "ingrls",
    "ingrl",
    "ingrltot",
    "ingtot",
    "ingtot_pc"
  )

  out <- .enemdu_prepare_output_vars(
    data = out,
    vars = output_vars,
    existing = existing,
    backup_suffix = backup_suffix,
    caller = "enemdu_build_variables"
  )

  out <- enemdu_build_hsize(
    data = out,
    household_id = resolved_household_id,
    hsize_name = "hsize",
    overwrite = TRUE
  )

  p63 <- .enemdu_numeric_or_na(out, "p63")
  p64b <- .enemdu_numeric_or_na(out, "p64b")
  p65 <- .enemdu_numeric_or_na(out, "p65")
  p66 <- .enemdu_numeric_or_na(out, "p66")
  p67 <- .enemdu_numeric_or_na(out, "p67")
  p68b <- .enemdu_numeric_or_na(out, "p68b")
  p69 <- .enemdu_numeric_or_na(out, "p69")
  p70b <- .enemdu_numeric_or_na(out, "p70b")

  p71a <- .enemdu_numeric_or_na(out, "p71a")
  p71b <- .enemdu_numeric_or_na(out, "p71b")
  p72a <- .enemdu_numeric_or_na(out, "p72a")
  p72b <- .enemdu_numeric_or_na(out, "p72b")
  p73a <- .enemdu_numeric_or_na(out, "p73a")
  p73b <- .enemdu_numeric_or_na(out, "p73b")
  p74a <- .enemdu_numeric_or_na(out, "p74a")
  p74b <- .enemdu_numeric_or_na(out, "p74b")
  p75 <- .enemdu_numeric_or_na(out, "p75")
  p76 <- .enemdu_numeric_or_na(out, "p76")
  p78 <- .enemdu_numeric_or_na(out, "p78")

  out[["ingr"]] <- .enemdu_build_main_labor_income(
    p63 = p63,
    p64b = p64b,
    p65 = p65,
    p66 = p66,
    p67 = p67,
    p68b = p68b
  )

  out[["ingrls"]] <- .enemdu_build_secondary_labor_income(
    p69 = p69,
    p70b = p70b
  )

  out[["ingrl"]] <- .enemdu_build_total_labor_income(
    ingr = out[["ingr"]],
    ingrls = out[["ingrls"]]
  )

  out[["ingrltot"]] <- .enemdu_build_individual_total_income(
    ingrl = out[["ingrl"]],
    p71a = p71a,
    p71b = p71b,
    p72a = p72a,
    p72b = p72b,
    p73a = p73a,
    p73b = p73b,
    p74a = p74a,
    p74b = p74b,
    p75 = p75,
    p76 = p76,
    p78 = p78
  )

  out[["ingtot"]] <- .enemdu_group_sum(
    x = out[["ingrltot"]],
    group = out[[resolved_household_id]],
    all_missing_value = 0
  )

  out[["ingtot_pc"]] <- out[["ingtot"]] / out[["hsize"]]
  out[["ingtot_pc"]][is.na(out[["ingtot_pc"]]) | out[["ingtot_pc"]] <= 0] <- NA_real_

  attr(out, "income_derivation") <- list(
    household_id = resolved_household_id,
    hsize = "hsize",
    outputs = output_vars,
    absent_income_vars = absent_income_vars,
    normalize_missing = normalize_missing,
    missing_applies_to = missing_applies_to,
    note = paste(
      "Phase-3 income derivation builds household per-capita income.",
      "Poverty flags are intentionally not derived here because they require",
      "validated poverty-line parameters."
    )
  )

  class(out) <- unique(c("enemdu_tbl", class(out)))
  out
}

#' Build ENEMDU quintiles
#'
#' Builds weighted or unweighted quantile groups for an income variable.
#' The default target is household per-capita income (`ingtot_pc`) and the
#' default weight is `fexp`.
#'
#' @param data A data frame.
#' @param income_var Income variable used to build quintiles.
#' @param weight Weight variable. If absent and `use_weights = TRUE`, the
#' function errors.
#' @param quintile_var Output variable name.
#' @param n Number of groups. Defaults to `5`.
#' @param use_weights Logical. If `TRUE`, builds weighted quantile cut points.
#' @param overwrite Logical. If `TRUE`, overwrites an existing output variable.
#'
#' @return A data frame with the quintile variable added.
#' @export
enemdu_build_quintiles <- function(data,
                                   income_var = "ingtot_pc",
                                   weight = "fexp",
                                   quintile_var = "quintil_ingreso_pc",
                                   n = 5,
                                   use_weights = TRUE,
                                   overwrite = FALSE) {
  if (!is.data.frame(data)) {
    .enemdu_abort_invalid_data(caller = "enemdu_build_quintiles")
  }

  .enemdu_abort_missing_vars(
    vars = income_var,
    names_data = names(data),
    caller = "enemdu_build_quintiles"
  )

  if (!is.numeric(data[[income_var]])) {
    .enemdu_abort_invalid_numeric_var(
      var = income_var,
      caller = "enemdu_build_quintiles"
    )
  }

  if (quintile_var %in% names(data) && !isTRUE(overwrite)) {
    rlang::abort(
      message = glue::glue(
        "Variable `{quintile_var}` already exists. ",
        "Use `overwrite = TRUE` to replace it."
      ),
      class = c("enemdu_error_existing_quintile_var", "enemdu_error")
    )
  }

  if (!is.numeric(n) || length(n) != 1 || is.na(n) || n < 2) {
    rlang::abort(
      message = "`n` must be a single numeric value greater than or equal to 2 in `enemdu_build_quintiles()`.",
      class = c("enemdu_error_invalid_quantile_n", "enemdu_error")
    )
  }

  n <- as.integer(n)

  x <- data[[income_var]]
  valid <- !is.na(x) & x > 0

  w <- rep(1, length(x))

  if (isTRUE(use_weights)) {
    .enemdu_abort_missing_vars(
      vars = weight,
      names_data = names(data),
      caller = "enemdu_build_quintiles"
    )

    if (!is.numeric(data[[weight]])) {
      .enemdu_abort_invalid_numeric_var(
        var = weight,
        caller = "enemdu_build_quintiles"
      )
    }

    w <- data[[weight]]
    valid <- valid & !is.na(w) & w > 0
  }

  out <- data
  q <- rep(NA_integer_, length(x))

  if (sum(valid) > 0) {
    probs <- seq(1 / n, (n - 1) / n, by = 1 / n)
    cuts <- .enemdu_weighted_quantile(
      x = x[valid],
      w = w[valid],
      probs = probs
    )

    unique_cuts <- sort(unique(cuts[!is.na(cuts)]))

    if (length(unique_cuts) == 0) {
      q[valid] <- 1L
    } else {
      q[valid] <- as.integer(findInterval(x[valid], vec = unique_cuts) + 1L)
      q[valid] <- pmin(q[valid], n)
    }
  }

  out[[quintile_var]] <- q

  attr(out, "quintile_derivation") <- list(
    income_var = income_var,
    weight = if (isTRUE(use_weights)) weight else NA_character_,
    quintile_var = quintile_var,
    n = n,
    use_weights = use_weights,
    note = "Quantile groups are built only for positive, non-missing income values."
  )

  class(out) <- unique(c("enemdu_tbl", class(out)))
  out
}

#' Build ENEMDU household profile
#'
#' Creates a one-row-per-household profile from a person-level ENEMDU data frame.
#' If `hsize` is not present, it is built automatically.
#'
#' @param data A data frame.
#' @param household_id Household identifier. Defaults to `"idhogar"` and falls
#' back to `"id_hogar"` when needed.
#' @param hsize Household-size variable.
#' @param include_income Logical. If `TRUE`, includes `ingtot` and `ingtot_pc`
#' when available.
#' @param include_weight Logical. If `TRUE`, includes the first observed `fexp`
#' value per household when available.
#'
#' @return A tibble with one row per household.
#' @export
enemdu_build_household_profile <- function(data,
                                           household_id = "idhogar",
                                           hsize = "hsize",
                                           include_income = TRUE,
                                           include_weight = TRUE) {
  if (!is.data.frame(data)) {
    .enemdu_abort_invalid_data(caller = "enemdu_build_household_profile")
  }

  resolved_household_id <- .enemdu_resolve_household_id(
    data = data,
    household_id = household_id,
    caller = "enemdu_build_household_profile"
  )

  out_data <- data

  if (!hsize %in% names(out_data)) {
    out_data <- enemdu_build_hsize(
      data = out_data,
      household_id = resolved_household_id,
      hsize_name = hsize,
      overwrite = FALSE
    )
  }

  household_values <- out_data[[resolved_household_id]]
  valid_household <- !is.na(household_values)
  household_keys <- unique(household_values[valid_household])

  profile <- tibble::tibble(
    household_id = household_keys
  )

  profile[[hsize]] <- .enemdu_first_by_group(
    x = out_data[[hsize]],
    group = household_values,
    keys = household_keys
  )

  profile[["n_records"]] <- profile[[hsize]]

  if (isTRUE(include_weight) && "fexp" %in% names(out_data)) {
    profile[["fexp_first"]] <- .enemdu_first_by_group(
      x = out_data[["fexp"]],
      group = household_values,
      keys = household_keys
    )
  }

  if (isTRUE(include_income)) {
    if ("ingtot" %in% names(out_data)) {
      profile[["ingtot"]] <- .enemdu_first_by_group(
        x = out_data[["ingtot"]],
        group = household_values,
        keys = household_keys
      )
    }

    if ("ingtot_pc" %in% names(out_data)) {
      profile[["ingtot_pc"]] <- .enemdu_first_by_group(
        x = out_data[["ingtot_pc"]],
        group = household_values,
        keys = household_keys
      )
    }
  }

  attr(profile, "household_id_variable") <- resolved_household_id
  class(profile) <- unique(c("enemdu_household_profile", class(profile)))
  profile
}

.enemdu_income_source_vars <- function() {
  c(
    "p63",
    "p64b",
    "p65",
    "p66",
    "p67",
    "p68b",
    "p69",
    "p70b",
    "p71a",
    "p71b",
    "p72a",
    "p72b",
    "p73a",
    "p73b",
    "p74a",
    "p74b",
    "p75",
    "p76",
    "p78"
  )
}

.enemdu_prepare_output_vars <- function(data,
                                        vars,
                                        existing = c("backup", "overwrite", "error"),
                                        backup_suffix = "_source",
                                        caller = "enemdu_internal") {
  existing <- match.arg(existing)
  out <- data

  present <- intersect(vars, names(out))

  if (length(present) == 0) {
    return(out)
  }

  if (identical(existing, "error")) {
    rlang::abort(
      message = glue::glue(
        "Output variables already exist in `{caller}()`: {paste(present, collapse = ', ')}."
      ),
      class = c("enemdu_error_existing_output_vars", "enemdu_error")
    )
  }

  if (identical(existing, "backup")) {
    backups <- character(length(present))

    for (i in seq_along(present)) {
      var <- present[[i]]
      backup_name <- .enemdu_unique_name(
        base_name = paste0(var, backup_suffix),
        existing_names = names(out)
      )

      out[[backup_name]] <- out[[var]]
      backups[[i]] <- backup_name
    }

    attr(out, "backed_up_output_vars") <- tibble::tibble(
      original_var = present,
      backup_var = backups,
      caller = caller
    )
  }

  out
}

.enemdu_unique_name <- function(base_name, existing_names) {
  if (!base_name %in% existing_names) {
    return(base_name)
  }

  i <- 1L
  candidate <- paste0(base_name, "_", i)

  while (candidate %in% existing_names) {
    i <- i + 1L
    candidate <- paste0(base_name, "_", i)
  }

  candidate
}

.enemdu_numeric_or_na <- function(data, var) {
  n <- nrow(data)

  if (!var %in% names(data)) {
    return(rep(NA_real_, n))
  }

  x <- data[[var]]

  if (inherits(x, "haven_labelled")) {
    x <- haven::zap_labels(x)
  }

  if (is.factor(x)) {
    x <- as.character(x)
  }

  suppressWarnings(as.numeric(x))
}

.enemdu_build_main_labor_income <- function(p63, p64b, p65, p66, p67, p68b) {
  components <- cbind(p63, p64b, p65, p66, p67, p68b)
  all_missing <- apply(is.na(components), 1, all)

  total <- .enemdu_row_sum_na0(p63) +
    .enemdu_row_sum_na0(p64b) -
    .enemdu_row_sum_na0(p65) +
    .enemdu_row_sum_na0(p66) +
    .enemdu_row_sum_na0(p67) +
    .enemdu_row_sum_na0(p68b)

  total[all_missing] <- NA_real_
  total
}

.enemdu_build_secondary_labor_income <- function(p69, p70b) {
  components <- cbind(p69, p70b)
  all_missing <- apply(is.na(components), 1, all)

  total <- .enemdu_row_sum_na0(p69) +
    .enemdu_row_sum_na0(p70b)

  total[all_missing] <- NA_real_
  total
}

.enemdu_build_total_labor_income <- function(ingr, ingrls) {
  out <- rep(NA_real_, length(ingr))

  valid_ingr <- !is.na(ingr)
  valid_ingrls <- !is.na(ingrls)

  out[valid_ingr & ingr < 0 & valid_ingrls] <- ingrls[valid_ingr & ingr < 0 & valid_ingrls]

  out[valid_ingr & ingr >= 0 & valid_ingrls] <-
    ingr[valid_ingr & ingr >= 0 & valid_ingrls] +
    ingrls[valid_ingr & ingr >= 0 & valid_ingrls]

  out[!valid_ingr & valid_ingrls] <- ingrls[!valid_ingr & valid_ingrls]

  out[valid_ingr & !valid_ingrls & ingr > 0] <- ingr[valid_ingr & !valid_ingrls & ingr > 0]

  out[valid_ingr & !valid_ingrls & ingr < 0] <- -1

  out
}

.enemdu_build_individual_total_income <- function(ingrl,
                                                  p71a,
                                                  p71b,
                                                  p72a,
                                                  p72b,
                                                  p73a,
                                                  p73b,
                                                  p74a,
                                                  p74b,
                                                  p75,
                                                  p76,
                                                  p78) {
  out <- rep(0, length(ingrl))

  out <- out + .enemdu_add_if_yes(amount = p71b, condition = p71a)
  out <- out + .enemdu_add_if_yes(amount = p72b, condition = p72a)
  out <- out + .enemdu_add_if_yes(amount = p73b, condition = p73a)
  out <- out + .enemdu_add_if_yes(amount = p74b, condition = p74a)
  out <- out + .enemdu_add_if_yes(amount = p76, condition = p75)

  valid_labor <- !is.na(ingrl) & ingrl > -1
  out[valid_labor] <- out[valid_labor] + ingrl[valid_labor]

  no_nonlabor_or_transfer <- is.na(p71b) &
    is.na(p72b) &
    is.na(p73b) &
    is.na(p74b) &
    is.na(p76) &
    is.na(p78)

  invalid_labor_for_total <- is.na(ingrl) | ingrl == -1

  out[no_nonlabor_or_transfer & invalid_labor_for_total] <- NA_real_
  out[is.na(ingrl) & out == 0] <- NA_real_
  out[out == 0] <- NA_real_

  out
}

.enemdu_row_sum_na0 <- function(x) {
  ifelse(is.na(x), 0, x)
}

.enemdu_add_if_yes <- function(amount, condition) {
  out <- rep(0, length(amount))
  idx <- !is.na(condition) & condition == 1 & !is.na(amount)
  out[idx] <- amount[idx]
  out
}

.enemdu_weighted_quantile <- function(x, w, probs) {
  valid <- !is.na(x) & !is.na(w) & w > 0

  x <- x[valid]
  w <- w[valid]

  if (length(x) == 0) {
    return(rep(NA_real_, length(probs)))
  }

  ord <- order(x)
  x <- x[ord]
  w <- w[ord]

  cw <- cumsum(w) / sum(w)

  vapply(
    probs,
    function(p) {
      idx <- which(cw >= p)[1]
      if (is.na(idx)) {
        x[length(x)]
      } else {
        x[idx]
      }
    },
    numeric(1)
  )
}

.enemdu_first_by_group <- function(x, group, keys) {
  out <- vector(mode = typeof(x), length = length(keys))

  for (i in seq_along(keys)) {
    idx <- which(group == keys[[i]])
    out[[i]] <- x[[idx[[1]]]]
  }

  out
}
