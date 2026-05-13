#' Build optional bonus variables
#'
#' Builds optional bonus variables such as `p78` without altering the base
#' official income construction. This function is intended for analytical
#' scenarios where transfer or bonus amounts captured by ENEMDU are studied
#' separately or used to build alternative income scenarios.
#'
#' By default, registered sentinel codes are kept as missing, while structural
#' missing values are treated as zero for optional amount totals. This default is
#' useful for transfer-amount analysis but is explicitly recorded in attributes.
#'
#' @param data A data frame.
#' @param bonus_vars Character vector of optional bonus variables. Defaults to
#' `"p78"`.
#' @param registry Optional bonus registry.
#' @param household_id Household identifier. Defaults to `"idhogar"` and falls
#' back to `"id_hogar"` when needed.
#' @param hsize Household-size variable. If absent and scenario income is
#' requested, it is built.
#' @param base_individual_income_var Base individual income variable used to
#' build the alternative scenario. Defaults to `"ingrltot"`.
#' @param output_total_var Output variable for row-level optional bonus total.
#' @param output_recipient_var Output variable for row-level bonus recipient flag.
#' @param create_income_scenario Logical. If `TRUE`, creates alternative income
#' variables: individual, household total, and household per-capita income with
#' optional bonuses added.
#' @param scenario_suffix Suffix for alternative income variables.
#' @param missing_amount_as_zero Logical. If `TRUE`, structural missing amounts
#' not identified as sentinel nonresponse are treated as zero.
#' @param keep_raw Logical. If `TRUE`, keeps raw copies of source bonus variables.
#' @param create_flags Logical. If `TRUE`, creates missing-code audit flags.
#' @param overwrite Logical. If `TRUE`, overwrites existing output variables.
#' @param strict Logical. If `TRUE`, errors when requested bonus variables are
#' absent. If `FALSE`, missing bonus variables are skipped with a warning.
#'
#' @return A data frame with optional bonus variables and scenario income
#' variables when requested.
#' @export
enemdu_build_optional_bonuses <- function(data,
                                          bonus_vars = "p78",
                                          registry = enemdu_optional_bonus_registry(),
                                          household_id = "idhogar",
                                          hsize = "hsize",
                                          base_individual_income_var = "ingrltot",
                                          output_total_var = "bonos_optional_total",
                                          output_recipient_var = "bonos_optional_recibe",
                                          create_income_scenario = TRUE,
                                          scenario_suffix = "_plus_optional_bonos",
                                          missing_amount_as_zero = TRUE,
                                          keep_raw = TRUE,
                                          create_flags = TRUE,
                                          overwrite = FALSE,
                                          strict = FALSE) {
  if (!is.data.frame(data)) {
    .enemdu_abort_invalid_data(caller = "enemdu_build_optional_bonuses")
  }

  .enemdu_validate_optional_bonus_registry(registry)

  if (is.null(bonus_vars) || length(bonus_vars) == 0) {
    .enemdu_abort_missing_argument(
      "bonus_vars",
      caller = "enemdu_build_optional_bonuses"
    )
  }

  registry_rows <- registry[registry$variable %in% bonus_vars, , drop = FALSE]

  if (nrow(registry_rows) == 0) {
    rlang::abort(
      message = glue::glue(
        "None of the requested bonus variables were found in `optional_bonus_registry.csv`: ",
        "{paste(bonus_vars, collapse = ', ')}."
      ),
      class = c("enemdu_error_invalid_bonus_vars", "enemdu_error")
    )
  }

  absent_vars <- setdiff(registry_rows$variable, names(data))

  if (length(absent_vars) > 0 && isTRUE(strict)) {
    .enemdu_abort_missing_vars(
      vars = registry_rows$variable,
      names_data = names(data),
      caller = "enemdu_build_optional_bonuses"
    )
  }

  if (length(absent_vars) > 0 && !isTRUE(strict)) {
    rlang::warn(
      message = glue::glue(
        "The following optional bonus variables are registered but absent in the data and will be skipped: ",
        "{paste(absent_vars, collapse = ', ')}."
      ),
      class = c("enemdu_warning_absent_optional_bonus_vars", "enemdu_warning")
    )
  }

  registry_rows <- registry_rows[registry_rows$variable %in% names(data), , drop = FALSE]

  if (nrow(registry_rows) == 0) {
    attr(data, "optional_bonus_policy") <- list(
      requested_bonus_vars = bonus_vars,
      applied_bonus_vars = character(0),
      note = "No optional bonus variables were available in the data."
    )
    return(data)
  }

  out <- data

  output_vars <- c(
    registry_rows$output_variable,
    registry_rows$recipient_variable,
    output_total_var,
    output_recipient_var
  )

  if (isTRUE(create_income_scenario)) {
    output_vars <- c(
      output_vars,
      paste0(base_individual_income_var, scenario_suffix),
      paste0("ingtot", scenario_suffix),
      paste0("ingtot_pc", scenario_suffix)
    )
  }

  existing_outputs <- intersect(output_vars, names(out))

  if (length(existing_outputs) > 0 && !isTRUE(overwrite)) {
    rlang::abort(
      message = glue::glue(
        "Optional bonus output variables already exist: ",
        "{paste(existing_outputs, collapse = ', ')}. ",
        "Use `overwrite = TRUE` to replace them."
      ),
      class = c("enemdu_error_existing_optional_bonus_outputs", "enemdu_error")
    )
  }

  bonus_output_vars <- character(nrow(registry_rows))
  bonus_missing_flag_vars <- character(nrow(registry_rows))

  for (i in seq_len(nrow(registry_rows))) {
    row <- registry_rows[i, , drop = FALSE]
    source_var <- row$variable[[1]]
    output_var <- row$output_variable[[1]]
    recipient_var <- row$recipient_variable[[1]]
    missing_codes <- .enemdu_parse_pipe_values(row$missing_codes[[1]])

    x <- .enemdu_optional_bonus_numeric(out[[source_var]])
    x_chr <- .enemdu_values_as_character(out[[source_var]])
    sentinel <- !is.na(x_chr) & x_chr %in% missing_codes

    amount <- x
    amount[sentinel] <- NA_real_

    if (isTRUE(missing_amount_as_zero)) {
      amount[is.na(amount) & !sentinel] <- 0
    }

    amount[!is.na(amount) & amount < 0] <- NA_real_

    if (isTRUE(keep_raw)) {
      raw_var <- paste0(output_var, "_raw")
      out[[raw_var]] <- out[[source_var]]
    }

    if (isTRUE(create_flags)) {
      out[[paste0(output_var, "_missing_flag")]] <- sentinel
      out[[paste0(output_var, "_missing_code")]] <- ifelse(sentinel, x_chr, NA_character_)
      out[[paste0(output_var, "_missing_type")]] <- ifelse(sentinel, "sentinel_nonresponse", NA_character_)
      bonus_missing_flag_vars[[i]] <- paste0(output_var, "_missing_flag")
    } else {
      bonus_missing_flag_vars[[i]] <- NA_character_
    }

    out[[output_var]] <- amount

    recipient <- rep(NA_integer_, length(amount))
    recipient[!is.na(amount)] <- as.integer(amount[!is.na(amount)] > 0)
    out[[recipient_var]] <- recipient

    bonus_output_vars[[i]] <- output_var
  }

  bonus_matrix <- as.data.frame(out[bonus_output_vars], stringsAsFactors = FALSE)
  any_sentinel <- rep(FALSE, nrow(out))

  if (isTRUE(create_flags)) {
    flag_matrix <- as.data.frame(out[bonus_missing_flag_vars], stringsAsFactors = FALSE)
    any_sentinel <- rowSums(flag_matrix, na.rm = TRUE) > 0
  }

  total <- rowSums(bonus_matrix, na.rm = TRUE)

  if (!isTRUE(missing_amount_as_zero)) {
    all_missing <- rowSums(!is.na(bonus_matrix)) == 0
    total[all_missing] <- NA_real_
  }

  total[any_sentinel] <- NA_real_

  out[[output_total_var]] <- total

  recipient_total <- rep(NA_integer_, length(total))
  recipient_total[!is.na(total)] <- as.integer(total[!is.na(total)] > 0)
  out[[output_recipient_var]] <- recipient_total

  if (isTRUE(create_income_scenario)) {
    .enemdu_abort_missing_vars(
      vars = base_individual_income_var,
      names_data = names(out),
      caller = "enemdu_build_optional_bonuses"
    )

    if (!is.numeric(out[[base_individual_income_var]])) {
      .enemdu_abort_invalid_numeric_var(
        var = base_individual_income_var,
        caller = "enemdu_build_optional_bonuses"
      )
    }

    resolved_household_id <- .enemdu_resolve_household_id(
      data = out,
      household_id = household_id,
      caller = "enemdu_build_optional_bonuses"
    )

    if (!hsize %in% names(out)) {
      out <- enemdu_build_hsize(
        data = out,
        household_id = resolved_household_id,
        hsize_name = hsize,
        overwrite = FALSE
      )
    }

    base_income <- out[[base_individual_income_var]]
    individual_scenario <- base_income

    usable_total <- !is.na(total)

    individual_scenario[usable_total] <- ifelse(
      is.na(base_income[usable_total]),
      0,
      base_income[usable_total]
    ) + total[usable_total]

    individual_scenario[is.na(base_income) & usable_total & total == 0] <- NA_real_

    individual_scenario_var <- paste0(base_individual_income_var, scenario_suffix)
    household_scenario_var <- paste0("ingtot", scenario_suffix)
    household_pc_scenario_var <- paste0("ingtot_pc", scenario_suffix)

    out[[individual_scenario_var]] <- individual_scenario

    out[[household_scenario_var]] <- .enemdu_group_sum(
      x = out[[individual_scenario_var]],
      group = out[[resolved_household_id]],
      all_missing_value = 0
    )

    out[[household_pc_scenario_var]] <- out[[household_scenario_var]] / out[[hsize]]
    out[[household_pc_scenario_var]][
      is.na(out[[household_pc_scenario_var]]) |
        out[[household_pc_scenario_var]] <= 0
    ] <- NA_real_
  }

  attr(out, "optional_bonus_policy") <- list(
    requested_bonus_vars = bonus_vars,
    applied_bonus_vars = registry_rows$variable,
    bonus_output_vars = bonus_output_vars,
    total_var = output_total_var,
    recipient_var = output_recipient_var,
    create_income_scenario = create_income_scenario,
    missing_amount_as_zero = missing_amount_as_zero,
    note = paste(
      "Optional bonus variables are not included in the base official income by default.",
      "They are built as a separate analytical layer and may be used for alternative poverty scenarios",
      "by passing the generated per-capita scenario income variable to `enemdu_build_poverty_flags()`."
    )
  )

  class(out) <- unique(c("enemdu_tbl", class(out)))
  out
}

.enemdu_validate_optional_bonus_registry <- function(registry) {
  required_cols <- c(
    "variable",
    "output_variable",
    "recipient_variable",
    "bonus_label",
    "bonus_group",
    "analysis_level",
    "default_income_inclusion",
    "optional_income_inclusion",
    "missing_codes",
    "missing_amount_as_zero_default",
    "source_status",
    "source_note",
    "notes"
  )

  missing_cols <- setdiff(required_cols, names(registry))

  if (length(missing_cols) > 0) {
    .enemdu_abort_invalid_registry(
      registry_name = "optional_bonus_registry",
      message = glue::glue("Missing columns: {paste(missing_cols, collapse = ', ')}."),
      caller = ".enemdu_validate_optional_bonus_registry"
    )
  }

  invisible(TRUE)
}

.enemdu_optional_bonus_numeric <- function(x) {
  if (inherits(x, "haven_labelled")) {
    x <- haven::zap_labels(x)
  }

  if (is.factor(x)) {
    x <- as.character(x)
  }

  suppressWarnings(as.numeric(x))
}
