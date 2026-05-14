#' Build validated social bonus variables
#'
#' Builds validated social bonus variables from ENEMDU amount variables and their
#' corresponding receipt questions. The function supports, by registry:
#'
#' - `p75` / `p76`: Bono de Desarrollo Humano
#' - `p77` / `p78`: Bono de Discapacidad
#'
#' The amount is not read as analytically valid unless the corresponding
#' receipt question confirms that the person received the bonus. If the receipt
#' question says the person did not receive the bonus, the amount is treated as
#' structural zero. If the receipt question says the person did receive the bonus
#' but the amount is missing, invalid, or a sentinel code, the amount remains
#' missing because it cannot be safely assumed to be zero.
#'
#' The scenario income variables add only bonuses marked in the registry with
#' `scenario_income_inclusion = TRUE`. This prevents duplicating the Bono de
#' Desarrollo Humano, which is already part of the base income construction
#' through `p75` / `p76`.
#'
#' @param data A data frame.
#' @param bonus_vars Character vector of bonus amount variables. If `NULL`,
#' all registered bonus variables are considered.
#' @param registry Optional bonus registry.
#' @param household_id Household identifier. Defaults to `"idhogar"` and falls
#' back to `"id_hogar"` when needed.
#' @param hsize Household-size variable. If absent and scenario income is
#' requested, it is built.
#' @param base_individual_income_var Base individual income variable used to
#' build the alternative scenario. Defaults to `"ingrltot"`.
#' @param output_total_var Output variable for row-level total social bonuses.
#' @param output_recipient_var Output variable for any social bonus receipt.
#' @param scenario_add_var Output variable containing only bonuses to add to
#' the alternative income scenario.
#' @param create_income_scenario Logical. If `TRUE`, creates alternative income
#' variables: individual, household total, and household per-capita income with
#' non-base bonuses added.
#' @param scenario_suffix Suffix for alternative income variables.
#' @param keep_raw Logical. If `TRUE`, keeps raw copies of source amount
#' variables.
#' @param create_flags Logical. If `TRUE`, creates audit flags for sentinels,
#' receipt validation, and amount/receipt inconsistencies.
#' @param overwrite Logical. If `TRUE`, overwrites existing output variables.
#' @param strict Logical. If `TRUE`, errors when registered amount or receipt
#' variables are absent. If `FALSE`, incomplete pairs are skipped with warning.
#'
#' @return A data frame with validated social bonus variables and scenario
#' income variables when requested.
#' @export
enemdu_build_optional_bonuses <- function(data,
                                          bonus_vars = NULL,
                                          registry = enemdu_optional_bonus_registry(),
                                          household_id = "idhogar",
                                          hsize = "hsize",
                                          base_individual_income_var = "ingrltot",
                                          output_total_var = "bonos_sociales_total",
                                          output_recipient_var = "bonos_sociales_recibe",
                                          scenario_add_var = "bonos_scenario_add_total",
                                          create_income_scenario = TRUE,
                                          scenario_suffix = "_plus_optional_bonos",
                                          keep_raw = TRUE,
                                          create_flags = TRUE,
                                          overwrite = FALSE,
                                          strict = FALSE) {
  if (!is.data.frame(data)) {
    .enemdu_abort_invalid_data(caller = "enemdu_build_optional_bonuses")
  }

  .enemdu_validate_optional_bonus_registry(registry)

  if (is.null(bonus_vars)) {
    bonus_vars <- registry$variable
  }

  if (length(bonus_vars) == 0) {
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

  required_pair_vars <- unique(c(registry_rows$variable, registry_rows$condition_variable))
  absent_pair_vars <- setdiff(required_pair_vars, names(data))

  if (length(absent_pair_vars) > 0 && isTRUE(strict)) {
    .enemdu_abort_missing_vars(
      vars = required_pair_vars,
      names_data = names(data),
      caller = "enemdu_build_optional_bonuses"
    )
  }

  if (length(absent_pair_vars) > 0 && !isTRUE(strict)) {
    rlang::warn(
      message = glue::glue(
        "The following social bonus amount/receipt variables are absent and their pairs will be skipped: ",
        "{paste(absent_pair_vars, collapse = ', ')}."
      ),
      class = c("enemdu_warning_absent_social_bonus_pair_vars", "enemdu_warning")
    )
  }

  registry_rows <- registry_rows[
    registry_rows$variable %in% names(data) &
      registry_rows$condition_variable %in% names(data),
    ,
    drop = FALSE
  ]

  if (nrow(registry_rows) == 0) {
    attr(data, "optional_bonus_policy") <- list(
      requested_bonus_vars = bonus_vars,
      applied_bonus_vars = character(0),
      note = "No complete social bonus amount/receipt pairs were available in the data."
    )
    return(data)
  }

  out <- data

  alias_output_vars <- c(
    "bonos_optional_total",
    "bonos_optional_recibe",
    "bono_jgl",
    "bono_jgl_recibe"
  )

  output_vars <- c(
    registry_rows$output_variable,
    registry_rows$recipient_variable,
    output_total_var,
    output_recipient_var,
    scenario_add_var,
    alias_output_vars
  )

  if (isTRUE(keep_raw)) {
    output_vars <- c(output_vars, paste0(registry_rows$output_variable, "_raw"))
  }

  if (isTRUE(create_flags)) {
    output_vars <- c(
      output_vars,
      paste0(registry_rows$output_variable, "_missing_flag"),
      paste0(registry_rows$output_variable, "_missing_code"),
      paste0(registry_rows$output_variable, "_missing_type"),
      paste0(registry_rows$output_variable, "_receipt_raw"),
      paste0(registry_rows$output_variable, "_receipt_validated_flag"),
      paste0(registry_rows$output_variable, "_amount_without_receipt_flag"),
      paste0(registry_rows$output_variable, "_received_without_valid_amount_flag")
    )
  }

  if (isTRUE(create_income_scenario)) {
    output_vars <- c(
      output_vars,
      paste0(base_individual_income_var, scenario_suffix),
      paste0("ingtot", scenario_suffix),
      paste0("ingtot_pc", scenario_suffix)
    )
  }

  output_vars <- unique(output_vars)
  existing_outputs <- intersect(output_vars, names(out))

  if (length(existing_outputs) > 0 && !isTRUE(overwrite)) {
    rlang::abort(
      message = glue::glue(
        "Social bonus output variables already exist: ",
        "{paste(existing_outputs, collapse = ', ')}. ",
        "Use `overwrite = TRUE` to replace them."
      ),
      class = c("enemdu_error_existing_optional_bonus_outputs", "enemdu_error")
    )
  }

  bonus_output_vars <- character(nrow(registry_rows))
  bonus_recipient_vars <- character(nrow(registry_rows))
  scenario_output_vars <- character(0)
  scenario_recipient_vars <- character(0)

  for (i in seq_len(nrow(registry_rows))) {
    row <- registry_rows[i, , drop = FALSE]

    source_var <- row$variable[[1]]
    condition_var <- row$condition_variable[[1]]
    output_var <- row$output_variable[[1]]
    recipient_var <- row$recipient_variable[[1]]

    missing_codes <- .enemdu_parse_pipe_values(row$missing_codes[[1]])
    yes_values <- .enemdu_parse_pipe_values(row$condition_yes_values[[1]])
    no_values <- .enemdu_parse_pipe_values(row$condition_no_values[[1]])

    amount_raw <- out[[source_var]]
    condition_raw <- out[[condition_var]]

    amount_numeric <- .enemdu_optional_bonus_numeric(amount_raw)
    amount_chr <- .enemdu_values_as_character(amount_raw)
    condition_chr <- .enemdu_values_as_character(condition_raw)

    sentinel <- !is.na(amount_chr) & amount_chr %in% missing_codes
    received_yes <- !is.na(condition_chr) & condition_chr %in% yes_values
    received_no <- !is.na(condition_chr) & condition_chr %in% no_values
    receipt_known <- received_yes | received_no

    amount <- rep(NA_real_, nrow(out))
    amount[received_no] <- 0

    valid_amount <- received_yes &
      !sentinel &
      !is.na(amount_numeric) &
      amount_numeric >= 0

    amount[valid_amount] <- amount_numeric[valid_amount]

    recipient <- rep(NA_integer_, nrow(out))
    recipient[received_yes] <- 1L
    recipient[received_no] <- 0L

    amount_without_receipt <- !received_yes &
      !is.na(amount_numeric) &
      amount_numeric > 0

    received_without_valid_amount <- received_yes &
      (sentinel | is.na(amount_numeric) | amount_numeric < 0)

    if (isTRUE(keep_raw)) {
      out[[paste0(output_var, "_raw")]] <- amount_raw
    }

    if (isTRUE(create_flags)) {
      out[[paste0(output_var, "_missing_flag")]] <- sentinel
      out[[paste0(output_var, "_missing_code")]] <- ifelse(sentinel, amount_chr, NA_character_)
      out[[paste0(output_var, "_missing_type")]] <- ifelse(sentinel, "sentinel_nonresponse", NA_character_)
      out[[paste0(output_var, "_receipt_raw")]] <- condition_raw
      out[[paste0(output_var, "_receipt_validated_flag")]] <- receipt_known
      out[[paste0(output_var, "_amount_without_receipt_flag")]] <- amount_without_receipt
      out[[paste0(output_var, "_received_without_valid_amount_flag")]] <- received_without_valid_amount
    }

    out[[output_var]] <- amount
    out[[recipient_var]] <- recipient

    bonus_output_vars[[i]] <- output_var
    bonus_recipient_vars[[i]] <- recipient_var

    if (isTRUE(.enemdu_as_logical(row$scenario_income_inclusion[[1]]))) {
      scenario_output_vars <- c(scenario_output_vars, output_var)
      scenario_recipient_vars <- c(scenario_recipient_vars, recipient_var)
    }
  }

  social_total <- .enemdu_bonus_total_from_vars(
    data = out,
    amount_vars = bonus_output_vars,
    recipient_vars = bonus_recipient_vars
  )

  social_recipient <- .enemdu_bonus_any_recipient_from_vars(
    data = out,
    recipient_vars = bonus_recipient_vars
  )

  scenario_add_total <- .enemdu_bonus_total_from_vars(
    data = out,
    amount_vars = scenario_output_vars,
    recipient_vars = scenario_recipient_vars,
    empty_value = 0
  )

  out[[output_total_var]] <- social_total
  out[[output_recipient_var]] <- social_recipient
  out[[scenario_add_var]] <- scenario_add_total

  out[["bonos_optional_total"]] <- out[[output_total_var]]
  out[["bonos_optional_recibe"]] <- out[[output_recipient_var]]

  if ("bono_discapacidad" %in% names(out)) {
    out[["bono_jgl"]] <- out[["bono_discapacidad"]]
  }

  if ("bono_discapacidad_recibe" %in% names(out)) {
    out[["bono_jgl_recibe"]] <- out[["bono_discapacidad_recibe"]]
  }

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

    valid_add <- !is.na(out[[scenario_add_var]])
    unknown_add <- is.na(out[[scenario_add_var]])

    individual_scenario[valid_add] <- ifelse(
      is.na(base_income[valid_add]),
      0,
      base_income[valid_add]
    ) + out[[scenario_add_var]][valid_add]

    individual_scenario[is.na(base_income) & valid_add & out[[scenario_add_var]] == 0] <- NA_real_
    individual_scenario[unknown_add] <- NA_real_

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
    bonus_recipient_vars = bonus_recipient_vars,
    total_var = output_total_var,
    recipient_var = output_recipient_var,
    scenario_add_var = scenario_add_var,
    scenario_output_vars = scenario_output_vars,
    create_income_scenario = create_income_scenario,
    note = paste(
      "Social bonus variables are validated against their receipt questions.",
      "Amounts are not read as valid unless receipt is confirmed.",
      "Scenario income adds only bonuses not included in the base income."
    )
  )

  class(out) <- unique(c("enemdu_tbl", class(out)))
  out
}

#' Estimate validated social bonus KPIs using survey design
#'
#' Builds or consumes validated social bonus variables and estimates key bonus
#' indicators through `enemdu_indicator_estimate()`. This function is the
#' preferred social-bonus KPI interface after the p75/p76 and p77/p78 contract.
#'
#' @param data A data frame.
#' @param group_vars Optional grouping variables.
#' @param build_if_missing Logical. If `TRUE`, calls
#' `enemdu_build_optional_bonuses()` when validated social bonus variables are
#' missing.
#' @param registry Indicator registry.
#' @param ... Additional arguments passed to `enemdu_indicator_estimate()`.
#'
#' @return A tibble of social bonus KPI estimates.
#' @export
enemdu_kpi_social_bonuses <- function(data,
                                      group_vars = NULL,
                                      build_if_missing = TRUE,
                                      registry = enemdu_indicator_registry(),
                                      ...) {
  if (!is.data.frame(data)) {
    .enemdu_abort_invalid_data(caller = "enemdu_kpi_social_bonuses")
  }

  out_data <- data

  needed <- c(
    "bonos_sociales_total",
    "bonos_sociales_recibe",
    "bono_desarrollo_humano",
    "bono_desarrollo_humano_recibe",
    "bono_discapacidad",
    "bono_discapacidad_recibe",
    "bonos_scenario_add_total"
  )

  if (!all(needed %in% names(out_data)) && isTRUE(build_if_missing)) {
    available_pair_vars <- c("p75", "p76", "p77", "p78")
    if (any(available_pair_vars %in% names(out_data))) {
      out_data <- enemdu_build_optional_bonuses(
        data = out_data,
        create_income_scenario = FALSE,
        strict = FALSE
      )
    }
  }

  if (all(c("bono_desarrollo_humano", "bono_desarrollo_humano_recibe") %in% names(out_data))) {
    out_data[["bono_desarrollo_humano_monto_receptor"]] <- ifelse(
      !is.na(out_data[["bono_desarrollo_humano_recibe"]]) &
        out_data[["bono_desarrollo_humano_recibe"]] == 1,
      out_data[["bono_desarrollo_humano"]],
      NA_real_
    )
  }

  if (all(c("bono_discapacidad", "bono_discapacidad_recibe") %in% names(out_data))) {
    out_data[["bono_discapacidad_monto_receptor"]] <- ifelse(
      !is.na(out_data[["bono_discapacidad_recibe"]]) &
        out_data[["bono_discapacidad_recibe"]] == 1,
      out_data[["bono_discapacidad"]],
      NA_real_
    )
  }

  indicator_values <- c(
    transferencias_bonos_total_enemdu = "bonos_sociales_total",
    bono_desarrollo_humano_monto_total = "bono_desarrollo_humano",
    bono_desarrollo_humano_receptores = "bono_desarrollo_humano_recibe",
    bono_desarrollo_humano_monto_promedio_receptor = "bono_desarrollo_humano_monto_receptor",
    bono_discapacidad_monto_total = "bono_discapacidad",
    bono_discapacidad_receptores = "bono_discapacidad_recibe",
    bono_discapacidad_monto_promedio_receptor = "bono_discapacidad_monto_receptor",
    bonos_scenario_add_total_enemdu = "bonos_scenario_add_total"
  )

  outputs <- list()
  i <- 1L

  for (indicator_id in names(indicator_values)) {
    value_var <- unname(indicator_values[[indicator_id]])

    if (!value_var %in% names(out_data)) {
      next
    }

    outputs[[i]] <- enemdu_indicator_estimate(
      data = out_data,
      indicator_id = indicator_id,
      group_vars = group_vars,
      registry = registry,
      value = value_var,
      scale_adjustment = "never",
      ...
    )

    i <- i + 1L
  }

  if (length(outputs) == 0) {
    return(.enemdu_empty_indicator_estimate())
  }

  result <- .enemdu_bind_estimate_rows(outputs)

  attr(result, "social_bonus_kpi_policy") <- list(
    build_if_missing = build_if_missing,
    note = paste(
      "Social bonus estimates are survey-based estimates from ENEMDU.",
      "Amounts are validated against receipt questions.",
      "They must not be interpreted directly as administrative execution."
    )
  )

  result
}

.enemdu_validate_optional_bonus_registry <- function(registry) {
  required_cols <- c(
    "variable",
    "condition_variable",
    "condition_yes_values",
    "condition_no_values",
    "output_variable",
    "recipient_variable",
    "bonus_label",
    "bonus_group",
    "analysis_level",
    "default_income_inclusion",
    "optional_income_inclusion",
    "scenario_income_inclusion",
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

.enemdu_bonus_total_from_vars <- function(data,
                                          amount_vars,
                                          recipient_vars,
                                          empty_value = NA_real_) {
  if (length(amount_vars) == 0 || length(recipient_vars) == 0) {
    return(rep(empty_value, nrow(data)))
  }

  amount_df <- as.data.frame(data[amount_vars], stringsAsFactors = FALSE)
  recipient_df <- as.data.frame(data[recipient_vars], stringsAsFactors = FALSE)

  amount_mat <- as.matrix(amount_df)
  recipient_mat <- as.matrix(recipient_df)

  unknown_received <- is.na(amount_mat) & recipient_mat == 1
  any_unknown_received <- rowSums(unknown_received, na.rm = TRUE) > 0

  total <- rowSums(ifelse(is.na(amount_mat), 0, amount_mat), na.rm = TRUE)
  total[any_unknown_received] <- NA_real_

  total
}

.enemdu_bonus_any_recipient_from_vars <- function(data,
                                                  recipient_vars) {
  if (length(recipient_vars) == 0) {
    return(rep(NA_integer_, nrow(data)))
  }

  recipient_df <- as.data.frame(data[recipient_vars], stringsAsFactors = FALSE)
  recipient_mat <- as.matrix(recipient_df)

  any_yes <- rowSums(recipient_mat == 1, na.rm = TRUE) > 0
  any_known <- rowSums(!is.na(recipient_mat), na.rm = TRUE) > 0

  out <- rep(NA_integer_, nrow(data))
  out[any_known] <- 0L
  out[any_yes] <- 1L

  out
}
