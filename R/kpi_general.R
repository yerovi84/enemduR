#' Compute general ENEMDU KPIs
#'
#' Produces a minimal set of general KPIs using stable output columns:
#'
#' - estimated persons,
#' - sample records,
#' - estimated households when a household identifier is available,
#' - sample households when a household identifier is available,
#' - mean household per-capita income when available,
#' - optional bonus total and recipients when optional-bonus variables are
#' available.
#'
#' These KPIs are weighted descriptive estimates. Full precision assessment
#' must be added by the representativity module.
#'
#' @param data A data frame.
#' @param weight Weight variable. Defaults to `"fexp"`.
#' @param household_id Household identifier. Defaults to `"idhogar"` and falls
#' back to `"id_hogar"` when needed.
#' @param income_var Household per-capita income variable.
#' @param optional_bonus_total_var Optional bonus total variable.
#' @param optional_bonus_recipient_var Optional bonus recipient variable.
#' @param sample_n_min Minimum unweighted sample size for preliminary flag.
#'
#' @return A tibble with KPI rows.
#' @export
enemdu_kpi_general <- function(data,
                               weight = "fexp",
                               household_id = "idhogar",
                               income_var = "ingtot_pc",
                               optional_bonus_total_var = "bonos_optional_total",
                               optional_bonus_recipient_var = "bonos_optional_recibe",
                               sample_n_min = 60) {
  if (!is.data.frame(data)) {
    .enemdu_abort_invalid_data(caller = "enemdu_kpi_general")
  }

  .enemdu_abort_missing_vars(
    vars = weight,
    names_data = names(data),
    caller = "enemdu_kpi_general"
  )

  if (!is.numeric(data[[weight]])) {
    .enemdu_abort_invalid_numeric_var(
      var = weight,
      caller = "enemdu_kpi_general"
    )
  }

  w <- data[[weight]]
  valid_w <- !is.na(w) & w > 0

  rows <- list()
  i <- 1L

  rows[[i]] <- .enemdu_kpi_row(
    indicator_id = "conteo_personas",
    indicator_label = "Personas estimadas",
    unit = "personas",
    analysis_unit = "person",
    universe = "registros_con_factor_expansion_valido",
    estimate = sum(w[valid_w], na.rm = TRUE),
    weighted_n = sum(w[valid_w], na.rm = TRUE),
    unweighted_n = sum(valid_w),
    quality_flag = .enemdu_sample_quality_flag(sum(valid_w), sample_n_min),
    warning_flag = "precision_not_evaluated",
    source_note = "Estimaci\u00f3n ponderada b\u00e1sica usando factor de expansi\u00f3n."
  )
  i <- i + 1L

  rows[[i]] <- .enemdu_kpi_row(
    indicator_id = "registros_muestrales",
    indicator_label = "Registros muestrales",
    unit = "registros",
    analysis_unit = "person",
    universe = "base_completa",
    estimate = nrow(data),
    weighted_n = NA_real_,
    unweighted_n = nrow(data),
    quality_flag = .enemdu_sample_quality_flag(nrow(data), sample_n_min),
    warning_flag = "descriptive_sample_count",
    source_note = "Conteo no ponderado de registros en la base."
  )
  i <- i + 1L

  household_available <- .enemdu_household_id_available(data, household_id)

  if (isTRUE(household_available)) {
    resolved_household_id <- .enemdu_resolve_household_id(
      data = data,
      household_id = household_id,
      caller = "enemdu_kpi_general"
    )

    hh_profile <- .enemdu_household_weight_profile(
      data = data,
      household_id = resolved_household_id,
      weight = weight
    )

    rows[[i]] <- .enemdu_kpi_row(
      indicator_id = "conteo_hogares",
      indicator_label = "Hogares estimados",
      unit = "hogares",
      analysis_unit = "household",
      universe = "hogares_con_factor_expansion_valido",
      estimate = sum(hh_profile$weight_first, na.rm = TRUE),
      weighted_n = sum(hh_profile$weight_first, na.rm = TRUE),
      unweighted_n = nrow(hh_profile),
      quality_flag = .enemdu_sample_quality_flag(nrow(hh_profile), sample_n_min),
      warning_flag = "household_estimate_from_first_record_weight",
      source_note = paste(
        "Estimaci\u00f3n preliminar de hogares usando el primer factor de expansi\u00f3n",
        "observado por hogar. Requiere validaci\u00f3n metodol\u00f3gica para uso oficial."
      )
    )
    i <- i + 1L

    rows[[i]] <- .enemdu_kpi_row(
      indicator_id = "hogares_muestrales",
      indicator_label = "Hogares muestrales",
      unit = "hogares",
      analysis_unit = "household",
      universe = "hogares_identificados",
      estimate = nrow(hh_profile),
      weighted_n = NA_real_,
      unweighted_n = nrow(hh_profile),
      quality_flag = .enemdu_sample_quality_flag(nrow(hh_profile), sample_n_min),
      warning_flag = "descriptive_sample_count",
      source_note = "Conteo no ponderado de hogares identificados."
    )
    i <- i + 1L
  }

  if (income_var %in% names(data)) {
    if (!is.numeric(data[[income_var]])) {
      .enemdu_abort_invalid_numeric_var(
        var = income_var,
        caller = "enemdu_kpi_general"
      )
    }

    income <- data[[income_var]]
    valid_income <- valid_w & !is.na(income) & income > 0

    rows[[i]] <- .enemdu_kpi_row(
      indicator_id = "ingreso_percapita_familiar_promedio",
      indicator_label = "Ingreso per c\u00e1pita familiar promedio",
      unit = "usd",
      analysis_unit = "household_repeated_person",
      universe = "personas_en_hogares_con_ingreso_percapita_valido",
      estimate = .enemdu_weighted_mean(income, w, valid_income),
      weighted_n = sum(w[valid_income], na.rm = TRUE),
      unweighted_n = sum(valid_income),
      quality_flag = .enemdu_sample_quality_flag(sum(valid_income), sample_n_min),
      warning_flag = "precision_not_evaluated",
      source_note = "Media ponderada b\u00e1sica. La precisi\u00f3n debe evaluarse con dise\u00f1o muestral."
    )
    i <- i + 1L

    rows[[i]] <- .enemdu_kpi_row(
      indicator_id = "personas_con_ingreso_percapita_valido",
      indicator_label = "Personas con ingreso per c\u00e1pita familiar v\u00e1lido",
      unit = "personas",
      analysis_unit = "person",
      universe = "personas_en_hogares_con_ingreso_percapita_valido",
      estimate = sum(w[valid_income], na.rm = TRUE),
      weighted_n = sum(w[valid_income], na.rm = TRUE),
      unweighted_n = sum(valid_income),
      quality_flag = .enemdu_sample_quality_flag(sum(valid_income), sample_n_min),
      warning_flag = "precision_not_evaluated",
      source_note = "Conteo ponderado de registros con ingreso per c\u00e1pita positivo y no faltante."
    )
    i <- i + 1L
  }

  if (optional_bonus_total_var %in% names(data)) {
    if (!is.numeric(data[[optional_bonus_total_var]])) {
      .enemdu_abort_invalid_numeric_var(
        var = optional_bonus_total_var,
        caller = "enemdu_kpi_general"
      )
    }

    bonus <- data[[optional_bonus_total_var]]
    valid_bonus <- valid_w & !is.na(bonus)

    rows[[i]] <- .enemdu_kpi_row(
      indicator_id = "transferencias_bonos_total_enemdu",
      indicator_label = "Monto total de bonos opcionales captado por ENEMDU",
      unit = "usd",
      analysis_unit = "person",
      universe = "personas_con_monto_de_bonos_observable",
      estimate = sum(bonus[valid_bonus] * w[valid_bonus], na.rm = TRUE),
      weighted_n = sum(w[valid_bonus], na.rm = TRUE),
      unweighted_n = sum(valid_bonus),
      quality_flag = .enemdu_sample_quality_flag(sum(valid_bonus), sample_n_min),
      warning_flag = "survey_estimate_not_administrative_execution",
      source_note = paste(
        "Estimaci\u00f3n ponderada de montos declarados/captados por ENEMDU.",
        "No debe interpretarse autom\u00e1ticamente como ejecuci\u00f3n fiscal o registro administrativo."
      )
    )
    i <- i + 1L
  }

  if (optional_bonus_recipient_var %in% names(data)) {
    if (!is.numeric(data[[optional_bonus_recipient_var]])) {
      .enemdu_abort_invalid_numeric_var(
        var = optional_bonus_recipient_var,
        caller = "enemdu_kpi_general"
      )
    }

    recipient <- data[[optional_bonus_recipient_var]]
    valid_recipient <- valid_w & !is.na(recipient)

    rows[[i]] <- .enemdu_kpi_row(
      indicator_id = "bono_jgl_receptores",
      indicator_label = "Personas con monto positivo de bono opcional",
      unit = "personas",
      analysis_unit = "person",
      universe = "personas_con_variable_de_bono_observable",
      estimate = sum(recipient[valid_recipient] * w[valid_recipient], na.rm = TRUE),
      weighted_n = sum(w[valid_recipient], na.rm = TRUE),
      unweighted_n = sum(valid_recipient),
      quality_flag = .enemdu_sample_quality_flag(sum(valid_recipient), sample_n_min),
      warning_flag = "survey_estimate_not_administrative_register",
      source_note = "Estimaci\u00f3n ponderada de receptores con monto positivo observado en ENEMDU."
    )
    i <- i + 1L
  }

  out <- do.call(rbind, rows)
  row.names(out) <- NULL

  out <- tibble::as_tibble(out)
  class(out) <- unique(c("enemdu_kpi_table", class(out)))
  out
}

#' Compute ENEMDU household KPIs
#'
#' Produces minimal household-level KPIs from an ENEMDU person-level data frame.
#' This is a descriptive household profile helper and does not yet replace full
#' survey-design estimation.
#'
#' @param data A data frame.
#' @param household_id Household identifier. Defaults to `"idhogar"`.
#' @param weight Weight variable. Defaults to `"fexp"`.
#' @param hsize Household-size variable.
#' @param income_var Optional per-capita household income variable.
#' @param sample_n_min Minimum unweighted sample size for preliminary flag.
#'
#' @return A tibble with household KPI rows.
#' @export
enemdu_kpi_households <- function(data,
                                  household_id = "idhogar",
                                  weight = "fexp",
                                  hsize = "hsize",
                                  income_var = "ingtot_pc",
                                  sample_n_min = 60) {
  if (!is.data.frame(data)) {
    .enemdu_abort_invalid_data(caller = "enemdu_kpi_households")
  }

  resolved_household_id <- .enemdu_resolve_household_id(
    data = data,
    household_id = household_id,
    caller = "enemdu_kpi_households"
  )

  profile <- enemdu_build_household_profile(
    data = data,
    household_id = resolved_household_id,
    hsize = hsize,
    include_income = income_var %in% names(data),
    include_weight = weight %in% names(data)
  )

  rows <- list()
  i <- 1L

  rows[[i]] <- .enemdu_kpi_row(
    indicator_id = "hogares_muestrales",
    indicator_label = "Hogares muestrales",
    unit = "hogares",
    analysis_unit = "household",
    universe = "hogares_identificados",
    estimate = nrow(profile),
    weighted_n = NA_real_,
    unweighted_n = nrow(profile),
    quality_flag = .enemdu_sample_quality_flag(nrow(profile), sample_n_min),
    warning_flag = "descriptive_sample_count",
    source_note = "Conteo no ponderado de hogares identificados."
  )
  i <- i + 1L

  if ("fexp_first" %in% names(profile)) {
    valid_w <- !is.na(profile$fexp_first) & profile$fexp_first > 0

    rows[[i]] <- .enemdu_kpi_row(
      indicator_id = "hogares_estimados",
      indicator_label = "Hogares estimados",
      unit = "hogares",
      analysis_unit = "household",
      universe = "hogares_con_factor_expansion_valido",
      estimate = sum(profile$fexp_first[valid_w], na.rm = TRUE),
      weighted_n = sum(profile$fexp_first[valid_w], na.rm = TRUE),
      unweighted_n = sum(valid_w),
      quality_flag = .enemdu_sample_quality_flag(sum(valid_w), sample_n_min),
      warning_flag = "household_estimate_from_first_record_weight",
      source_note = paste(
        "Estimaci\u00f3n preliminar de hogares usando el primer factor de expansi\u00f3n",
        "observado por hogar. Requiere validaci\u00f3n metodol\u00f3gica para uso oficial."
      )
    )
    i <- i + 1L
  }

  if (hsize %in% names(profile)) {
    rows[[i]] <- .enemdu_kpi_row(
      indicator_id = "tamano_promedio_hogar_muestral",
      indicator_label = "Tama\u00f1o promedio del hogar muestral",
      unit = "personas_por_hogar",
      analysis_unit = "household",
      universe = "hogares_identificados",
      estimate = mean(profile[[hsize]], na.rm = TRUE),
      weighted_n = NA_real_,
      unweighted_n = sum(!is.na(profile[[hsize]])),
      quality_flag = .enemdu_sample_quality_flag(sum(!is.na(profile[[hsize]])), sample_n_min),
      warning_flag = "unweighted_household_descriptive",
      source_note = "Promedio no ponderado calculado sobre perfil de hogares."
    )
    i <- i + 1L
  }

  if (income_var %in% names(profile)) {
    valid_income <- !is.na(profile[[income_var]]) & profile[[income_var]] > 0

    if ("fexp_first" %in% names(profile)) {
      valid_income <- valid_income & !is.na(profile$fexp_first) & profile$fexp_first > 0
      estimate <- .enemdu_weighted_mean(
        x = profile[[income_var]],
        w = profile$fexp_first,
        valid = valid_income
      )
      weighted_n <- sum(profile$fexp_first[valid_income], na.rm = TRUE)
    } else {
      estimate <- mean(profile[[income_var]][valid_income], na.rm = TRUE)
      weighted_n <- NA_real_
    }

    rows[[i]] <- .enemdu_kpi_row(
      indicator_id = "ingreso_percapita_promedio_hogar",
      indicator_label = "Ingreso per c\u00e1pita familiar promedio por hogar",
      unit = "usd",
      analysis_unit = "household",
      universe = "hogares_con_ingreso_percapita_valido",
      estimate = estimate,
      weighted_n = weighted_n,
      unweighted_n = sum(valid_income),
      quality_flag = .enemdu_sample_quality_flag(sum(valid_income), sample_n_min),
      warning_flag = "precision_not_evaluated",
      source_note = "Estimaci\u00f3n descriptiva sobre perfil de hogares."
    )
    i <- i + 1L
  }

  out <- do.call(rbind, rows)
  row.names(out) <- NULL

  out <- tibble::as_tibble(out)
  class(out) <- unique(c("enemdu_kpi_table", class(out)))
  out
}

.enemdu_kpi_row <- function(indicator_id,
                            indicator_label,
                            unit,
                            analysis_unit,
                            universe,
                            estimate,
                            weighted_n = NA_real_,
                            unweighted_n = NA_integer_,
                            standard_error = NA_real_,
                            cv = NA_real_,
                            quality_flag = "not_evaluated",
                            warning_flag = "precision_not_evaluated",
                            source_note = NA_character_) {
  tibble::tibble(
    indicator_id = indicator_id,
    indicator_label = indicator_label,
    unit = unit,
    analysis_unit = analysis_unit,
    universe = universe,
    domain_type = NA_character_,
    domain_value = NA_character_,
    estimate = as.numeric(estimate),
    weighted_n = as.numeric(weighted_n),
    unweighted_n = as.integer(unweighted_n),
    standard_error = as.numeric(standard_error),
    cv = as.numeric(cv),
    quality_flag = quality_flag,
    warning_flag = warning_flag,
    source_note = source_note
  )
}

.enemdu_weighted_mean <- function(x, w, valid) {
  if (!any(valid, na.rm = TRUE)) {
    return(NA_real_)
  }

  if (sum(w[valid], na.rm = TRUE) <= 0) {
    return(NA_real_)
  }

  sum(x[valid] * w[valid], na.rm = TRUE) / sum(w[valid], na.rm = TRUE)
}

.enemdu_household_id_available <- function(data, household_id) {
  if (!is.null(household_id) && household_id %in% names(data)) {
    return(TRUE)
  }

  any(c("idhogar", "id_hogar") %in% names(data))
}

.enemdu_household_weight_profile <- function(data, household_id, weight) {
  household_values <- data[[household_id]]
  valid <- !is.na(household_values)

  households <- unique(household_values[valid])

  weight_first <- rep(NA_real_, length(households))

  for (i in seq_along(households)) {
    idx <- which(household_values == households[[i]])
    weight_first[[i]] <- data[[weight]][idx[[1]]]
  }

  tibble::tibble(
    household_id = households,
    weight_first = weight_first
  )
}
