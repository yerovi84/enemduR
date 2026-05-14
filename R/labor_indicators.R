#' Build core ENEMDU labor flags from official consolidated classification
#'
#' Builds binary labor-status flags from the official consolidated ENEMDU
#' condition-of-activity variable. This helper does not reconstruct labor status
#' from raw questionnaire items. It assumes that `condact` already contains the
#' official ENEMDU classification present in the microdata.
#'
#' @param data A data frame.
#' @param condact Condition-of-activity variable. Defaults to `"condact"`.
#' @param age Age variable. Defaults to `"p03"`.
#' @param sector Employment-sector variable. Defaults to `"secemp"`.
#' @param strict Logical. If `TRUE`, invalid observed codes produce an error.
#'
#' @return A tibble/data frame with added binary labor flags.
#' @export
enemdu_build_labor_flags <- function(data,
                                     condact = "condact",
                                     age = "p03",
                                     sector = "secemp",
                                     strict = TRUE) {
  if (!is.data.frame(data)) {
    .enemdu_abort_invalid_data(caller = "enemdu_build_labor_flags")
  }

  .enemdu_abort_missing_vars(
    vars = condact,
    names_data = names(data),
    caller = "enemdu_build_labor_flags"
  )

  .enemdu_validate_labor_code_variable(
    data = data,
    var = condact,
    valid_codes = 0:9,
    strict = strict,
    caller = "enemdu_build_labor_flags"
  )

  if (age %in% names(data) && !is.numeric(data[[age]])) {
    .enemdu_abort_invalid_numeric_var(
      var = age,
      caller = "enemdu_build_labor_flags"
    )
  }

  if (sector %in% names(data)) {
    .enemdu_validate_labor_code_variable(
      data = data,
      var = sector,
      valid_codes = 1:4,
      strict = strict,
      caller = "enemdu_build_labor_flags"
    )
  }

  out <- tibble::as_tibble(data)
  condact_values <- out[[condact]]

  out[["labor_menor_15"]] <- .enemdu_flag_in(condact_values, 0L)

  if (age %in% names(out)) {
    age_values <- out[[age]]
    out[["labor_pet"]] <- ifelse(
      is.na(age_values),
      NA_integer_,
      as.integer(age_values >= 15)
    )
  } else {
    out[["labor_pet"]] <- .enemdu_flag_in(condact_values, 1:9)
  }

  out[["labor_pea"]] <- .enemdu_flag_in(condact_values, 1:8)
  out[["labor_pei"]] <- .enemdu_flag_in(condact_values, 9L)

  out[["labor_empleo"]] <- .enemdu_flag_in(condact_values, 1:6)
  out[["labor_empleo_adecuado"]] <- .enemdu_flag_in(condact_values, 1L)
  out[["labor_subempleo"]] <- .enemdu_flag_in(condact_values, 2:3)
  out[["labor_subempleo_tiempo"]] <- .enemdu_flag_in(condact_values, 2L)
  out[["labor_subempleo_ingresos"]] <- .enemdu_flag_in(condact_values, 3L)
  out[["labor_otro_empleo_no_pleno"]] <- .enemdu_flag_in(condact_values, 4L)
  out[["labor_empleo_no_remunerado"]] <- .enemdu_flag_in(condact_values, 5L)
  out[["labor_empleo_no_clasificado"]] <- .enemdu_flag_in(condact_values, 6L)

  out[["labor_desempleo"]] <- .enemdu_flag_in(condact_values, 7:8)
  out[["labor_desempleo_abierto"]] <- .enemdu_flag_in(condact_values, 7L)
  out[["labor_desempleo_oculto"]] <- .enemdu_flag_in(condact_values, 8L)

  if (sector %in% names(out)) {
    sector_values <- out[[sector]]
    out[["labor_sector_formal"]] <- .enemdu_flag_in(sector_values, 1L)
    out[["labor_sector_informal"]] <- .enemdu_flag_in(sector_values, 2L)
    out[["labor_sector_domestico"]] <- .enemdu_flag_in(sector_values, 3L)
    out[["labor_sector_no_clasificado"]] <- .enemdu_flag_in(sector_values, 4L)
  }

  attr(out, "labor_status_policy") <- list(
    condact = condact,
    age = if (age %in% names(out)) age else NA_character_,
    sector = if (sector %in% names(out)) sector else NA_character_,
    strict = strict,
    note = paste(
      "Labor flags were built from the official consolidated ENEMDU condition-of-activity variable.",
      "This helper does not reconstruct labor status from raw questionnaire items."
    )
  )

  out
}

#' Estimate core ENEMDU employment KPIs using survey design
#'
#' Estimates labor totals and core rates from the official consolidated ENEMDU
#' condition-of-activity variable. The function first builds labor flags with
#' `enemdu_build_labor_flags()` and then estimates totals and rates using the
#' package survey-design estimators.
#'
#' The default `domain_scope = "observed"` preserves observed grouping values in
#' the output. When `domain_scope = "design"`, estimation still uses the full
#' input microdata and only the output rows are filtered to recognized design
#' domains for registered grouping variables. For official city-domain
#' validation against published tabulations, use `dominio` when that derived
#' field is available; `ciudad` is not assumed to be equivalent to the published
#' official city-domain tabulations.
#'
#' @param data A data frame.
#' @param group_vars Optional grouping variables.
#' @param condact Condition-of-activity variable. Defaults to `"condact"`.
#' @param age Age variable. Defaults to `"p03"`.
#' @param sector Employment-sector variable. Defaults to `"secemp"`.
#' @param ids Primary sampling unit variable. Defaults to `"upm"`.
#' @param strata Strata variable. Defaults to `"estrato"`.
#' @param weight Expansion factor variable. Defaults to `"fexp"`.
#' @param survey_type Optional ENEMDU survey type.
#' @param domain_scope Domain filtering policy. `"observed"` returns all observed
#' domains generated by `group_vars`. `"design"` estimates with the complete data
#' and then filters the output to recognized ENEMDU design domains for the
#' declared `survey_type`.
#' @param include_totals Logical. If `TRUE`, estimates labor-population totals.
#' @param include_rates Logical. If `TRUE`, estimates core labor rates.
#' @param strict Logical. If `TRUE`, invalid observed labor codes produce an error.
#' @param conf_level Confidence level.
#' @param lonely_psu Option passed to `survey.lonely.psu`.
#' @param sample_n_min Minimum unweighted sample size for preliminary quality flag.
#'
#' @return A tibble with survey-design employment KPI estimates.
#' @export
enemdu_kpi_employment <- function(data,
                                  group_vars = NULL,
                                  condact = "condact",
                                  age = "p03",
                                  sector = "secemp",
                                  ids = "upm",
                                  strata = "estrato",
                                  weight = "fexp",
                                  survey_type = NULL,
                                  domain_scope = c("observed", "design"),
                                  include_totals = TRUE,
                                  include_rates = TRUE,
                                  strict = TRUE,
                                  conf_level = 0.95,
                                  lonely_psu = "adjust",
                                  sample_n_min = 60) {
  domain_scope <- match.arg(domain_scope)

  if (!is.data.frame(data)) {
    .enemdu_abort_invalid_data(caller = "enemdu_kpi_employment")
  }

  if (!is.null(group_vars)) {
    .enemdu_abort_missing_vars(
      vars = group_vars,
      names_data = names(data),
      caller = "enemdu_kpi_employment"
    )
  }

  .enemdu_abort_missing_vars(
    vars = c(ids, strata, weight),
    names_data = names(data),
    caller = "enemdu_kpi_employment"
  )

  out_data <- enemdu_build_labor_flags(
    data = data,
    condact = condact,
    age = age,
    sector = sector,
    strict = strict
  )

  if (is.null(survey_type)) {
    survey_type <- attr(out_data, "survey_type") %||% NA_character_
  }

  outputs <- list()
  i <- 1L

  if (isTRUE(include_totals)) {
    total_specs <- .enemdu_labor_total_specs(include_sector = sector %in% names(out_data))

    for (spec in total_specs) {
      outputs[[i]] <- .enemdu_labor_total_estimate(
        data = out_data,
        spec = spec,
        group_vars = group_vars,
        ids = ids,
        strata = strata,
        weight = weight,
        survey_type = survey_type,
        conf_level = conf_level,
        lonely_psu = lonely_psu,
        sample_n_min = sample_n_min
      )
      i <- i + 1L
    }
  }

  if (isTRUE(include_rates)) {
    rate_specs <- .enemdu_labor_rate_specs(include_sector = sector %in% names(out_data))

    for (spec in rate_specs) {
      rate_data <- .enemdu_labor_prepare_rate_data(
        data = out_data,
        numerator = spec$numerator,
        denominator = spec$denominator
      )

      outputs[[i]] <- .enemdu_labor_rate_estimate(
        data = rate_data,
        spec = spec,
        group_vars = group_vars,
        ids = ids,
        strata = strata,
        weight = weight,
        survey_type = survey_type,
        conf_level = conf_level,
        lonely_psu = lonely_psu,
        sample_n_min = sample_n_min
      )
      i <- i + 1L
    }
  }

  if (length(outputs) == 0) {
    return(.enemdu_empty_survey_estimate())
  }

  result <- .enemdu_bind_estimate_rows(outputs)
  result <- tibble::as_tibble(result)

  result <- .enemdu_labor_apply_domain_scope(
    result = result,
    survey_type = survey_type,
    group_vars = group_vars,
    domain_scope = domain_scope
  )

  class(result) <- unique(c("enemdu_employment_kpi", class(result)))

  attr(result, "labor_kpi_policy") <- list(
    condact = condact,
    age = if (age %in% names(out_data)) age else NA_character_,
    sector = if (sector %in% names(out_data)) sector else NA_character_,
    domain_scope = domain_scope,
    rate_scale = "proportion_0_1",
    note = paste(
      "Labor KPIs are estimated with ENEMDU survey design variables.",
      "Rates are returned as proportions in [0, 1], not multiplied by 100.",
      "Condition-of-activity flags are based on the consolidated `condact` variable.",
      "If `domain_scope = 'design'`, output rows are filtered after estimation to recognized design-domain codes."
    )
  )

  result
}

.enemdu_labor_total_specs <- function(include_sector = TRUE) {
  specs <- list(
    list("labor_pet_total", "Población en edad de trabajar", "labor_pet", "personas", "personas_15_mas"),
    list("labor_pea_total", "Población económicamente activa", "labor_pea", "personas", "personas_15_mas"),
    list("labor_pei_total", "Población económicamente inactiva", "labor_pei", "personas", "personas_15_mas"),
    list("labor_empleo_total", "Población con empleo", "labor_empleo", "personas", "personas_15_mas"),
    list("labor_empleo_adecuado_total", "Población con empleo adecuado/pleno", "labor_empleo_adecuado", "personas", "pea"),
    list("labor_subempleo_total", "Población en subempleo", "labor_subempleo", "personas", "pea"),
    list("labor_subempleo_tiempo_total", "Población en subempleo por insuficiencia de tiempo", "labor_subempleo_tiempo", "personas", "pea"),
    list("labor_subempleo_ingresos_total", "Población en subempleo por insuficiencia de ingresos", "labor_subempleo_ingresos", "personas", "pea"),
    list("labor_otro_empleo_no_pleno_total", "Población en otro empleo no pleno", "labor_otro_empleo_no_pleno", "personas", "pea"),
    list("labor_empleo_no_remunerado_total", "Población con empleo no remunerado", "labor_empleo_no_remunerado", "personas", "pea"),
    list("labor_empleo_no_clasificado_total", "Población con empleo no clasificado", "labor_empleo_no_clasificado", "personas", "pea"),
    list("labor_desempleo_total", "Población con desempleo", "labor_desempleo", "personas", "pea"),
    list("labor_desempleo_abierto_total", "Población con desempleo abierto", "labor_desempleo_abierto", "personas", "pea"),
    list("labor_desempleo_oculto_total", "Población con desempleo oculto", "labor_desempleo_oculto", "personas", "pea")
  )

  if (isTRUE(include_sector)) {
    specs <- c(
      specs,
      list(
        list("labor_sector_formal_total", "Población con empleo en el sector formal", "labor_sector_formal", "personas", "poblacion_con_empleo"),
        list("labor_sector_informal_total", "Población con empleo en el sector informal", "labor_sector_informal", "personas", "poblacion_con_empleo"),
        list("labor_sector_domestico_total", "Población con empleo doméstico", "labor_sector_domestico", "personas", "poblacion_con_empleo"),
        list("labor_sector_no_clasificado_total", "Población con empleo no clasificado por sector", "labor_sector_no_clasificado", "personas", "poblacion_con_empleo")
      )
    )
  }

  lapply(specs, function(x) {
    list(
      indicator_id = x[[1]],
      label = x[[2]],
      value = x[[3]],
      unit = x[[4]],
      universe = x[[5]]
    )
  })
}

.enemdu_labor_rate_specs <- function(include_sector = TRUE) {
  specs <- list(
    list("labor_tasa_participacion_bruta", "Tasa de participación bruta", "labor_pea", "labor_population_base", "proportion", "poblacion_total_observada"),
    list("labor_tasa_participacion_global", "Tasa de participación global", "labor_pea", "labor_pet", "proportion", "pet"),
    list("labor_tasa_ocupacion_bruta", "Tasa de ocupación bruta", "labor_empleo", "labor_pet", "proportion", "pet"),
    list("labor_tasa_ocupacion_global", "Tasa de ocupación global", "labor_empleo", "labor_pea", "proportion", "pea"),
    list("labor_tasa_empleo_adecuado", "Tasa de empleo adecuado/pleno", "labor_empleo_adecuado", "labor_pea", "proportion", "pea"),
    list("labor_tasa_subempleo", "Tasa de subempleo", "labor_subempleo", "labor_pea", "proportion", "pea"),
    list("labor_tasa_subempleo_tiempo", "Tasa de subempleo por insuficiencia de tiempo", "labor_subempleo_tiempo", "labor_pea", "proportion", "pea"),
    list("labor_tasa_subempleo_ingresos", "Tasa de subempleo por insuficiencia de ingresos", "labor_subempleo_ingresos", "labor_pea", "proportion", "pea"),
    list("labor_tasa_otro_empleo_no_pleno", "Tasa de otro empleo no pleno", "labor_otro_empleo_no_pleno", "labor_pea", "proportion", "pea"),
    list("labor_tasa_empleo_no_remunerado", "Tasa de empleo no remunerado", "labor_empleo_no_remunerado", "labor_pea", "proportion", "pea"),
    list("labor_tasa_empleo_no_clasificado", "Tasa de empleo no clasificado", "labor_empleo_no_clasificado", "labor_pea", "proportion", "pea"),
    list("labor_tasa_desempleo", "Tasa de desempleo", "labor_desempleo", "labor_pea", "proportion", "pea"),
    list("labor_tasa_desempleo_abierto", "Tasa de desempleo abierto", "labor_desempleo_abierto", "labor_pea", "proportion", "pea"),
    list("labor_tasa_desempleo_oculto", "Tasa de desempleo oculto", "labor_desempleo_oculto", "labor_pea", "proportion", "pea")
  )

  lapply(specs, function(x) {
    list(
      indicator_id = x[[1]],
      label = x[[2]],
      numerator = x[[3]],
      denominator = x[[4]],
      unit = x[[5]],
      universe = x[[6]]
    )
  })
}

.enemdu_labor_total_estimate <- function(data,
                                         spec,
                                         group_vars,
                                         ids,
                                         strata,
                                         weight,
                                         survey_type,
                                         conf_level,
                                         lonely_psu,
                                         sample_n_min) {
  estimate <- enemdu_survey_total(
    data = data,
    value = spec$value,
    group_vars = group_vars,
    ids = ids,
    strata = strata,
    weight = weight,
    survey_type = survey_type,
    indicator_id = spec$indicator_id,
    measure = spec$label,
    conf_level = conf_level,
    lonely_psu = lonely_psu,
    sample_n_min = sample_n_min
  )

  .enemdu_labor_annotate_estimate(
    estimate = estimate,
    indicator_label = spec$label,
    indicator_group = "labor",
    unit = spec$unit,
    analysis_level = "person",
    universe = spec$universe,
    value_var = spec$value,
    method_note_extra = "Total estimated from a binary labor-status flag."
  )
}

.enemdu_labor_rate_estimate <- function(data,
                                        spec,
                                        group_vars,
                                        ids,
                                        strata,
                                        weight,
                                        survey_type,
                                        conf_level,
                                        lonely_psu,
                                        sample_n_min) {
  estimate <- enemdu_survey_proportion(
    data = data,
    value = ".enemdu_labor_rate_value",
    group_vars = group_vars,
    ids = ids,
    strata = strata,
    weight = weight,
    survey_type = survey_type,
    indicator_id = spec$indicator_id,
    measure = spec$label,
    conf_level = conf_level,
    lonely_psu = lonely_psu,
    sample_n_min = sample_n_min
  )

  .enemdu_labor_annotate_estimate(
    estimate = estimate,
    indicator_label = spec$label,
    indicator_group = "labor_rate",
    unit = spec$unit,
    analysis_level = "person",
    universe = spec$universe,
    value_var = spec$numerator,
    method_note_extra = paste(
      "Rate estimated as a survey proportion after restricting the denominator universe.",
      "Returned estimate is a proportion in [0, 1]."
    )
  )
}

.enemdu_labor_prepare_rate_data <- function(data,
                                            numerator,
                                            denominator) {
  .enemdu_abort_missing_vars(
    vars = numerator,
    names_data = names(data),
    caller = ".enemdu_labor_prepare_rate_data"
  )

  out <- data

  if (identical(denominator, "labor_population_base")) {
    denominator_ok <- rep(TRUE, nrow(out))
  } else {
    .enemdu_abort_missing_vars(
      vars = denominator,
      names_data = names(out),
      caller = ".enemdu_labor_prepare_rate_data"
    )
    denominator_ok <- !is.na(out[[denominator]]) & out[[denominator]] == 1
  }

  out <- out[denominator_ok, , drop = FALSE]
  out[[".enemdu_labor_rate_value"]] <- out[[numerator]]

  out
}

.enemdu_labor_annotate_estimate <- function(estimate,
                                            indicator_label,
                                            indicator_group,
                                            unit,
                                            analysis_level,
                                            universe,
                                            value_var,
                                            method_note_extra) {
  estimate[["indicator_label"]] <- indicator_label
  estimate[["indicator_group"]] <- indicator_group
  estimate[["unit"]] <- unit
  estimate[["analysis_level"]] <- analysis_level
  estimate[["universe"]] <- universe
  estimate[["registry_value_var"]] <- value_var

  if ("method_note" %in% names(estimate)) {
    estimate[["method_note"]] <- paste(estimate[["method_note"]], method_note_extra)
  } else {
    estimate[["method_note"]] <- method_note_extra
  }

  front_cols <- c(
    "indicator_id",
    "indicator_label",
    "indicator_group",
    "unit",
    "analysis_level",
    "universe",
    "registry_value_var"
  )

  estimate[c(front_cols, setdiff(names(estimate), front_cols))]
}

.enemdu_labor_apply_domain_scope <- function(result,
                                             survey_type,
                                             group_vars,
                                             domain_scope) {
  if (!identical(domain_scope, "design")) {
    attr(result, "labor_design_domain_filter") <- list(
      applied = FALSE,
      reason = "domain_scope_observed"
    )
    return(result)
  }

  if (is.null(group_vars) || length(group_vars) == 0) {
    attr(result, "labor_design_domain_filter") <- list(
      applied = FALSE,
      reason = "no_group_vars"
    )
    return(result)
  }

  specs <- .enemdu_labor_design_domain_specs(survey_type = survey_type)

  if (length(specs) == 0) {
    attr(result, "labor_design_domain_filter") <- list(
      applied = FALSE,
      reason = "survey_type_not_recognized",
      survey_type = survey_type
    )
    return(result)
  }

  applied <- list()

  for (var in intersect(group_vars, names(specs))) {
    if (var %in% names(result)) {
      before_n <- nrow(result)
      valid_codes <- specs[[var]]
      result <- result[as.numeric(result[[var]]) %in% valid_codes, , drop = FALSE]
      applied[[var]] <- list(
        valid_codes = valid_codes,
        rows_before = before_n,
        rows_after = nrow(result)
      )
    }
  }

  attr(result, "labor_design_domain_filter") <- list(
    applied = length(applied) > 0,
    survey_type = survey_type,
    group_vars = group_vars,
    filters = applied
  )

  tibble::as_tibble(result)
}

.enemdu_labor_design_domain_specs <- function(survey_type) {
  survey_type <- as.character(survey_type)[1]

  five_city_codes <- c(
    10150,
    70150,
    90150,
    170150,
    180150
  )

  if (is.na(survey_type)) {
    return(list())
  }

  switch(
    survey_type,
    mensual = list(
      area = c(1, 2)
    ),
    trimestral = list(
      area = c(1, 2),
      ciudad = five_city_codes
    ),
    anual = list(
      area = c(1, 2),
      ciudad = five_city_codes,
      prov = 1:24
    ),
    list()
  )
}

.enemdu_validate_labor_code_variable <- function(data,
                                                 var,
                                                 valid_codes,
                                                 strict,
                                                 caller) {
  if (!var %in% names(data)) {
    return(invisible(FALSE))
  }

  if (!is.numeric(data[[var]])) {
    .enemdu_abort_invalid_numeric_var(var = var, caller = caller)
  }

  observed <- unique(data[[var]][!is.na(data[[var]])])
  invalid <- setdiff(observed, valid_codes)

  if (length(invalid) > 0 && isTRUE(strict)) {
    rlang::abort(
      message = glue::glue(
        "Variable `{var}` contains values outside the declared ENEMDU labor-code contract: ",
        "{paste(sort(invalid), collapse = ', ')}."
      ),
      class = c("enemdu_error_invalid_labor_code", "enemdu_error")
    )
  }

  invisible(TRUE)
}

.enemdu_flag_in <- function(x, codes) {
  ifelse(
    is.na(x),
    NA_integer_,
    as.integer(x %in% codes)
  )
}
