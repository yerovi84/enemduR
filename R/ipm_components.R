#' Build registered IPM component indicators
#'
#' Builds the IPM component columns that are auditable in the current package
#' contract and marks the remaining registered components as pending. The
#' current operational rules cover school attendance, incomplete education,
#' inadequate employment from consolidated labor flags, precomputed extreme
#' income poverty, public-network water, overcrowding, excreta sanitation, and
#' garbage collection for the `enemdu_2025_anual` profile.
#'
#' This function does not invent unsupported rules. Access to higher education
#' for economic reasons, child and adolescent labor, pension contribution, and
#' housing deficit remain pending unless supplied as accepted precomputed
#' component columns. When `strict = TRUE`, the function aborts if the full set
#' of 12 registered IPM components cannot be completed from implemented rules or
#' accepted precomputed component columns. When `strict = FALSE`, pending
#' components are returned as `NA_integer_` and documented in the diagnostics
#' attribute.
#'
#' This function does not compute `ipm_score`, `tpm`, `tpem`, `A`, or aggregate
#' `ipm`, and it does not claim official validation.
#'
#' @param data Person-level ENEMDU data.
#' @param profile IPM derivation profile. Defaults to `"enemdu_2025_anual"`.
#' @param household_data Optional household- or housing-level ENEMDU data. When
#' supplied, it is joined with `enemdu_join_ipm_sources()`.
#' @param household_id Household identifier.
#' @param person_id Person identifier.
#' @param hsize_var Household-size variable. If absent, household size is
#' derived by counting persons within `household_id` for implemented household
#' components.
#' @param overwrite Logical. If `TRUE`, replace components that can be built
#' from implemented rules.
#' @param strict Logical. If `TRUE`, abort when any registered component remains
#' pending or when implemented source values are invalid or missing.
#' @param extreme_poverty_var Precomputed binary extreme-income-poverty flag.
#' Defaults to `"expobre"`.
#' @param build_from_precomputed Logical. If `TRUE`, existing registered
#' component columns are accepted as precomputed inputs after binary validation.
#' @param water_public_network_codes Codes that identify public-network water in
#' the source water variable. The default `1` is profile-specific and still
#' requires real-data reproducibility validation.
#' @param extreme_poverty_alias_vars Alias variables checked when
#' `extreme_poverty_var` is absent. Defaults to `"epobreza"` for the 2025
#' annual ENEMDU profile.
#' @param area_var Urban/rural area variable used by sanitation rules.
#' @param age_var Age variable.
#' @param attendance_var School-attendance variable.
#' @param attendance_yes_codes Codes that mean attending formal education.
#' @param attendance_no_codes Codes that mean not attending formal education.
#' @param school_age_min Minimum age for basic and bachillerato attendance.
#' @param school_age_max Maximum age for basic and bachillerato attendance.
#' @param education_level_var Educational level variable.
#' @param education_grade_var Completed grade/year variable within educational
#' level.
#' @param incomplete_education_age_min Minimum age for incomplete educational
#' attainment.
#' @param incomplete_education_age_max Maximum age for incomplete educational
#' attainment.
#' @param incomplete_schooling_years Schooling-year cutoff for incomplete
#' educational attainment.
#' @param condact_var Consolidated ENEMDU condition-of-activity variable used
#' through `enemdu_build_labor_flags()`.
#' @param labor_inadequate_flags Existing or derived labor flags treated as
#' unemployment or inadequate employment.
#' @param sanitation_var Excreta-sanitation source variable.
#' @param sanitation_urban_area_codes Area codes treated as urban.
#' @param sanitation_rural_area_codes Area codes treated as rural.
#' @param sanitation_sewer_codes Sanitation codes treated as sewerage.
#' @param sanitation_septic_codes Sanitation codes treated as septic tank.
#' @param sanitation_valid_codes Confirmed valid codes for the sanitation
#' source variable.
#' @param garbage_var Garbage-disposal source variable.
#' @param garbage_collection_codes Garbage-disposal codes treated as collection
#' service. The default includes contracted and municipal collection, based on
#' 2025 SPSS labels, and remains profile-specific.
#' @param garbage_valid_codes Confirmed valid codes for the garbage-disposal
#' source variable.
#'
#' @return A data frame with the 12 registered IPM component columns when
#' `strict = FALSE` or when all components are available, plus an
#' `ipm_component_diagnostics` attribute.
#' @export
enemdu_build_ipm_components <- function(
  data,
  profile = "enemdu_2025_anual",
  household_data = NULL,
  household_id = "id_hogar",
  person_id = "p01",
  hsize_var = "hsize",
  overwrite = FALSE,
  strict = TRUE,
  extreme_poverty_var = "expobre",
  build_from_precomputed = TRUE,
  water_public_network_codes = 1,
  extreme_poverty_alias_vars = "epobreza",
  area_var = "area",
  age_var = "p03",
  attendance_var = "p07",
  attendance_yes_codes = 1,
  attendance_no_codes = 2,
  school_age_min = 5,
  school_age_max = 17,
  education_level_var = "p10a",
  education_grade_var = "p10b",
  incomplete_education_age_min = 18,
  incomplete_education_age_max = 64,
  incomplete_schooling_years = 10,
  condact_var = "condact",
  labor_inadequate_flags = c(
    "labor_desempleo",
    "labor_subempleo",
    "labor_otro_empleo_no_pleno",
    "labor_empleo_no_remunerado",
    "labor_empleo_no_clasificado"
  ),
  sanitation_var = "vi09",
  sanitation_urban_area_codes = 1,
  sanitation_rural_area_codes = 2,
  sanitation_sewer_codes = 1,
  sanitation_septic_codes = 2,
  sanitation_valid_codes = 1:5,
  garbage_var = "vi13",
  garbage_collection_codes = c(1, 2),
  garbage_valid_codes = 1:4
) {
  caller <- "enemdu_build_ipm_components"

  if (!is.data.frame(data)) {
    .enemdu_abort_invalid_data(caller = caller)
  }

  profile <- .enemdu_ipm_single_value(profile, "profile")
  household_id <- .enemdu_ipm_single_var_name(household_id, "household_id", caller)
  person_id <- .enemdu_ipm_single_var_name(person_id, "person_id", caller)
  hsize_var <- .enemdu_ipm_single_var_name(hsize_var, "hsize_var", caller)
  extreme_poverty_var <- .enemdu_ipm_single_var_name(
    extreme_poverty_var,
    "extreme_poverty_var",
    caller
  )
  extreme_poverty_alias_vars <- .enemdu_ipm_var_names(
    extreme_poverty_alias_vars,
    "extreme_poverty_alias_vars"
  )
  area_var <- .enemdu_ipm_single_var_name(area_var, "area_var", caller)
  age_var <- .enemdu_ipm_single_var_name(age_var, "age_var", caller)
  attendance_var <- .enemdu_ipm_single_var_name(attendance_var, "attendance_var", caller)
  education_level_var <- .enemdu_ipm_single_var_name(
    education_level_var,
    "education_level_var",
    caller
  )
  education_grade_var <- .enemdu_ipm_single_var_name(
    education_grade_var,
    "education_grade_var",
    caller
  )
  condact_var <- .enemdu_ipm_single_var_name(condact_var, "condact_var", caller)
  labor_inadequate_flags <- .enemdu_ipm_var_names(
    labor_inadequate_flags,
    "labor_inadequate_flags"
  )
  sanitation_var <- .enemdu_ipm_single_var_name(sanitation_var, "sanitation_var", caller)
  garbage_var <- .enemdu_ipm_single_var_name(garbage_var, "garbage_var", caller)
  sanitation_valid_codes <- .enemdu_ipm_validate_code_set(
    sanitation_valid_codes,
    "sanitation_valid_codes"
  )
  garbage_valid_codes <- .enemdu_ipm_validate_code_set(
    garbage_valid_codes,
    "garbage_valid_codes"
  )

  component_registry <- .enemdu_ipm_component_registry_for_builder()
  .enemdu_validate_ipm_profile(profile)

  component_vars <- as.character(component_registry$expected_component_name)
  names(component_vars) <- as.character(component_registry$indicator_id)

  out <- data
  source_join_applied <- FALSE
  source_join_diagnostics <- NULL

  if (!is.null(household_data)) {
    out <- enemdu_join_ipm_sources(
      data = out,
      household_data = household_data,
      household_id = household_id,
      overwrite = overwrite,
      strict = strict
    )
    source_join_applied <- TRUE
    source_join_diagnostics <- attr(out, "ipm_source_join_diagnostics")
  }

  built_components <- character()
  precomputed_components <- character()
  pending_components <- character()
  pending_reasons <- list()
  variables_used <- list()

  precomputed <- .enemdu_accept_precomputed_ipm_components(
    data = out,
    component_vars = component_vars,
    strict = strict,
    enabled = isTRUE(build_from_precomputed)
  )
  out <- precomputed$data
  precomputed_components <- precomputed$components

  low_risk <- .enemdu_build_available_ipm_household_components(
    data = out,
    component_vars = component_vars,
    household_id = household_id,
    person_id = person_id,
    hsize_var = hsize_var,
    water_public_network_codes = water_public_network_codes,
    overwrite = overwrite,
    strict = strict
  )
  out <- low_risk$data
  built_components <- unique(c(built_components, low_risk$built_components))
  pending_components <- unique(c(pending_components, low_risk$pending_components))
  pending_reasons <- c(pending_reasons, low_risk$pending_reasons)
  variables_used <- c(variables_used, low_risk$variables_used)

  extreme_poverty <- .enemdu_build_ipm_extreme_poverty_component(
    data = out,
    component_var = component_vars[["ipm_i07_pobreza_extrema_ingresos"]],
    extreme_poverty_var = extreme_poverty_var,
    extreme_poverty_alias_vars = extreme_poverty_alias_vars,
    overwrite = overwrite,
    strict = strict
  )
  out <- extreme_poverty$data
  built_components <- unique(c(built_components, extreme_poverty$built_components))
  pending_components <- unique(c(pending_components, extreme_poverty$pending_components))
  pending_reasons <- c(pending_reasons, extreme_poverty$pending_reasons)
  variables_used <- c(variables_used, extreme_poverty$variables_used)

  school_attendance <- .enemdu_build_ipm_school_attendance_component(
    data = out,
    component_var = component_vars[["ipm_i01_inasistencia_basica_bach"]],
    household_id = household_id,
    age_var = age_var,
    attendance_var = attendance_var,
    attendance_yes_codes = attendance_yes_codes,
    attendance_no_codes = attendance_no_codes,
    school_age_min = school_age_min,
    school_age_max = school_age_max,
    overwrite = overwrite,
    strict = strict
  )
  out <- school_attendance$data
  built_components <- unique(c(built_components, school_attendance$built_components))
  pending_components <- unique(c(pending_components, school_attendance$pending_components))
  pending_reasons <- c(pending_reasons, school_attendance$pending_reasons)
  variables_used <- c(variables_used, school_attendance$variables_used)

  incomplete_education <- .enemdu_build_ipm_incomplete_education_component(
    data = out,
    component_var = component_vars[["ipm_i03_logro_educativo_incompleto"]],
    household_id = household_id,
    age_var = age_var,
    attendance_var = attendance_var,
    education_level_var = education_level_var,
    education_grade_var = education_grade_var,
    attendance_no_codes = attendance_no_codes,
    incomplete_education_age_min = incomplete_education_age_min,
    incomplete_education_age_max = incomplete_education_age_max,
    incomplete_schooling_years = incomplete_schooling_years,
    overwrite = overwrite,
    strict = strict
  )
  out <- incomplete_education$data
  built_components <- unique(c(built_components, incomplete_education$built_components))
  pending_components <- unique(c(pending_components, incomplete_education$pending_components))
  pending_reasons <- c(pending_reasons, incomplete_education$pending_reasons)
  variables_used <- c(variables_used, incomplete_education$variables_used)

  labor_inadequate <- .enemdu_build_ipm_labor_inadequate_component(
    data = out,
    component_var = component_vars[["ipm_i05_desempleo_empleo_inadecuado"]],
    household_id = household_id,
    age_var = age_var,
    condact_var = condact_var,
    labor_inadequate_flags = labor_inadequate_flags,
    overwrite = overwrite,
    strict = strict
  )
  out <- labor_inadequate$data
  built_components <- unique(c(built_components, labor_inadequate$built_components))
  pending_components <- unique(c(pending_components, labor_inadequate$pending_components))
  pending_reasons <- c(pending_reasons, labor_inadequate$pending_reasons)
  variables_used <- c(variables_used, labor_inadequate$variables_used)

  sanitation <- .enemdu_build_ipm_sanitation_component(
    data = out,
    component_var = component_vars[["ipm_i11_sin_saneamiento_excretas"]],
    area_var = area_var,
    sanitation_var = sanitation_var,
    sanitation_urban_area_codes = sanitation_urban_area_codes,
    sanitation_rural_area_codes = sanitation_rural_area_codes,
    sanitation_sewer_codes = sanitation_sewer_codes,
    sanitation_septic_codes = sanitation_septic_codes,
    sanitation_valid_codes = sanitation_valid_codes,
    overwrite = overwrite,
    strict = strict
  )
  out <- sanitation$data
  built_components <- unique(c(built_components, sanitation$built_components))
  pending_components <- unique(c(pending_components, sanitation$pending_components))
  pending_reasons <- c(pending_reasons, sanitation$pending_reasons)
  variables_used <- c(variables_used, sanitation$variables_used)

  garbage <- .enemdu_build_ipm_garbage_component(
    data = out,
    component_var = component_vars[["ipm_i12_sin_recoleccion_basura"]],
    garbage_var = garbage_var,
    garbage_collection_codes = garbage_collection_codes,
    garbage_valid_codes = garbage_valid_codes,
    overwrite = overwrite,
    strict = strict
  )
  out <- garbage$data
  built_components <- unique(c(built_components, garbage$built_components))
  pending_components <- unique(c(pending_components, garbage$pending_components))
  pending_reasons <- c(pending_reasons, garbage$pending_reasons)
  variables_used <- c(variables_used, garbage$variables_used)

  unavailable_indicator_ids <- setdiff(
    names(component_vars),
    c(
      "ipm_i01_inasistencia_basica_bach",
      "ipm_i03_logro_educativo_incompleto",
      "ipm_i05_desempleo_empleo_inadecuado",
      "ipm_i07_pobreza_extrema_ingresos",
      "ipm_i08_sin_agua_red_publica",
      "ipm_i09_hacinamiento",
      "ipm_i11_sin_saneamiento_excretas",
      "ipm_i12_sin_recoleccion_basura"
    )
  )

  for (indicator_id in unavailable_indicator_ids) {
    component_var <- component_vars[[indicator_id]]

    if (component_var %in% precomputed_components) {
      next
    }

    pending_components <- unique(c(pending_components, component_var))
    pending_reasons[[component_var]] <- .enemdu_pending_ipm_component_reason(indicator_id)
  }

  completed_components <- unique(c(built_components, precomputed_components))
  pending_components <- setdiff(unique(pending_components), completed_components)

  if (length(pending_components) > 0 && isTRUE(strict)) {
    .enemdu_abort_pending_ipm_components(
      pending_components = pending_components,
      pending_reasons = pending_reasons
    )
  }

  if (length(pending_components) > 0 && !isTRUE(strict)) {
    for (component in pending_components) {
      out[[component]] <- rep(NA_integer_, nrow(out))
    }
  }

  component_na_counts <- stats::setNames(
    as.integer(vapply(component_vars, function(component) {
      if (!component %in% names(out)) {
        return(NA_integer_)
      }
      sum(is.na(out[[component]]))
    }, integer(1))),
    component_vars
  )

  attr(out, "ipm_component_diagnostics") <- list(
    profile = profile,
    n_rows = nrow(out),
    component_vars = unname(component_vars),
    components_built = built_components,
    components_precomputed = precomputed_components,
    components_pending = pending_components,
    pending_reasons = pending_reasons[pending_components],
    variables_used = variables_used,
    component_na_counts = component_na_counts,
    household_id = household_id,
    person_id = person_id,
    hsize_var = hsize_var,
    source_join_applied = source_join_applied,
    source_join_diagnostics = source_join_diagnostics,
    strict = isTRUE(strict),
    overwrite = isTRUE(overwrite),
    build_from_precomputed = isTRUE(build_from_precomputed),
    official_validation_status = "not_officially_validated"
  )

  class(out) <- unique(c("enemdu_tbl", class(out)))
  out
}

.enemdu_ipm_component_registry_for_builder <- function() {
  registry <- .enemdu_ipm_component_registry()
  required_cols <- c("indicator_order", "indicator_id", "expected_component_name")
  missing_cols <- setdiff(required_cols, names(registry))

  if (length(missing_cols) > 0) {
    .enemdu_abort_invalid_registry(
      registry_name = "ipm_component_registry",
      message = glue::glue("Missing columns: {paste(missing_cols, collapse = ', ')}."),
      caller = ".enemdu_ipm_component_registry_for_builder"
    )
  }

  registry <- registry[order(registry$indicator_order), , drop = FALSE]

  if (
    any(is.na(registry$indicator_id)) ||
      any(!nzchar(as.character(registry$indicator_id))) ||
      anyDuplicated(registry$indicator_id) > 0
  ) {
    .enemdu_abort_invalid_registry(
      registry_name = "ipm_component_registry",
      message = "Registered IPM indicator IDs must be unique and non-empty.",
      caller = ".enemdu_ipm_component_registry_for_builder"
    )
  }

  if (
    any(is.na(registry$expected_component_name)) ||
      any(!nzchar(as.character(registry$expected_component_name))) ||
      anyDuplicated(registry$expected_component_name) > 0
  ) {
    .enemdu_abort_invalid_registry(
      registry_name = "ipm_component_registry",
      message = "Registered IPM component names must be unique and non-empty.",
      caller = ".enemdu_ipm_component_registry_for_builder"
    )
  }

  registry
}

.enemdu_validate_ipm_profile <- function(profile) {
  registry <- .enemdu_read_csv_registry("ipm_derivation_registry.csv")

  if (!"profile" %in% names(registry)) {
    .enemdu_abort_invalid_registry(
      registry_name = "ipm_derivation_registry",
      message = "Missing column: profile.",
      caller = ".enemdu_validate_ipm_profile"
    )
  }

  if (!profile %in% registry$profile) {
    rlang::abort(
      message = glue::glue("IPM derivation profile `{profile}` was not found in the registry."),
      class = c("enemdu_error_invalid_ipm_derivation_profile", "enemdu_error")
    )
  }

  invisible(TRUE)
}

.enemdu_accept_precomputed_ipm_components <- function(data,
                                                      component_vars,
                                                      strict,
                                                      enabled) {
  if (!isTRUE(enabled)) {
    return(list(data = data, components = character()))
  }

  out <- data
  existing_components <- intersect(unname(component_vars), names(out))

  for (component in existing_components) {
    out[[component]] <- .enemdu_coerce_ipm_component(
      values = out[[component]],
      var = component,
      strict = strict
    )
  }

  list(data = out, components = existing_components)
}

.enemdu_build_available_ipm_household_components <- function(data,
                                                             component_vars,
                                                             household_id,
                                                             person_id,
                                                             hsize_var,
                                                             water_public_network_codes,
                                                             overwrite,
                                                             strict) {
  low_risk_ids <- c(
    "ipm_i08_sin_agua_red_publica",
    "ipm_i09_hacinamiento"
  )
  low_risk_components <- unname(component_vars[low_risk_ids])
  existing_low_risk <- intersect(low_risk_components, names(data))

  if (setequal(existing_low_risk, low_risk_components) && !isTRUE(overwrite)) {
    return(list(
      data = data,
      built_components = character(),
      pending_components = character(),
      pending_reasons = list(),
      variables_used = list()
    ))
  }

  required_vars <- c(household_id, "vi10", "vi07")

  if (hsize_var %in% names(data)) {
    required_vars <- c(required_vars, hsize_var)
  } else {
    required_vars <- c(required_vars, person_id)
  }

  missing_vars <- setdiff(unique(required_vars), names(data))

  if (length(missing_vars) > 0) {
    if (isTRUE(strict)) {
      .enemdu_abort_missing_vars(
        vars = unique(required_vars),
        names_data = names(data),
        caller = "enemdu_build_ipm_components"
      )
    }

    reasons <- stats::setNames(
      as.list(rep(
        paste0(
          "Missing source variables for implemented household rule: ",
          paste(missing_vars, collapse = ", "),
          "."
        ),
        length(low_risk_components)
      )),
      low_risk_components
    )

    return(list(
      data = data,
      built_components = character(),
      pending_components = low_risk_components,
      pending_reasons = reasons,
      variables_used = list()
    ))
  }

  temp <- data

  if (length(existing_low_risk) > 0 && !isTRUE(overwrite)) {
    temp[existing_low_risk] <- NULL
  }

  temp <- .enemdu_build_ipm_low_risk_household_components(
    data = temp,
    household_id = household_id,
    person_id = person_id,
    hsize_var = hsize_var,
    water_var = "vi10",
    bedrooms_var = "vi07",
    water_public_network_codes = water_public_network_codes,
    overwrite = TRUE,
    strict = strict
  )

  out <- data
  built_components <- character()

  for (component in low_risk_components) {
    if (component %in% existing_low_risk && !isTRUE(overwrite)) {
      next
    }

    out[[component]] <- temp[[component]]
    built_components <- c(built_components, component)
  }

  list(
    data = out,
    built_components = built_components,
    pending_components = character(),
    pending_reasons = list(),
    variables_used = list(
      low_risk_household = list(
        household_id = household_id,
        person_id = person_id,
        hsize_var = hsize_var,
        water_var = "vi10",
        bedrooms_var = "vi07",
        water_public_network_codes = water_public_network_codes
      )
    )
  )
}

.enemdu_build_ipm_extreme_poverty_component <- function(data,
                                                        component_var,
                                                        extreme_poverty_var,
                                                        extreme_poverty_alias_vars,
                                                        overwrite,
                                                        strict) {
  if (component_var %in% names(data) && !isTRUE(overwrite)) {
    return(list(
      data = data,
      built_components = character(),
      pending_components = character(),
      pending_reasons = list(),
      variables_used = list()
    ))
  }

  source_var <- .enemdu_ipm_first_existing_var(
    data = data,
    vars = c(extreme_poverty_var, extreme_poverty_alias_vars)
  )

  if (is.na(source_var)) {
    reason <- paste0(
      "No precomputed extreme income poverty variable is available. Checked: ",
      paste(c(extreme_poverty_var, extreme_poverty_alias_vars), collapse = ", "),
      ". The public builder does not derive poverty lines."
    )

    return(list(
      data = data,
      built_components = character(),
      pending_components = component_var,
      pending_reasons = stats::setNames(list(reason), component_var),
      variables_used = list()
    ))
  }

  out <- data
  out[[component_var]] <- .enemdu_coerce_ipm_component(
    values = out[[source_var]],
    var = source_var,
    strict = strict
  )

  list(
    data = out,
    built_components = component_var,
    pending_components = character(),
    pending_reasons = list(),
    variables_used = list(
      extreme_income_poverty = list(
        source_var = source_var,
        checked_aliases = c(extreme_poverty_var, extreme_poverty_alias_vars),
        output_component = component_var,
        rule = "copy_precomputed_binary_extreme_poverty_flag"
      )
    )
  )
}

.enemdu_build_ipm_school_attendance_component <- function(data,
                                                          component_var,
                                                          household_id,
                                                          age_var,
                                                          attendance_var,
                                                          attendance_yes_codes,
                                                          attendance_no_codes,
                                                          school_age_min,
                                                          school_age_max,
                                                          overwrite,
                                                          strict) {
  if (component_var %in% names(data) && !isTRUE(overwrite)) {
    return(.enemdu_ipm_noop_component_result(data))
  }

  required_vars <- c(household_id, age_var, attendance_var)
  missing_vars <- setdiff(required_vars, names(data))

  if (length(missing_vars) > 0) {
    return(.enemdu_ipm_pending_component_result(
      data = data,
      component_var = component_var,
      reason = paste0(
        "Missing source variables for school attendance: ",
        paste(missing_vars, collapse = ", "),
        "."
      )
    ))
  }

  age <- .enemdu_ipm_coerce_source_numeric(data[[age_var]], age_var)
  attendance <- .enemdu_ipm_coerce_source_numeric(data[[attendance_var]], attendance_var)
  yes_codes <- .enemdu_ipm_validate_code_set(attendance_yes_codes, "attendance_yes_codes")
  no_codes <- .enemdu_ipm_validate_code_set(attendance_no_codes, "attendance_no_codes")

  valid_attendance_codes <- unique(c(yes_codes, no_codes))
  invalid_attendance <- !is.na(attendance) & !attendance %in% valid_attendance_codes

  if (any(invalid_attendance) && isTRUE(strict)) {
    .enemdu_abort_invalid_ipm_source_codes(attendance_var, valid_attendance_codes)
  }

  person_deprivation <- rep(0L, length(age))
  applicable <- !is.na(age) & age >= school_age_min & age <= school_age_max

  person_deprivation[is.na(age)] <- NA_integer_
  person_deprivation[applicable & (is.na(attendance) | invalid_attendance)] <- NA_integer_
  observed_applicable <- applicable & !is.na(attendance) & !invalid_attendance
  person_deprivation[observed_applicable] <-
    as.integer(attendance[observed_applicable] %in% no_codes)

  component <- .enemdu_ipm_household_max_complete(
    household_id = data[[household_id]],
    values = person_deprivation
  )
  .enemdu_ipm_abort_component_missing_if_strict(
    component = component,
    component_var = component_var,
    strict = strict
  )

  out <- data
  out[[component_var]] <- component

  list(
    data = out,
    built_components = component_var,
    pending_components = character(),
    pending_reasons = list(),
    variables_used = list(
      school_attendance = list(
        source_vars = c(household_id, age_var, attendance_var),
        attendance_yes_codes = yes_codes,
        attendance_no_codes = no_codes,
        applicable_age_range = c(school_age_min, school_age_max),
        output_component = component_var
      )
    )
  )
}

.enemdu_build_ipm_incomplete_education_component <- function(data,
                                                             component_var,
                                                             household_id,
                                                             age_var,
                                                             attendance_var,
                                                             education_level_var,
                                                             education_grade_var,
                                                             attendance_no_codes,
                                                             incomplete_education_age_min,
                                                             incomplete_education_age_max,
                                                             incomplete_schooling_years,
                                                             overwrite,
                                                             strict) {
  if (component_var %in% names(data) && !isTRUE(overwrite)) {
    return(.enemdu_ipm_noop_component_result(data))
  }

  required_vars <- c(
    household_id,
    age_var,
    attendance_var,
    education_level_var,
    education_grade_var
  )
  missing_vars <- setdiff(required_vars, names(data))

  if (length(missing_vars) > 0) {
    return(.enemdu_ipm_pending_component_result(
      data = data,
      component_var = component_var,
      reason = paste0(
        "Missing source variables for incomplete education: ",
        paste(missing_vars, collapse = ", "),
        "."
      )
    ))
  }

  age <- .enemdu_ipm_coerce_source_numeric(data[[age_var]], age_var)
  attendance <- .enemdu_ipm_coerce_source_numeric(data[[attendance_var]], attendance_var)
  education_level <- .enemdu_ipm_coerce_source_numeric(
    data[[education_level_var]],
    education_level_var
  )
  education_grade <- .enemdu_ipm_coerce_source_numeric(
    data[[education_grade_var]],
    education_grade_var
  )
  no_codes <- .enemdu_ipm_validate_code_set(attendance_no_codes, "attendance_no_codes")

  schooling_years <- .enemdu_ipm_schooling_years_2025(
    education_level = education_level,
    education_grade = education_grade
  )

  person_deprivation <- rep(0L, length(age))
  applicable <- !is.na(age) &
    age >= incomplete_education_age_min &
    age <= incomplete_education_age_max
  not_attending <- !is.na(attendance) & attendance %in% no_codes
  observed_applicable <- applicable & not_attending & !is.na(schooling_years)

  person_deprivation[is.na(age)] <- NA_integer_
  person_deprivation[applicable & is.na(attendance)] <- NA_integer_
  person_deprivation[applicable & not_attending & is.na(schooling_years)] <- NA_integer_
  person_deprivation[observed_applicable] <-
    as.integer(schooling_years[observed_applicable] < incomplete_schooling_years)

  component <- .enemdu_ipm_household_max_complete(
    household_id = data[[household_id]],
    values = person_deprivation
  )
  .enemdu_ipm_abort_component_missing_if_strict(
    component = component,
    component_var = component_var,
    strict = strict
  )

  out <- data
  out[[component_var]] <- component

  list(
    data = out,
    built_components = component_var,
    pending_components = character(),
    pending_reasons = list(),
    variables_used = list(
      incomplete_education = list(
        source_vars = c(household_id, age_var, attendance_var, education_level_var, education_grade_var),
        attendance_no_codes = no_codes,
        applicable_age_range = c(incomplete_education_age_min, incomplete_education_age_max),
        incomplete_schooling_years = incomplete_schooling_years,
        output_component = component_var,
        note = paste(
          "Schooling years use the 2025 p10a labels and p10b grade values.",
          "This is profile-specific and must be compared against benchmarks before validation claims."
        )
      )
    )
  )
}

.enemdu_build_ipm_labor_inadequate_component <- function(data,
                                                         component_var,
                                                         household_id,
                                                         age_var,
                                                         condact_var,
                                                         labor_inadequate_flags,
                                                         overwrite,
                                                         strict) {
  if (component_var %in% names(data) && !isTRUE(overwrite)) {
    return(.enemdu_ipm_noop_component_result(data))
  }

  if (!household_id %in% names(data) || !age_var %in% names(data)) {
    missing_vars <- setdiff(c(household_id, age_var), names(data))
    return(.enemdu_ipm_pending_component_result(
      data = data,
      component_var = component_var,
      reason = paste0(
        "Missing source variables for labor component: ",
        paste(missing_vars, collapse = ", "),
        "."
      )
    ))
  }

  labor_data <- data
  missing_flags <- setdiff(labor_inadequate_flags, names(labor_data))

  if (length(missing_flags) > 0) {
    if (!condact_var %in% names(labor_data)) {
      return(.enemdu_ipm_pending_component_result(
        data = data,
        component_var = component_var,
        reason = paste0(
          "Missing labor flags and consolidated condition-of-activity variable `",
          condact_var,
          "`. The builder does not reconstruct labor status from raw questionnaire variables."
        )
      ))
    }

    labor_input <- labor_data[, unique(c(condact_var, age_var)), drop = FALSE]

    labor_flags_data <- enemdu_build_labor_flags(
      data = labor_input,
      condact = condact_var,
      age = age_var,
      strict = strict
    )

    new_labor_flags <- setdiff(names(labor_flags_data), names(labor_data))

    for (flag in new_labor_flags) {
      labor_data[[flag]] <- labor_flags_data[[flag]]
    }
  }

  missing_flags <- setdiff(labor_inadequate_flags, names(labor_data))

  if (length(missing_flags) > 0) {
    return(.enemdu_ipm_pending_component_result(
      data = data,
      component_var = component_var,
      reason = paste0(
        "Missing labor flags for inadequate employment: ",
        paste(missing_flags, collapse = ", "),
        "."
      )
    ))
  }

  age <- .enemdu_ipm_coerce_source_numeric(labor_data[[age_var]], age_var)
  flag_data <- lapply(labor_inadequate_flags, function(flag) {
    .enemdu_coerce_ipm_component(
      values = labor_data[[flag]],
      var = flag,
      strict = strict
    )
  })
  flag_matrix <- as.data.frame(flag_data, optional = TRUE)
  names(flag_matrix) <- labor_inadequate_flags

  person_deprivation <- rep(0L, length(age))

    # The Ecuador IPM component "desempleo o empleo inadecuado" applies to
  # people aged 18 years and older. This cutoff intentionally differs from
  # the general ENEMDU labor helper working-age cutoff.
  applicable <- !is.na(age) & age >= 18
  row_missing_flag <- !stats::complete.cases(flag_matrix)
  any_inadequate <- rowSums(flag_matrix == 1, na.rm = TRUE) > 0

  person_deprivation[is.na(age)] <- NA_integer_
  person_deprivation[applicable & row_missing_flag] <- NA_integer_
  person_deprivation[applicable & !row_missing_flag] <-
    as.integer(any_inadequate[applicable & !row_missing_flag])

  component <- .enemdu_ipm_household_max_complete(
    household_id = labor_data[[household_id]],
    values = person_deprivation
  )
  .enemdu_ipm_abort_component_missing_if_strict(
    component = component,
    component_var = component_var,
    strict = strict
  )

  out <- data
  out[[component_var]] <- component

  list(
    data = out,
    built_components = component_var,
    pending_components = character(),
    pending_reasons = list(),
    variables_used = list(
      labor_inadequate_employment = list(
        source_vars = c(household_id, age_var, condact_var),
        labor_inadequate_flags = labor_inadequate_flags,
        output_component = component_var,
        rule = "household_has_person_age_18_plus_unemployed_or_inadequately_employed",
        note = paste(
          "Labor flags are derived only from the consolidated ENEMDU condact variable when needed.",
          "Sector variables such as secemp are intentionally ignored for this IPM component."
        )
      )
    )
  )
}

.enemdu_build_ipm_sanitation_component <- function(data,
                                                   component_var,
                                                   area_var,
                                                   sanitation_var,
                                                   sanitation_urban_area_codes,
                                                   sanitation_rural_area_codes,
                                                   sanitation_sewer_codes,
                                                   sanitation_septic_codes,
                                                   sanitation_valid_codes,
                                                   overwrite,
                                                   strict) {
  if (component_var %in% names(data) && !isTRUE(overwrite)) {
    return(.enemdu_ipm_noop_component_result(data))
  }

  required_vars <- c(area_var, sanitation_var)
  missing_vars <- setdiff(required_vars, names(data))

  if (length(missing_vars) > 0) {
    return(.enemdu_ipm_pending_component_result(
      data = data,
      component_var = component_var,
      reason = paste0(
        "Missing source variables for sanitation: ",
        paste(missing_vars, collapse = ", "),
        "."
      )
    ))
  }

  area <- .enemdu_ipm_coerce_source_numeric(data[[area_var]], area_var)
  sanitation <- .enemdu_ipm_coerce_source_numeric(data[[sanitation_var]], sanitation_var)
  urban_codes <- .enemdu_ipm_validate_code_set(sanitation_urban_area_codes, "sanitation_urban_area_codes")
  rural_codes <- .enemdu_ipm_validate_code_set(sanitation_rural_area_codes, "sanitation_rural_area_codes")
  sewer_codes <- .enemdu_ipm_validate_code_set(sanitation_sewer_codes, "sanitation_sewer_codes")
  septic_codes <- .enemdu_ipm_validate_code_set(sanitation_septic_codes, "sanitation_septic_codes")
  valid_sanitation_codes <- .enemdu_ipm_validate_code_set(
    sanitation_valid_codes,
    "sanitation_valid_codes"
  )

  invalid_sanitation <- !is.na(sanitation) &
    !(sanitation %in% valid_sanitation_codes)

  if (any(invalid_sanitation) && isTRUE(strict)) {
    .enemdu_abort_invalid_ipm_source_codes(sanitation_var, valid_sanitation_codes)
  }

  urban <- !is.na(area) & area %in% urban_codes
  rural <- !is.na(area) & area %in% rural_codes
  known_area <- urban | rural

  component <- rep(NA_integer_, length(area))
  valid_urban <- urban & !is.na(sanitation) & !invalid_sanitation
  valid_rural <- rural & !is.na(sanitation) & !invalid_sanitation
  component[valid_urban] <- as.integer(!(sanitation[valid_urban] %in% sewer_codes))
  component[valid_rural] <- as.integer(
    !(sanitation[valid_rural] %in% c(sewer_codes, septic_codes))
  )
  component[!is.na(area) & !known_area] <- NA_integer_

  .enemdu_ipm_abort_component_missing_if_strict(
    component = component,
    component_var = component_var,
    strict = strict
  )

  out <- data
  out[[component_var]] <- component

  list(
    data = out,
    built_components = component_var,
    pending_components = character(),
    pending_reasons = list(),
    variables_used = list(
      sanitation = list(
        source_vars = c(area_var, sanitation_var),
        urban_area_codes = urban_codes,
        rural_area_codes = rural_codes,
        sewer_codes = sewer_codes,
        septic_codes = septic_codes,
        sanitation_valid_codes = valid_sanitation_codes,
        output_component = component_var,
        rule = "urban_requires_sewerage_rural_requires_sewerage_or_septic_tank"
      )
    )
  )
}

.enemdu_build_ipm_garbage_component <- function(data,
                                                component_var,
                                                garbage_var,
                                                garbage_collection_codes,
                                                garbage_valid_codes,
                                                overwrite,
                                                strict) {
  if (component_var %in% names(data) && !isTRUE(overwrite)) {
    return(.enemdu_ipm_noop_component_result(data))
  }

  if (!garbage_var %in% names(data)) {
    return(.enemdu_ipm_pending_component_result(
      data = data,
      component_var = component_var,
      reason = paste0("Missing source variable for garbage collection: ", garbage_var, ".")
    ))
  }

  garbage <- .enemdu_ipm_coerce_source_numeric(data[[garbage_var]], garbage_var)
  collection_codes <- .enemdu_ipm_validate_code_set(
    garbage_collection_codes,
    "garbage_collection_codes"
  )
  valid_garbage_codes <- .enemdu_ipm_validate_code_set(
    garbage_valid_codes,
    "garbage_valid_codes"
  )

  invalid_garbage <- !is.na(garbage) &
    !(garbage %in% valid_garbage_codes)

  if (any(invalid_garbage) && isTRUE(strict)) {
    .enemdu_abort_invalid_ipm_source_codes(garbage_var, valid_garbage_codes)
  }

  component <- rep(NA_integer_, length(garbage))
  observed <- !is.na(garbage) & !invalid_garbage
  component[observed] <- as.integer(!(garbage[observed] %in% collection_codes))

  .enemdu_ipm_abort_component_missing_if_strict(
    component = component,
    component_var = component_var,
    strict = strict
  )

  out <- data
  out[[component_var]] <- component

  list(
    data = out,
    built_components = component_var,
    pending_components = character(),
    pending_reasons = list(),
    variables_used = list(
      garbage_collection = list(
        source_var = garbage_var,
        garbage_collection_codes = collection_codes,
        garbage_valid_codes = valid_garbage_codes,
        output_component = component_var,
        rule = "not_deprived_when_garbage_disposal_is_contracted_or_municipal_collection"
      )
    )
  )
}

.enemdu_pending_ipm_component_reason <- function(indicator_id) {
  paste0(
    "No operational rule is implemented for `",
    indicator_id,
    "` because the repository audit does not yet provide enough confirmed ",
    "source-variable and coding evidence."
  )
}

.enemdu_ipm_noop_component_result <- function(data) {
  list(
    data = data,
    built_components = character(),
    pending_components = character(),
    pending_reasons = list(),
    variables_used = list()
  )
}

.enemdu_ipm_pending_component_result <- function(data, component_var, reason) {
  list(
    data = data,
    built_components = character(),
    pending_components = component_var,
    pending_reasons = stats::setNames(list(reason), component_var),
    variables_used = list()
  )
}

.enemdu_ipm_first_existing_var <- function(data, vars) {
  vars <- as.character(vars)
  matched <- vars[vars %in% names(data)]

  if (length(matched) == 0) {
    return(NA_character_)
  }

  matched[[1]]
}

.enemdu_ipm_coerce_source_numeric <- function(values, var) {
  missing <- is.na(values)
  numeric_values <- .enemdu_coerce_ipm_numeric(values)
  invalid_conversion <- !missing & is.na(numeric_values)

  if (any(invalid_conversion)) {
    rlang::abort(
      message = glue::glue("IPM source variable `{var}` must contain visible numeric codes."),
      class = c("enemdu_error_invalid_ipm_source", "enemdu_error")
    )
  }

  numeric_values
}

.enemdu_ipm_validate_code_set <- function(codes, arg) {
  numeric_codes <- .enemdu_coerce_ipm_numeric(codes)

  if (
    length(numeric_codes) == 0 ||
      any(is.na(numeric_codes)) ||
      any(!is.finite(numeric_codes))
  ) {
    rlang::abort(
      message = glue::glue("`{arg}` must contain finite numeric codes."),
      class = c("enemdu_error_invalid_ipm_code_set", "enemdu_error")
    )
  }

  unique(numeric_codes)
}

.enemdu_abort_invalid_ipm_source_codes <- function(var, valid_codes) {
  rlang::abort(
    message = glue::glue(
      "IPM source variable `{var}` contains values outside confirmed code set: ",
      "{paste(valid_codes, collapse = ', ')}."
    ),
    class = c("enemdu_error_invalid_ipm_source_codes", "enemdu_error")
  )
}

.enemdu_ipm_household_max_complete <- function(household_id, values) {
  ids <- as.character(household_id)
  split_values <- split(values, ids)
  household_values <- vapply(split_values, function(x) {
    if (any(x == 1L, na.rm = TRUE)) {
      return(1L)
    }
    if (all(!is.na(x) & x == 0L)) {
      return(0L)
    }
    NA_integer_
  }, integer(1))

  as.integer(household_values[ids])
}

.enemdu_ipm_abort_component_missing_if_strict <- function(component,
                                                         component_var,
                                                         strict) {
  if (isTRUE(strict) && any(is.na(component))) {
    rlang::abort(
      message = glue::glue(
        "IPM component `{component_var}` contains missing values from source derivation."
      ),
      class = c("enemdu_error_missing_ipm_component_derivation", "enemdu_error")
    )
  }

  invisible(TRUE)
}

.enemdu_ipm_schooling_years_2025 <- function(education_level,
                                             education_grade) {
  years <- rep(NA_real_, length(education_level))

  no_schooling <- !is.na(education_level) & education_level == 1
  kindergarten <- !is.na(education_level) & education_level == 3
  years[no_schooling | kindergarten] <- 0

  literacy <- !is.na(education_level) & education_level == 2 & !is.na(education_grade)
  years[literacy] <- education_grade[literacy]

  primary <- !is.na(education_level) & education_level == 4 & !is.na(education_grade)
  years[primary] <- pmin(education_grade[primary], 6)

  basic <- !is.na(education_level) & education_level == 5 & !is.na(education_grade)
  years[basic] <- education_grade[basic]

  secondary <- !is.na(education_level) & education_level == 6 & !is.na(education_grade)
  years[secondary] <- 6 + education_grade[secondary]

  upper_secondary <- !is.na(education_level) & education_level == 7 & !is.na(education_grade)
  years[upper_secondary] <- 10 + education_grade[upper_secondary]

  higher_non_university <- !is.na(education_level) &
    education_level == 8 &
    !is.na(education_grade)
  years[higher_non_university] <- 13 + education_grade[higher_non_university]

  university <- !is.na(education_level) &
    education_level == 9 &
    !is.na(education_grade)
  years[university] <- 13 + education_grade[university]

  postgraduate <- !is.na(education_level) &
    education_level == 10 &
    !is.na(education_grade)
  years[postgraduate] <- 18 + education_grade[postgraduate]

  invalid_grade <- !is.na(education_grade) & education_grade < 0
  years[invalid_grade] <- NA_real_

  years
}

.enemdu_abort_pending_ipm_components <- function(pending_components,
                                                pending_reasons) {
  reasons <- unlist(pending_reasons[pending_components], use.names = TRUE)

  rlang::abort(
    message = glue::glue(
      "Cannot build all 12 registered IPM components with the current audited rules. ",
      "Pending components: {paste(pending_components, collapse = ', ')}."
    ),
    class = c("enemdu_error_pending_ipm_components", "enemdu_error"),
    pending_components = pending_components,
    pending_reasons = reasons
  )
}

.enemdu_ipm_single_value <- function(value, arg) {
  value <- as.character(value)

  if (length(value) != 1 || is.na(value) || !nzchar(value)) {
    rlang::abort(
      message = glue::glue("`{arg}` must be a single non-empty value."),
      class = c("enemdu_error_invalid_ipm_component_input", "enemdu_error")
    )
  }

  value
}

.enemdu_ipm_var_names <- function(vars, arg) {
  vars <- as.character(vars)

  if (length(vars) == 0 || any(is.na(vars)) || any(!nzchar(vars))) {
    rlang::abort(
      message = glue::glue("`{arg}` must contain non-empty variable names."),
      class = c("enemdu_error_invalid_ipm_component_input", "enemdu_error")
    )
  }

  unique(vars)
}
