#' Build registered IPM component indicators
#'
#' Builds the IPM component columns that are auditable in the current package
#' contract and marks the remaining registered components as pending. The
#' current operational rules cover all 12 registered components for the
#' `enemdu_2025_anual` profile when the required source variables are present.
#'
#' This function does not invent unsupported rules. Rule details that depend on
#' questionnaire codes are explicit arguments. When `strict = TRUE`, the
#' function aborts if the full set of 12 registered IPM components cannot be
#' completed from implemented rules or accepted precomputed component columns.
#' When `strict = FALSE`, pending or non-evaluable components are returned as
#' `NA_integer_` and documented in the diagnostics attribute.
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
#' Defaults to `"epobre"` for the official-syntax profile.
#' @param extreme_poverty_income_var Optional per-capita income variable used
#' only to fill missing values in the selected `extreme_poverty_var` or alias
#' when `extreme_poverty_line` is also supplied. This local reproducibility
#' fallback does not constitute official validation by itself.
#' @param extreme_poverty_line Optional extreme-poverty line used with
#' `extreme_poverty_income_var` to fill missing selected binary-flag cases via
#' `income < line`. The line is never hard-coded by this builder.
#' @param build_from_precomputed Logical. If `TRUE`, existing registered
#' component columns are accepted as precomputed inputs after binary validation.
#' @param water_public_network_codes Codes that identify public-network water in
#' the source water variable. The default `1` is profile-specific and still
#' requires real-data reproducibility validation.
#' @param extreme_poverty_alias_vars Alias variables checked when
#' `extreme_poverty_var` is absent. Defaults to `"epobreza"` and `"expobre"`
#' for the 2025 annual ENEMDU profile.
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
#' @param higher_education_reason_var Reason for not attending higher education.
#' @param higher_education_economic_reason_codes Codes treated as economic
#' reasons for not accessing higher education. The default `3` follows the
#' official-syntax profile. If `NULL`, the builder attempts to infer the code
#' from value labels containing economic-resource wording.
#' @param bachillerato_completed_levels Optional education-level codes that
#' identify completed bachillerato when paired with
#' `bachillerato_completed_min_grade`. If `NULL`, the profile-specific
#' schooling-years helper is used.
#' @param bachillerato_completed_min_grade Optional minimum grade/year for the
#' levels in `bachillerato_completed_levels`.
#' @param child_work_var Binary employment/work variable for children and
#' adolescents.
#' @param child_hours_var Weekly-hours variable for working adolescents.
#' @param child_income_var Labor-income variable for working adolescents.
#' @param p20_var,p21_var,p22_var,p24_var,pea_var Official child/adolescent
#' employment source variables used when available.
#' @param condactn_var Candidate official condition-of-activity variables.
#' Defaults to checking `"condactn"` and `"condact"`.
#' @param p51_prefix Prefix for p51 hour variables used in official
#' child/adolescent employment hours.
#' @param child_sbu Profile-specific basic salary cutoff used for adolescents.
#' The default `470` is for the 2025 annual profile.
#' @param employment_na_as_not_employed Logical. If `TRUE`, missing values in
#' the operational employment flag are treated as not employed for IPM
#' components that use `employment_var`. This matches the ENEMDU 2025 annual
#' profile where `empleo` is coded as `1/NA`.
#' @param hours_sentinel_codes Hour codes treated as non-evaluable sentinels.
#' @param income_negative_sentinel_codes Income codes treated as negative-income
#' sentinels.
#' @param income_missing_sentinel_codes Income codes treated as missing-income
#' sentinels.
#' @param social_security_var Social-security contribution variable.
#' @param social_security_a_var,social_security_b_var Official social-security
#' affiliation/contribution variables used when available.
#' @param pension_income_var Retirement or pension receipt variable.
#' @param human_development_bonus_var Human-development bonus receipt variable.
#' @param disability_bonus_var Disability-bonus proxy variable.
#' @param social_security_contribution_codes Contribution codes.
#' @param social_security_no_contribution_codes No-contribution codes.
#' @param social_security_unknown_codes Unknown contribution codes.
#' @param unemployment_var Binary unemployment variable used for diagnostics in
#' the pension component.
#' @param unemployed_var,inactive_var,working_age_var Official pension component
#' source variables used when available.
#' @param roof_material_var Roof-material variable.
#' @param roof_state_var Roof-state variable.
#' @param floor_material_var Floor-material variable.
#' @param floor_state_var Floor-state variable.
#' @param wall_material_var Wall-material variable.
#' @param wall_state_var Wall-state variable.
#' @param housing_material_valid_codes Confirmed valid material codes. If
#' `NULL`, labels are used when available.
#' @param housing_state_valid_codes Confirmed valid state codes.
#' @param bedrooms_zero_policy Policy for `vi07 == 0` in overcrowding. The
#' default `"official_recode_to_one"` follows the official syntax.
#' @param deficit_roof_material_codes Roof-material codes treated as deficit.
#' @param deficit_floor_material_codes Floor-material codes treated as deficit.
#' @param deficit_wall_material_codes Wall-material codes treated as deficit.
#' @param deficit_state_codes State codes treated as deficit.
#' @param sanitation_var Excreta-sanitation source variable.
#' @param sanitation_urban_area_codes Area codes treated as urban.
#' @param sanitation_rural_area_codes Area codes treated as rural.
#' @param sanitation_sewer_codes Sanitation codes treated as sewerage.
#' @param sanitation_septic_codes Sanitation codes treated as septic tank.
#' @param sanitation_valid_codes Confirmed valid codes for the sanitation
#' source variable.
#' @param garbage_var Garbage-disposal source variable.
#' @param garbage_collection_codes Garbage-disposal codes treated as collection
#' service. The default `2` follows the official-syntax profile where only
#' `vi13 == 2` is non-deprived.
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
  extreme_poverty_var = "epobre",
  extreme_poverty_income_var = NULL,
  extreme_poverty_line = NULL,
  build_from_precomputed = TRUE,
  water_public_network_codes = 1,
  extreme_poverty_alias_vars = c("epobreza", "expobre"),
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
  higher_education_reason_var = "p09",
  higher_education_economic_reason_codes = 3,
  bachillerato_completed_levels = NULL,
  bachillerato_completed_min_grade = NULL,
  child_work_var = "empleo",
  child_hours_var = "p24",
  child_income_var = "ingrl",
  p20_var = "p20",
  p21_var = "p21",
  p22_var = "p22",
  p24_var = "p24",
  pea_var = "pea",
  condactn_var = c("condactn", "condact"),
  p51_prefix = "p51",
  child_sbu = 470,
  employment_na_as_not_employed = TRUE,
  hours_sentinel_codes = 999,
  income_negative_sentinel_codes = -1,
  income_missing_sentinel_codes = 999999,
  social_security_var = "p61b1",
  social_security_a_var = "p05a",
  social_security_b_var = "p05b",
  pension_income_var = "p72a",
  human_development_bonus_var = "p75",
  disability_bonus_var = "p77",
  social_security_contribution_codes = c(1, 2, 3, 4),
  social_security_no_contribution_codes = 5,
  social_security_unknown_codes = 6,
  unemployment_var = "desempleo",
  unemployed_var = "desem",
  inactive_var = "pei",
  working_age_var = "pet",
  roof_material_var = "vi03a",
  roof_state_var = "vi03b",
  floor_material_var = "vi04a",
  floor_state_var = "vi04b",
  wall_material_var = "vi05a",
  wall_state_var = "vi05b",
  housing_material_valid_codes = NULL,
  housing_state_valid_codes = 1:3,
  bedrooms_zero_policy = c("official_recode_to_one", "deprived"),
  deficit_roof_material_codes = NULL,
  deficit_floor_material_codes = NULL,
  deficit_wall_material_codes = NULL,
  deficit_state_codes = 3,
  sanitation_var = "vi09",
  sanitation_urban_area_codes = 1,
  sanitation_rural_area_codes = 2,
  sanitation_sewer_codes = 1,
  sanitation_septic_codes = 2,
  sanitation_valid_codes = 1:5,
  garbage_var = "vi13",
  garbage_collection_codes = 2,
  garbage_valid_codes = 1:5
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
  if (!is.null(extreme_poverty_income_var)) {
    extreme_poverty_income_var <- .enemdu_ipm_single_var_name(
      extreme_poverty_income_var,
      "extreme_poverty_income_var",
      caller
    )
  }
  if (!is.null(extreme_poverty_line)) {
    extreme_poverty_line <- .enemdu_ipm_single_positive_number(
      extreme_poverty_line,
      "extreme_poverty_line"
    )
  }
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
  higher_education_reason_var <- .enemdu_ipm_single_var_name(
    higher_education_reason_var,
    "higher_education_reason_var",
    caller
  )
  child_work_var <- .enemdu_ipm_single_var_name(child_work_var, "child_work_var", caller)
  child_hours_var <- .enemdu_ipm_single_var_name(child_hours_var, "child_hours_var", caller)
  child_income_var <- .enemdu_ipm_single_var_name(child_income_var, "child_income_var", caller)
  p20_var <- .enemdu_ipm_single_var_name(p20_var, "p20_var", caller)
  p21_var <- .enemdu_ipm_single_var_name(p21_var, "p21_var", caller)
  p22_var <- .enemdu_ipm_single_var_name(p22_var, "p22_var", caller)
  p24_var <- .enemdu_ipm_single_var_name(p24_var, "p24_var", caller)
  pea_var <- .enemdu_ipm_single_var_name(pea_var, "pea_var", caller)
  condactn_var <- .enemdu_ipm_var_names(condactn_var, "condactn_var")
  p51_prefix <- .enemdu_ipm_single_value(p51_prefix, "p51_prefix")
  employment_na_as_not_employed <- .enemdu_ipm_single_logical(
    employment_na_as_not_employed,
    "employment_na_as_not_employed"
  )
  social_security_var <- .enemdu_ipm_single_var_name(
    social_security_var,
    "social_security_var",
    caller
  )
  social_security_a_var <- .enemdu_ipm_single_var_name(
    social_security_a_var,
    "social_security_a_var",
    caller
  )
  social_security_b_var <- .enemdu_ipm_single_var_name(
    social_security_b_var,
    "social_security_b_var",
    caller
  )
  pension_income_var <- .enemdu_ipm_single_var_name(
    pension_income_var,
    "pension_income_var",
    caller
  )
  human_development_bonus_var <- .enemdu_ipm_single_var_name(
    human_development_bonus_var,
    "human_development_bonus_var",
    caller
  )
  disability_bonus_var <- .enemdu_ipm_single_var_name(
    disability_bonus_var,
    "disability_bonus_var",
    caller
  )
  unemployment_var <- .enemdu_ipm_single_var_name(unemployment_var, "unemployment_var", caller)
  unemployed_var <- .enemdu_ipm_single_var_name(unemployed_var, "unemployed_var", caller)
  inactive_var <- .enemdu_ipm_single_var_name(inactive_var, "inactive_var", caller)
  working_age_var <- .enemdu_ipm_single_var_name(working_age_var, "working_age_var", caller)
  roof_material_var <- .enemdu_ipm_single_var_name(roof_material_var, "roof_material_var", caller)
  roof_state_var <- .enemdu_ipm_single_var_name(roof_state_var, "roof_state_var", caller)
  floor_material_var <- .enemdu_ipm_single_var_name(floor_material_var, "floor_material_var", caller)
  floor_state_var <- .enemdu_ipm_single_var_name(floor_state_var, "floor_state_var", caller)
  wall_material_var <- .enemdu_ipm_single_var_name(wall_material_var, "wall_material_var", caller)
  wall_state_var <- .enemdu_ipm_single_var_name(wall_state_var, "wall_state_var", caller)
  sanitation_var <- .enemdu_ipm_single_var_name(sanitation_var, "sanitation_var", caller)
  garbage_var <- .enemdu_ipm_single_var_name(garbage_var, "garbage_var", caller)
  higher_education_economic_reason_codes <- .enemdu_ipm_optional_code_set(
    higher_education_economic_reason_codes,
    "higher_education_economic_reason_codes"
  )
  bachillerato_completed_levels <- .enemdu_ipm_optional_code_set(
    bachillerato_completed_levels,
    "bachillerato_completed_levels"
  )
  bachillerato_completed_min_grade <- .enemdu_ipm_optional_code_set(
    bachillerato_completed_min_grade,
    "bachillerato_completed_min_grade"
  )
  hours_sentinel_codes <- .enemdu_ipm_optional_code_set(
    hours_sentinel_codes,
    "hours_sentinel_codes"
  )
  income_negative_sentinel_codes <- .enemdu_ipm_optional_code_set(
    income_negative_sentinel_codes,
    "income_negative_sentinel_codes"
  )
  income_missing_sentinel_codes <- .enemdu_ipm_optional_code_set(
    income_missing_sentinel_codes,
    "income_missing_sentinel_codes"
  )
  social_security_contribution_codes <- .enemdu_ipm_validate_code_set(
    social_security_contribution_codes,
    "social_security_contribution_codes"
  )
  social_security_no_contribution_codes <- .enemdu_ipm_validate_code_set(
    social_security_no_contribution_codes,
    "social_security_no_contribution_codes"
  )
  social_security_unknown_codes <- .enemdu_ipm_validate_code_set(
    social_security_unknown_codes,
    "social_security_unknown_codes"
  )
  housing_material_valid_codes <- .enemdu_ipm_optional_code_set(
    housing_material_valid_codes,
    "housing_material_valid_codes"
  )
  housing_state_valid_codes <- .enemdu_ipm_validate_code_set(
    housing_state_valid_codes,
    "housing_state_valid_codes"
  )
  bedrooms_zero_policy <- match.arg(bedrooms_zero_policy)
  deficit_roof_material_codes <- .enemdu_ipm_optional_code_set(
    deficit_roof_material_codes,
    "deficit_roof_material_codes"
  )
  deficit_floor_material_codes <- .enemdu_ipm_optional_code_set(
    deficit_floor_material_codes,
    "deficit_floor_material_codes"
  )
  deficit_wall_material_codes <- .enemdu_ipm_optional_code_set(
    deficit_wall_material_codes,
    "deficit_wall_material_codes"
  )
  deficit_state_codes <- .enemdu_ipm_validate_code_set(
    deficit_state_codes,
    "deficit_state_codes"
  )
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
  critical_missing <- list()

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
    bedrooms_zero_policy = bedrooms_zero_policy,
    overwrite = overwrite,
    strict = strict
  )
  out <- low_risk$data
  built_components <- unique(c(built_components, low_risk$built_components))
  pending_components <- unique(c(pending_components, low_risk$pending_components))
  pending_reasons <- c(pending_reasons, low_risk$pending_reasons)
  variables_used <- c(variables_used, low_risk$variables_used)
  critical_missing <- c(critical_missing, low_risk$critical_missing %||% list())

  extreme_poverty <- .enemdu_build_ipm_extreme_poverty_component(
    data = out,
    component_var = component_vars[["ipm_i07_pobreza_extrema_ingresos"]],
    extreme_poverty_var = extreme_poverty_var,
    extreme_poverty_income_var = extreme_poverty_income_var,
    extreme_poverty_line = extreme_poverty_line,
    extreme_poverty_alias_vars = extreme_poverty_alias_vars,
    overwrite = overwrite,
    strict = strict
  )
  out <- extreme_poverty$data
  built_components <- unique(c(built_components, extreme_poverty$built_components))
  pending_components <- unique(c(pending_components, extreme_poverty$pending_components))
  pending_reasons <- c(pending_reasons, extreme_poverty$pending_reasons)
  variables_used <- c(variables_used, extreme_poverty$variables_used)
  critical_missing <- c(critical_missing, extreme_poverty$critical_missing %||% list())

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
    education_level_var = education_level_var,
    education_grade_var = education_grade_var,
    overwrite = overwrite,
    strict = strict
  )
  out <- school_attendance$data
  built_components <- unique(c(built_components, school_attendance$built_components))
  pending_components <- unique(c(pending_components, school_attendance$pending_components))
  pending_reasons <- c(pending_reasons, school_attendance$pending_reasons)
  variables_used <- c(variables_used, school_attendance$variables_used)
  critical_missing <- c(critical_missing, school_attendance$critical_missing %||% list())

  higher_education_access <- .enemdu_build_ipm_higher_education_access_component(
    data = out,
    component_var = component_vars[["ipm_i02_no_acceso_superior_economico"]],
    household_id = household_id,
    age_var = age_var,
    attendance_var = attendance_var,
    attendance_yes_codes = attendance_yes_codes,
    attendance_no_codes = attendance_no_codes,
    higher_education_reason_var = higher_education_reason_var,
    higher_education_economic_reason_codes = higher_education_economic_reason_codes,
    education_level_var = education_level_var,
    education_grade_var = education_grade_var,
    bachillerato_completed_levels = bachillerato_completed_levels,
    bachillerato_completed_min_grade = bachillerato_completed_min_grade,
    overwrite = overwrite,
    strict = strict
  )
  out <- higher_education_access$data
  built_components <- unique(c(built_components, higher_education_access$built_components))
  pending_components <- unique(c(pending_components, higher_education_access$pending_components))
  pending_reasons <- c(pending_reasons, higher_education_access$pending_reasons)
  variables_used <- c(variables_used, higher_education_access$variables_used)
  critical_missing <- c(critical_missing, higher_education_access$critical_missing %||% list())

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
    schooling_unknown_policy = .enemdu_ipm_schooling_unknown_policy(profile),
    overwrite = overwrite,
    strict = strict
  )
  out <- incomplete_education$data
  built_components <- unique(c(built_components, incomplete_education$built_components))
  pending_components <- unique(c(pending_components, incomplete_education$pending_components))
  pending_reasons <- c(pending_reasons, incomplete_education$pending_reasons)
  variables_used <- c(variables_used, incomplete_education$variables_used)
  critical_missing <- c(critical_missing, incomplete_education$critical_missing %||% list())

  child_adolescent_employment <- .enemdu_build_ipm_child_adolescent_employment_component(
    data = out,
    component_var = component_vars[["ipm_i04_empleo_infantil_adolescente"]],
    household_id = household_id,
    age_var = age_var,
    attendance_var = attendance_var,
    attendance_yes_codes = attendance_yes_codes,
    attendance_no_codes = attendance_no_codes,
    child_work_var = child_work_var,
    child_hours_var = child_hours_var,
    child_income_var = child_income_var,
    p20_var = p20_var,
    p21_var = p21_var,
    p22_var = p22_var,
    p24_var = p24_var,
    pea_var = pea_var,
    condactn_var = condactn_var,
    unemployment_var = unemployment_var,
    p51_prefix = p51_prefix,
    child_sbu = child_sbu,
    employment_na_as_not_employed = employment_na_as_not_employed,
    hours_sentinel_codes = hours_sentinel_codes,
    income_negative_sentinel_codes = income_negative_sentinel_codes,
    income_missing_sentinel_codes = income_missing_sentinel_codes,
    overwrite = overwrite,
    strict = strict
  )
  out <- child_adolescent_employment$data
  built_components <- unique(c(built_components, child_adolescent_employment$built_components))
  pending_components <- unique(c(pending_components, child_adolescent_employment$pending_components))
  pending_reasons <- c(pending_reasons, child_adolescent_employment$pending_reasons)
  variables_used <- c(variables_used, child_adolescent_employment$variables_used)
  critical_missing <- c(critical_missing, child_adolescent_employment$critical_missing %||% list())

  labor_inadequate <- .enemdu_build_ipm_labor_inadequate_component(
    data = out,
    component_var = component_vars[["ipm_i05_desempleo_empleo_inadecuado"]],
    household_id = household_id,
    age_var = age_var,
    condact_var = condact_var,
    condactn_var = condactn_var,
    labor_block_vars = c(p20_var, p21_var, p22_var, "p32", "p34", "p35"),
    labor_inadequate_flags = labor_inadequate_flags,
    overwrite = overwrite,
    strict = strict
  )
  out <- labor_inadequate$data
  built_components <- unique(c(built_components, labor_inadequate$built_components))
  pending_components <- unique(c(pending_components, labor_inadequate$pending_components))
  pending_reasons <- c(pending_reasons, labor_inadequate$pending_reasons)
  variables_used <- c(variables_used, labor_inadequate$variables_used)
  critical_missing <- c(critical_missing, labor_inadequate$critical_missing %||% list())

  pension_contribution <- .enemdu_build_ipm_pension_contribution_component(
    data = out,
    component_var = component_vars[["ipm_i06_no_contribucion_pensiones"]],
    household_id = household_id,
    age_var = age_var,
    employment_var = child_work_var,
    unemployment_var = unemployment_var,
    social_security_var = social_security_var,
    social_security_a_var = social_security_a_var,
    social_security_b_var = social_security_b_var,
    pension_income_var = pension_income_var,
    human_development_bonus_var = human_development_bonus_var,
    disability_bonus_var = disability_bonus_var,
    pea_var = pea_var,
    unemployed_var = unemployed_var,
    inactive_var = inactive_var,
    working_age_var = working_age_var,
    condactn_var = condactn_var,
    social_security_contribution_codes = social_security_contribution_codes,
    social_security_no_contribution_codes = social_security_no_contribution_codes,
    social_security_unknown_codes = social_security_unknown_codes,
    employment_na_as_not_employed = employment_na_as_not_employed,
    overwrite = overwrite,
    strict = strict
  )
  out <- pension_contribution$data
  built_components <- unique(c(built_components, pension_contribution$built_components))
  pending_components <- unique(c(pending_components, pension_contribution$pending_components))
  pending_reasons <- c(pending_reasons, pension_contribution$pending_reasons)
  variables_used <- c(variables_used, pension_contribution$variables_used)
  critical_missing <- c(critical_missing, pension_contribution$critical_missing %||% list())

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
  critical_missing <- c(critical_missing, sanitation$critical_missing %||% list())

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
  critical_missing <- c(critical_missing, garbage$critical_missing %||% list())

  housing_deficit <- .enemdu_build_ipm_housing_deficit_component(
    data = out,
    component_var = component_vars[["ipm_i10_deficit_habitacional"]],
    household_id = household_id,
    roof_material_var = roof_material_var,
    roof_state_var = roof_state_var,
    floor_material_var = floor_material_var,
    floor_state_var = floor_state_var,
    wall_material_var = wall_material_var,
    wall_state_var = wall_state_var,
    housing_material_valid_codes = housing_material_valid_codes,
    housing_state_valid_codes = housing_state_valid_codes,
    deficit_roof_material_codes = deficit_roof_material_codes,
    deficit_floor_material_codes = deficit_floor_material_codes,
    deficit_wall_material_codes = deficit_wall_material_codes,
    deficit_state_codes = deficit_state_codes,
    overwrite = overwrite,
    strict = strict
  )
  out <- housing_deficit$data
  built_components <- unique(c(built_components, housing_deficit$built_components))
  pending_components <- unique(c(pending_components, housing_deficit$pending_components))
  pending_reasons <- c(pending_reasons, housing_deficit$pending_reasons)
  variables_used <- c(variables_used, housing_deficit$variables_used)
  critical_missing <- c(critical_missing, housing_deficit$critical_missing %||% list())

  unavailable_indicator_ids <- setdiff(
    names(component_vars),
    c(
      "ipm_i01_inasistencia_basica_bach",
      "ipm_i02_no_acceso_superior_economico",
      "ipm_i03_logro_educativo_incompleto",
      "ipm_i04_empleo_infantil_adolescente",
      "ipm_i05_desempleo_empleo_inadecuado",
      "ipm_i06_no_contribucion_pensiones",
      "ipm_i07_pobreza_extrema_ingresos",
      "ipm_i08_sin_agua_red_publica",
      "ipm_i09_hacinamiento",
      "ipm_i10_deficit_habitacional",
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

  critical_missing_info <- .enemdu_ipm_critical_missing_info(
    critical_missing = critical_missing,
    component_vars = component_vars,
    household_id = out[[household_id]]
  )
  out[["ipm_missing_critical_flag"]] <- critical_missing_info$person_flag
  out[["ipm_missing_critical_household_flag"]] <- critical_missing_info$household_flag

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
    critical_missing_by_component = critical_missing_info$by_component,
    critical_missing_households = critical_missing_info$households_n,
    critical_missing_person_rows = critical_missing_info$person_rows_n,
    critical_missing_flags = c(
      "ipm_missing_critical_flag",
      "ipm_missing_critical_household_flag"
    ),
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

.enemdu_ipm_schooling_unknown_policy <- function(profile) {
  if (identical(profile, "enemdu_2025_anual")) {
    return("official_recode_to_zero")
  }

  "preserve_na"
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
                                                             bedrooms_zero_policy,
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

  bedrooms <- .enemdu_ipm_coerce_source_numeric(temp[["vi07"]], "vi07")
  hsize <- if (hsize_var %in% names(temp)) {
    .enemdu_ipm_coerce_source_numeric(temp[[hsize_var]], hsize_var)
  } else {
    .enemdu_ipm_make_hsize(temp[[household_id]], temp[[person_id]])
  }
  bedrooms_zero_n <- sum(!is.na(bedrooms) & bedrooms == 0)

  if (identical(bedrooms_zero_policy, "official_recode_to_one")) {
    bedrooms[!is.na(bedrooms) & bedrooms == 0] <- 1
  }

  temp[[component_vars[["ipm_i09_hacinamiento"]]]] <-
    .enemdu_ipm_overcrowding_component(hsize = hsize, bedrooms = bedrooms)

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
        water_public_network_codes = water_public_network_codes,
        bedrooms_zero_policy = bedrooms_zero_policy,
        bedrooms_zero_n = bedrooms_zero_n,
        rule_status = "official_syntax_household_rule"
      )
    ),
    critical_missing = list(
      ipm_i08_sin_agua_red_publica = is.na(temp[["vi10"]]),
      ipm_i09_hacinamiento = is.na(hsize) | is.na(temp[["vi07"]])
    )
  )
}

.enemdu_build_ipm_extreme_poverty_component <- function(data,
                                                        component_var,
                                                        extreme_poverty_var,
                                                        extreme_poverty_income_var,
                                                        extreme_poverty_line,
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
    strict = FALSE
  )
  fallback_requested <- !is.null(extreme_poverty_income_var) &&
    !is.null(extreme_poverty_line)
  fallback_available <- fallback_requested &&
    extreme_poverty_income_var %in% names(out)
  fallback_filled_n <- 0L
  fallback_missing_income_n <- NA_integer_

  if (isTRUE(fallback_available)) {
    missing_flag <- is.na(out[[component_var]])
    fallback_missing_income_n <- 0L

    if (any(missing_flag)) {
      income_subset <- .enemdu_ipm_coerce_source_numeric(
        out[[extreme_poverty_income_var]][missing_flag],
        extreme_poverty_income_var
      )
      fillable <- !is.na(income_subset)
      component_subset <- out[[component_var]][missing_flag]
      component_subset[fillable] <- as.integer(
        income_subset[fillable] < extreme_poverty_line
      )
      out[[component_var]][missing_flag] <- component_subset
      fallback_filled_n <- sum(fillable)
      fallback_missing_income_n <- sum(is.na(income_subset))
    }
  }

  .enemdu_ipm_abort_component_missing_if_strict(
    component = out[[component_var]],
    component_var = component_var,
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
        fallback_income_var = if (!is.null(extreme_poverty_income_var)) {
          extreme_poverty_income_var
        } else {
          NA_character_
        },
        fallback_line = if (!is.null(extreme_poverty_line)) {
          extreme_poverty_line
        } else {
          NA_real_
        },
        fallback_requested = fallback_requested,
        fallback_available = fallback_available,
        fallback_filled_n = fallback_filled_n,
        fallback_missing_income_n = fallback_missing_income_n,
        still_missing_n = sum(is.na(out[[component_var]])),
        output_component = component_var,
        rule = "copy_precomputed_binary_extreme_poverty_flag_with_optional_income_line_fallback",
        rule_status = if (isTRUE(fallback_available)) {
          "direct_official_flag_with_explicit_non_official_income_fallback"
        } else {
          "official_syntax_direct_flag"
        },
        official_validation_status = "not_officially_validated"
      )
    ),
    critical_missing = list(
      ipm_i07_pobreza_extrema_ingresos = is.na(out[[component_var]])
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
                                                          education_level_var,
                                                          education_grade_var,
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
        "Missing source variables for school attendance: ",
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
  yes_codes <- .enemdu_ipm_validate_code_set(attendance_yes_codes, "attendance_yes_codes")
  no_codes <- .enemdu_ipm_validate_code_set(attendance_no_codes, "attendance_no_codes")

  valid_attendance_codes <- unique(c(yes_codes, no_codes))
  invalid_attendance <- !is.na(attendance) & !attendance %in% valid_attendance_codes

  if (any(invalid_attendance) && isTRUE(strict)) {
    .enemdu_abort_invalid_ipm_source_codes(attendance_var, valid_attendance_codes)
  }

  person_deprivation <- rep(0L, length(age))
  applicable <- !is.na(age) & age >= school_age_min & age <= school_age_max
  basic_age <- !is.na(age) & age >= 5 & age <= 14
  bach_age <- !is.na(age) & age >= 15 & age <= 17
  attending <- !is.na(attendance) & !invalid_attendance & attendance %in% yes_codes

  asist_basica <- basic_age & attending & (
    education_level %in% c(1, 3) |
      (education_level == 4 & education_grade %in% 0:6) |
      (education_level == 5 & education_grade %in% 0:9) |
      (education_level == 6 & education_grade %in% 0:2)
  )
  asist_bach <- bach_age & attending & (
    (education_level == 5 & education_grade == 10) |
      (education_level == 7 & education_grade %in% 0:2) |
      (education_level == 6 & education_grade %in% 3:5)
  )
  critical_missing <- applicable & (is.na(attendance) | is.na(education_level))

  person_deprivation[is.na(age)] <- NA_integer_
  person_deprivation[applicable & !is.na(education_level) & education_level < 8] <- 1L
  person_deprivation[applicable & (asist_basica | asist_bach)] <- 0L
  person_deprivation[applicable & !is.na(education_level) & education_level >= 8] <- 0L
  person_deprivation[critical_missing | (applicable & invalid_attendance)] <- NA_integer_

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
        source_vars = c(
          household_id,
          age_var,
          attendance_var,
          education_level_var,
          education_grade_var
        ),
        attendance_yes_codes = yes_codes,
        attendance_no_codes = no_codes,
        applicable_age_range = c(school_age_min, school_age_max),
        critical_missing_n = sum(critical_missing),
        output_component = component_var,
        rule_status = "official_syntax_rule"
      )
    ),
    critical_missing = list(
      ipm_i01_inasistencia_basica_bach = critical_missing | (applicable & invalid_attendance)
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
                                                             schooling_unknown_policy,
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

  schooling_info <- .enemdu_ipm_schooling_years_2025_info(
    education_level = education_level,
    education_grade = education_grade,
    unknown_policy = schooling_unknown_policy
  )
  schooling_years <- schooling_info$years

  person_deprivation <- rep(0L, length(age))
  applicable <- !is.na(age) &
    age >= incomplete_education_age_min &
    age <= incomplete_education_age_max
  invalid_attendance <- !is.na(attendance) & !(attendance %in% c(1, 2))
  attending <- !is.na(attendance) & attendance == 1
  not_attending <- !is.na(attendance) & attendance %in% no_codes

  person_deprivation[is.na(age)] <- NA_integer_
  person_deprivation[
    applicable & not_attending & !is.na(schooling_years) &
      schooling_years < incomplete_schooling_years
  ] <- 1L
  person_deprivation[
    applicable & !is.na(schooling_years) &
      ((schooling_years < incomplete_schooling_years & attending) |
        schooling_years >= incomplete_schooling_years)
  ] <- 0L

  critical_missing <- applicable & (is.na(attendance) | is.na(schooling_years))
  person_deprivation[critical_missing | (applicable & invalid_attendance)] <- NA_integer_

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
        schooling_unknown_policy = schooling_unknown_policy,
        schooling_unmatched_converted_to_zero_n = sum(
          schooling_info$unmatched_observed &
            schooling_info$unknown_converted_to_zero
        ),
        schooling_unmatched_observed_n = sum(schooling_info$unmatched_observed),
        schooling_missing_inputs_n = sum(schooling_info$missing_inputs),
        schooling_unknown_converted_to_zero_n = sum(schooling_info$unknown_converted_to_zero),
        schooling_missing_converted_to_zero_n = sum(schooling_info$missing_converted_to_zero),
        schooling_missing_required_grade_n = sum(schooling_info$missing_required_grade),
        critical_missing_n = sum(critical_missing),
        output_component = component_var,
        rule_status = "official_syntax_rule",
        note = paste(
          "Schooling years use the official 2025 p10a/p10b formula.",
          "Under the official profile, unassigned schooling is recoded to zero following official syntax.",
          "Conversion counts are reported for auditability.",
          "This is profile-specific and must be compared against benchmarks before validation claims."
        )
      )
    ),
    critical_missing = list(
      ipm_i03_logro_educativo_incompleto =
        critical_missing | (applicable & invalid_attendance)
    )
  )
}

.enemdu_build_ipm_higher_education_access_component <- function(data,
                                                                 component_var,
                                                                 household_id,
                                                                 age_var,
                                                                 attendance_var,
                                                                 attendance_yes_codes,
                                                                 attendance_no_codes,
                                                                 higher_education_reason_var,
                                                                 higher_education_economic_reason_codes,
                                                                 education_level_var,
                                                                 education_grade_var,
                                                                 bachillerato_completed_levels,
                                                                 bachillerato_completed_min_grade,
                                                                 overwrite,
                                                                 strict) {
  if (component_var %in% names(data) && !isTRUE(overwrite)) {
    return(.enemdu_ipm_noop_component_result(data))
  }

  required_vars <- c(
    household_id,
    age_var,
    attendance_var,
    higher_education_reason_var,
    education_level_var,
    education_grade_var
  )
  missing_vars <- setdiff(required_vars, names(data))

  if (length(missing_vars) > 0) {
    return(.enemdu_ipm_pending_component_result(
      data = data,
      component_var = component_var,
      reason = paste0(
        "Missing source variables for higher-education access: ",
        paste(missing_vars, collapse = ", "),
        "."
      )
    ))
  }

  economic_reason_codes <- higher_education_economic_reason_codes
  economic_reason_source <- "argument"

  if (length(economic_reason_codes) == 0) {
    economic_reason_codes <- .enemdu_ipm_infer_codes_from_labels(
      values = data[[higher_education_reason_var]],
      patterns = c("econ", "recurso", "dinero", "falta.*recurso")
    )
    economic_reason_source <- "value_labels"
  }

  if (length(economic_reason_codes) == 0) {
    return(.enemdu_ipm_pending_component_result(
      data = data,
      component_var = component_var,
      reason = paste0(
        "Economic reason codes for higher-education access were not supplied ",
        "and could not be inferred from source value labels."
      )
    ))
  }

  age <- .enemdu_ipm_coerce_source_numeric(data[[age_var]], age_var)
  attendance <- .enemdu_ipm_coerce_source_numeric(data[[attendance_var]], attendance_var)
  reason <- .enemdu_ipm_coerce_source_numeric(
    data[[higher_education_reason_var]],
    higher_education_reason_var
  )
  education_level <- .enemdu_ipm_coerce_source_numeric(
    data[[education_level_var]],
    education_level_var
  )
  education_grade <- .enemdu_ipm_coerce_source_numeric(
    data[[education_grade_var]],
    education_grade_var
  )

  yes_codes <- .enemdu_ipm_validate_code_set(attendance_yes_codes, "attendance_yes_codes")
  no_codes <- .enemdu_ipm_validate_code_set(attendance_no_codes, "attendance_no_codes")
  valid_attendance_codes <- unique(c(yes_codes, no_codes))
  invalid_attendance <- !is.na(attendance) & !(attendance %in% valid_attendance_codes)

  applicable <- !is.na(age) & age >= 18 & age <= 29

  if (any(applicable & invalid_attendance) && isTRUE(strict)) {
    .enemdu_abort_invalid_ipm_source_codes(attendance_var, valid_attendance_codes)
  }

  if (length(bachillerato_completed_levels) > 0) {
    bachillerato_completed <- .enemdu_ipm_bachillerato_completed_2025(
      education_level = education_level,
      education_grade = education_grade,
      completed_levels = bachillerato_completed_levels,
      completed_min_grade = bachillerato_completed_min_grade
    )
    bachillerato_completed_for_deprivation <- bachillerato_completed
  } else {
    bachillerato_completed <- .enemdu_ipm_official_bachillerato_completed(
      education_level = education_level,
      education_grade = education_grade,
      include_postgraduate = TRUE
    )
    bachillerato_completed_for_deprivation <- .enemdu_ipm_official_bachillerato_completed(
      education_level = education_level,
      education_grade = education_grade,
      include_postgraduate = FALSE
    )
  }

  not_attending <- applicable & !is.na(attendance) & attendance %in% no_codes
  person_deprivation <- rep(0L, length(age))
  critical_missing <- applicable & (is.na(attendance) | is.na(education_level))
  person_deprivation[is.na(age)] <- NA_integer_
  person_deprivation[critical_missing | (applicable & invalid_attendance)] <- NA_integer_
  person_deprivation[not_attending & is.na(bachillerato_completed_for_deprivation)] <-
    NA_integer_

  candidate <- not_attending & bachillerato_completed_for_deprivation %in% TRUE
  person_deprivation[candidate & is.na(reason)] <- NA_integer_

  observed_candidate <- candidate & !is.na(reason)
  person_deprivation[observed_candidate] <- as.integer(
    reason[observed_candidate] %in% economic_reason_codes
  )

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
      higher_education_access = list(
        source_vars = c(
          household_id,
          age_var,
          attendance_var,
          higher_education_reason_var,
          education_level_var,
          education_grade_var
        ),
        applicable_age_range = c(18, 29),
        attendance_yes_codes = yes_codes,
        attendance_no_codes = no_codes,
        economic_reason_codes = economic_reason_codes,
        economic_reason_source = economic_reason_source,
        bachillerato_completed_levels = bachillerato_completed_levels,
        bachillerato_completed_min_grade = bachillerato_completed_min_grade,
        critical_missing_n = sum(critical_missing),
        output_component = component_var,
        rule_status = "official_syntax_rule"
      )
    ),
    critical_missing = list(
      ipm_i02_no_acceso_superior_economico =
        critical_missing | (applicable & invalid_attendance)
    )
  )
}

.enemdu_build_ipm_child_adolescent_employment_component <- function(data,
                                                                    component_var,
                                                                    household_id,
                                                                    age_var,
                                                                    attendance_var,
                                                                    attendance_yes_codes,
                                                                    attendance_no_codes,
                                                                    child_work_var,
                                                                    child_hours_var,
                                                                    child_income_var,
                                                                    p20_var,
                                                                    p21_var,
                                                                    p22_var,
                                                                    p24_var,
                                                                    pea_var,
                                                                    condactn_var,
                                                                    unemployment_var,
                                                                    p51_prefix,
                                                                    child_sbu,
                                                                    employment_na_as_not_employed,
                                                                    hours_sentinel_codes,
                                                                    income_negative_sentinel_codes,
                                                                    income_missing_sentinel_codes,
                                                                    overwrite,
                                                                    strict) {
  if (component_var %in% names(data) && !isTRUE(overwrite)) {
    return(.enemdu_ipm_noop_component_result(data))
  }

  condactn_source_var <- .enemdu_ipm_first_existing_var(data, condactn_var)
  official_like_required_vars <- c(
    household_id,
    age_var,
    attendance_var,
    child_work_var,
    p20_var,
    p21_var,
    p22_var,
    p24_var,
    condactn_source_var
  )
  official_like_required_vars <- official_like_required_vars[
    !is.na(official_like_required_vars)
  ]
  official_like_available <- !is.na(condactn_source_var) &&
    all(official_like_required_vars %in% names(data))
  pea_available <- pea_var %in% names(data)
  pea_derived_available <- !isTRUE(pea_available) &&
    all(c(child_work_var, unemployment_var) %in% names(data))

  if (isTRUE(official_like_available)) {
    if (isTRUE(pea_available)) {
      pea_source <- "provided_pea"
      rule_status <- "official_syntax_rule"
    } else if (isTRUE(pea_derived_available)) {
      pea_source <- "derived_from_empleo_desempleo"
      rule_status <- "official_like_with_derived_pea"
    } else {
      pea_source <- "not_available_without_gate"
      rule_status <- "official_like_without_pea_gate"
    }
    missing_official_source_vars <- setdiff(
      c(official_like_required_vars, pea_var),
      names(data)
    )
    if (!isTRUE(pea_available) && !isTRUE(pea_derived_available)) {
      missing_official_source_vars <- unique(c(
        missing_official_source_vars,
        setdiff(unemployment_var, names(data))
      ))
    }

    return(.enemdu_build_ipm_child_adolescent_employment_official_component(
      data = data,
      component_var = component_var,
      household_id = household_id,
      age_var = age_var,
      attendance_var = attendance_var,
      child_work_var = child_work_var,
      p20_var = p20_var,
      p21_var = p21_var,
      p22_var = p22_var,
      p24_var = p24_var,
      pea_var = pea_var,
      unemployment_var = unemployment_var,
      condactn_var = condactn_source_var,
      pea_source = pea_source,
      rule_status = rule_status,
      missing_official_source_vars = missing_official_source_vars,
      p51_prefix = p51_prefix,
      overwrite = overwrite,
      strict = strict
    ))
  }

  required_vars <- c(
    household_id,
    age_var,
    attendance_var,
    child_work_var,
    child_hours_var,
    child_income_var
  )
  missing_vars <- setdiff(required_vars, names(data))

  if (length(missing_vars) > 0) {
    return(.enemdu_ipm_pending_component_result(
      data = data,
      component_var = component_var,
      reason = paste0(
        "Missing source variables for child and adolescent employment: ",
        paste(missing_vars, collapse = ", "),
        "."
      )
    ))
  }

  child_sbu <- .enemdu_ipm_single_positive_number(child_sbu, "child_sbu")
  age <- .enemdu_ipm_coerce_source_numeric(data[[age_var]], age_var)
  attendance <- .enemdu_ipm_coerce_source_numeric(data[[attendance_var]], attendance_var)
  employment <- .enemdu_ipm_coerce_source_numeric(data[[child_work_var]], child_work_var)
  hours <- .enemdu_ipm_coerce_source_numeric(data[[child_hours_var]], child_hours_var)
  income <- .enemdu_ipm_coerce_source_numeric(data[[child_income_var]], child_income_var)

  yes_codes <- .enemdu_ipm_validate_code_set(attendance_yes_codes, "attendance_yes_codes")
  no_codes <- .enemdu_ipm_validate_code_set(attendance_no_codes, "attendance_no_codes")
  valid_attendance_codes <- unique(c(yes_codes, no_codes))
  invalid_attendance <- !is.na(attendance) & !(attendance %in% valid_attendance_codes)

  child <- !is.na(age) & age >= 5 & age <= 14
  adolescent <- !is.na(age) & age >= 15 & age <= 17
  applicable <- child | adolescent
  invalid_employment <- !is.na(employment) & !(employment %in% c(0, 1))
  working <- !is.na(employment) & !invalid_employment & employment == 1
  not_working <- (!is.na(employment) & !invalid_employment & employment == 0) |
    (is.na(employment) & isTRUE(employment_na_as_not_employed))
  employment_unknown <- is.na(employment) & !isTRUE(employment_na_as_not_employed)
  adolescent_working <- adolescent & working

  invalid_hours <- !is.na(hours) & (
    hours < 0 |
      hours %in% hours_sentinel_codes
  )
  invalid_income <- !is.na(income) & (
    income < 0 |
      income %in% c(income_negative_sentinel_codes, income_missing_sentinel_codes)
  )

  if (any(applicable & invalid_employment) && isTRUE(strict)) {
    .enemdu_abort_invalid_ipm_source_codes(child_work_var, c(0, 1))
  }

  person_deprivation <- rep(0L, length(age))
  person_deprivation[is.na(age)] <- NA_integer_
  person_deprivation[applicable & (employment_unknown | invalid_employment)] <- NA_integer_

  person_deprivation[child & working] <- 1L
  person_deprivation[child & not_working] <- 0L

  not_attending <- !is.na(attendance) & attendance %in% no_codes
  long_hours <- !is.na(hours) & !invalid_hours & hours > 30
  low_income <- !is.na(income) & !invalid_income & income < child_sbu

  adolescent_deprived <- adolescent_working & (not_attending | long_hours | low_income)
  unknown_attendance_needed <- adolescent_working &
    !adolescent_deprived &
    (is.na(attendance) | invalid_attendance)
  unknown_hours_needed <- adolescent_working &
    !adolescent_deprived &
    (is.na(hours) | invalid_hours)
  unknown_income_needed <- adolescent_working &
    !adolescent_deprived &
    (is.na(income) | invalid_income)
  unknown_adolescent <- unknown_attendance_needed |
    unknown_hours_needed |
    unknown_income_needed

  if (any(unknown_attendance_needed & invalid_attendance) && isTRUE(strict)) {
    .enemdu_abort_invalid_ipm_source_codes(attendance_var, valid_attendance_codes)
  }
  if (any(unknown_hours_needed & invalid_hours) && isTRUE(strict)) {
    .enemdu_abort_invalid_ipm_source_values(child_hours_var)
  }
  if (any(unknown_income_needed & invalid_income) && isTRUE(strict)) {
    .enemdu_abort_invalid_ipm_source_values(child_income_var)
  }

  person_deprivation[adolescent & not_working] <- 0L
  person_deprivation[adolescent_deprived] <- 1L
  person_deprivation[adolescent_working & !adolescent_deprived & unknown_adolescent] <-
    NA_integer_
  person_deprivation[adolescent_working & !adolescent_deprived & !unknown_adolescent] <-
    0L

  component <- .enemdu_ipm_household_max_complete(
    household_id = data[[household_id]],
    values = person_deprivation
  )
  household_na_n <- sum(is.na(component))
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
      child_adolescent_employment = list(
        source_vars = c(
          household_id,
          age_var,
          attendance_var,
          child_work_var,
          child_hours_var,
          child_income_var
        ),
        child_age_range = c(5, 14),
        adolescent_age_range = c(15, 17),
        attendance_no_codes = no_codes,
        child_sbu = child_sbu,
        hours_sentinel_codes = hours_sentinel_codes,
        income_sentinel_codes = c(income_negative_sentinel_codes, income_missing_sentinel_codes),
        employment_na_as_not_employed = isTRUE(employment_na_as_not_employed),
        employment_na_treated_as_not_employed_n = sum(
          applicable & is.na(employment) & isTRUE(employment_na_as_not_employed)
        ),
        remaining_unknown_employment_cases_n = sum(
          applicable & (employment_unknown | invalid_employment)
        ),
        applicable_persons_n = sum(applicable),
        working_adolescents_unknown_hours_n = sum(
          adolescent_working & (is.na(hours) | invalid_hours)
        ),
        working_adolescents_unknown_income_n = sum(
          adolescent_working & (is.na(income) | invalid_income)
        ),
        household_na_n = household_na_n,
        output_component = component_var,
        rule_status = "proxy_fallback_not_official_syntax",
        missing_official_source_vars = setdiff(
          c(official_like_required_vars, pea_var),
          names(data)
        )
      )
    )
  )
}

.enemdu_ipm_i04_pea_source <- function(age,
                                       employment,
                                       unemployment,
                                       data,
                                       pea_var,
                                       pea_source) {
  pet_derived <- rep(0L, length(age))
  pet_derived[is.na(age) | age == 99] <- NA_integer_
  pet_derived[!is.na(age) & age >= 15 & age <= 98] <- 1L

  if (identical(pea_source, "provided_pea")) {
    pea <- .enemdu_ipm_coerce_source_numeric(data[[pea_var]], pea_var)
    return(list(
      pea = pea,
      pet_derived_n = as.integer(sum(pet_derived == 1L, na.rm = TRUE)),
      pea_derived_n = NA_integer_,
      pea_derived_missing_n = NA_integer_
    ))
  }

  if (identical(pea_source, "derived_from_empleo_desempleo")) {
    pea <- rep(0L, length(age))
    pea[is.na(pet_derived)] <- NA_integer_
    pet <- !is.na(pet_derived) & pet_derived == 1L
    employed <- !is.na(employment) & employment == 1
    unemployed <- !is.na(unemployment) & unemployment == 1
    both_observed <- !is.na(employment) & !is.na(unemployment)

    pea[pet & (employed | unemployed)] <- 1L
    pea[pet & both_observed & !employed & !unemployed] <- 0L
    pea[pet & !both_observed & !employed & !unemployed] <- NA_integer_

    return(list(
      pea = pea,
      pet_derived_n = as.integer(sum(pet_derived == 1L, na.rm = TRUE)),
      pea_derived_n = as.integer(sum(pea == 1L, na.rm = TRUE)),
      pea_derived_missing_n = as.integer(sum(is.na(pea)))
    ))
  }

  list(
    pea = rep(NA_real_, length(age)),
    pet_derived_n = as.integer(sum(pet_derived == 1L, na.rm = TRUE)),
    pea_derived_n = NA_integer_,
    pea_derived_missing_n = NA_integer_
  )
}

.enemdu_build_ipm_child_adolescent_employment_official_component <- function(data,
                                                                             component_var,
                                                                             household_id,
                                                                             age_var,
                                                                             attendance_var,
                                                                             child_work_var,
                                                                             p20_var,
                                                                             p21_var,
                                                                             p22_var,
                                                                             p24_var,
                                                                             pea_var,
                                                                             unemployment_var,
                                                                             condactn_var,
                                                                             pea_source,
                                                                             rule_status,
                                                                             missing_official_source_vars,
                                                                             p51_prefix,
                                                                             overwrite,
                                                                             strict) {
  age <- .enemdu_ipm_coerce_source_numeric(data[[age_var]], age_var)
  attendance <- .enemdu_ipm_coerce_source_numeric(data[[attendance_var]], attendance_var)
  employment <- .enemdu_ipm_coerce_source_numeric(data[[child_work_var]], child_work_var)
  p20 <- .enemdu_ipm_coerce_source_numeric(data[[p20_var]], p20_var)
  p21 <- .enemdu_ipm_coerce_source_numeric(data[[p21_var]], p21_var)
  p22 <- .enemdu_ipm_coerce_source_numeric(data[[p22_var]], p22_var)
  p24 <- .enemdu_ipm_coerce_source_numeric(data[[p24_var]], p24_var)
  condactn <- .enemdu_ipm_coerce_source_numeric(data[[condactn_var]], condactn_var)
  unemployment <- if (unemployment_var %in% names(data)) {
    .enemdu_ipm_coerce_source_numeric(data[[unemployment_var]], unemployment_var)
  } else {
    rep(NA_real_, length(age))
  }

  pea_info <- .enemdu_ipm_i04_pea_source(
    age = age,
    employment = employment,
    unemployment = unemployment,
    data = data,
    pea_var = pea_var,
    pea_source = pea_source
  )
  pea <- pea_info$pea
  use_pea_gate <- !identical(pea_source, "not_available_without_gate")

  p51_vars <- grep(paste0("^", p51_prefix), names(data), value = TRUE)
  p51_hours <- rep(NA_real_, length(age))
  p51_sentinel_n <- 0L
  p51_all_missing_n <- 0L

  if (length(p51_vars) > 0) {
    p51_data <- lapply(p51_vars, function(var) {
      values <- .enemdu_ipm_coerce_source_numeric(data[[var]], var)
      p51_sentinel_n <<- p51_sentinel_n + sum(!is.na(values) & values == 999)
      values[!is.na(values) & values == 999] <- NA_real_
      values
    })
    p51_matrix <- as.data.frame(p51_data, optional = TRUE)
    p51_non_missing_n <- rowSums(!is.na(p51_matrix))
    p51_hours <- rowSums(p51_matrix, na.rm = TRUE)
    p51_hours[p51_non_missing_n == 0] <- NA_real_
    p51_hours[p51_hours < 0] <- NA_real_
    p51_all_missing_n <- sum(p51_non_missing_n == 0)
  }

  horas <- rep(NA_real_, length(age))
  horas[!is.na(employment) & employment == 1] <- 0
  pea_hours_gate <- if (isTRUE(use_pea_gate)) {
    !is.na(pea) & pea == 1
  } else {
    rep(TRUE, length(age))
  }
  direct_hours <- pea_hours_gate &
    (
      (!is.na(p20) & p20 == 1) |
        (!is.na(p20) & p20 == 2 & !is.na(p21) & p21 <= 11)
    )
  horas[direct_hours] <- p24[direct_hours]

  p51_hours_rows <- pea_hours_gate &
    !is.na(p20) & p20 == 2 &
    !is.na(p21) & p21 == 12 &
    !is.na(p22) & p22 == 1
  horas[p51_hours_rows] <- p51_hours[p51_hours_rows]

  child <- !is.na(age) & age >= 5 & age <= 14
  adolescent <- !is.na(age) & age >= 15 & age <= 17
  applicable <- child | adolescent

  child_deprived <- child & (
    (!is.na(p20) & p20 == 1) |
      (!is.na(p21) & p21 %in% 1:11) |
      (!is.na(p22) & p22 == 1)
  )
  adolescent_deprived <- adolescent & (
    (!is.na(condactn) & condactn %in% 2:6) |
      (!is.na(condactn) & condactn == 1 &
        ((!is.na(attendance) & attendance == 2) | (!is.na(horas) & horas > 30)))
  )

  adolescent_non_deprived <- adolescent &
    !is.na(condactn) &
    condactn == 1 &
    !is.na(attendance) &
    attendance == 1 &
    !is.na(horas) &
    horas <= 30
  adolescent_unknown <- adolescent &
    !is.na(condactn) &
    condactn == 1 &
    !adolescent_deprived &
    !adolescent_non_deprived

  critical_missing <- (child & is.na(p20) & is.na(p21) & is.na(p22)) |
    (adolescent & is.na(condactn)) |
    adolescent_unknown
  child_missing_labor_block_n <- sum(child & is.na(p20) & is.na(p21) & is.na(p22))
  adolescent_condact_missing_n <- sum(adolescent & is.na(condactn))
  adolescent_decision_missing_n <- sum(adolescent_unknown)

  person_deprivation <- rep(0L, length(age))
  person_deprivation[is.na(age)] <- NA_integer_
  person_deprivation[child_deprived | adolescent_deprived] <- 1L
  person_deprivation[critical_missing] <- NA_integer_

  component <- .enemdu_ipm_household_max_complete(
    household_id = data[[household_id]],
    values = person_deprivation
  )
  household_na_n <- sum(is.na(component))
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
      child_adolescent_employment = list(
        source_vars = c(
          household_id,
          age_var,
          attendance_var,
          child_work_var,
          p20_var,
          p21_var,
          p22_var,
          p24_var,
          if (identical(pea_source, "provided_pea")) pea_var else character(),
          if (identical(pea_source, "derived_from_empleo_desempleo")) unemployment_var else character(),
          condactn_var,
          p51_vars
        ),
        child_age_range = c(5, 14),
        adolescent_age_range = c(15, 17),
        official_like_policy = rule_status,
        pea_available = identical(pea_source, "provided_pea"),
        pea_source = pea_source,
        pea_derived_available = identical(pea_source, "derived_from_empleo_desempleo"),
        pea_used_for_hours = isTRUE(use_pea_gate),
        pet_derived_n = pea_info$pet_derived_n,
        pea_derived_n = pea_info$pea_derived_n,
        pea_derived_missing_n = pea_info$pea_derived_missing_n,
        missing_official_source_vars = missing_official_source_vars,
        hours_policy = if (isTRUE(use_pea_gate)) {
          "stata_order_with_pea_gate"
        } else {
          "stata_order_without_pea_gate"
        },
        p51_prefix = p51_prefix,
        p51_vars = p51_vars,
        p51_vars_used = p51_vars,
        p51_sentinel_999_recode_to_missing_n = p51_sentinel_n,
        p51_all_missing_n = p51_all_missing_n,
        hh_from_p51_missing_n = sum(p51_hours_rows & is.na(p51_hours)),
        child_missing_labor_block_n = child_missing_labor_block_n,
        adolescent_condact_missing_n = adolescent_condact_missing_n,
        adolescent_decision_missing_n = adolescent_decision_missing_n,
        critical_missing_n = sum(critical_missing),
        applicable_persons_n = sum(applicable),
        working_adolescents_unknown_hours_n = sum(
          adolescent & !is.na(condactn) & condactn == 1 & is.na(horas)
        ),
        household_na_n = household_na_n,
        output_component = component_var,
        rule_status = rule_status,
        official_validation_status = "not_officially_validated"
      )
    ),
    critical_missing = list(
      ipm_i04_empleo_infantil_adolescente = critical_missing
    )
  )
}

.enemdu_build_ipm_labor_inadequate_component <- function(data,
                                                         component_var,
                                                         household_id,
                                                         age_var,
                                                         condact_var,
                                                         condactn_var = NULL,
                                                         labor_block_vars = c("p20", "p21", "p22", "p32", "p34", "p35"),
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

  if (is.null(condactn_var)) {
    condactn_var <- c("condactn", condact_var)
  }

  condactn_source_var <- .enemdu_ipm_first_existing_var(data, condactn_var)
  missing_labor_block_vars <- setdiff(labor_block_vars, names(data))
  labor_block_available <- length(missing_labor_block_vars) == 0

  if (!is.na(condactn_source_var) && isTRUE(labor_block_available)) {
    return(.enemdu_build_ipm_labor_inadequate_official_component(
      data = data,
      component_var = component_var,
      household_id = household_id,
      age_var = age_var,
      condactn_var = condactn_source_var,
      labor_block_vars = labor_block_vars,
      overwrite = overwrite,
      strict = strict
    ))
  }

  labor_data <- data
  missing_flags <- setdiff(labor_inadequate_flags, names(labor_data))
  fallback_condact_var <- condact_var

  if (!fallback_condact_var %in% names(labor_data) && !is.na(condactn_source_var)) {
    fallback_condact_var <- condactn_source_var
  }

  if (length(missing_flags) > 0) {
    if (!fallback_condact_var %in% names(labor_data)) {
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

    labor_input <- labor_data[, unique(c(fallback_condact_var, age_var)), drop = FALSE]

    labor_flags_data <- enemdu_build_labor_flags(
      data = labor_input,
      condact = fallback_condact_var,
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
        source_vars = unique(c(household_id, age_var, fallback_condact_var)),
        labor_inadequate_flags = labor_inadequate_flags,
        output_component = component_var,
        rule = "household_has_person_age_18_plus_unemployed_or_inadequately_employed",
        note = paste(
          "Labor flags are derived only from the consolidated ENEMDU condact variable when needed.",
          "Sector variables such as secemp are intentionally ignored for this IPM component."
        ),
        rule_status = "proxy_fallback_not_official_syntax",
        fallback_reason = if (!is.na(condactn_source_var)) {
          "raw_labor_block_vars_missing"
        } else {
          "official_condition_var_missing"
        },
        missing_official_source_vars = missing_labor_block_vars
      )
    )
  )
}

.enemdu_build_ipm_labor_inadequate_official_component <- function(data,
                                                                  component_var,
                                                                  household_id,
                                                                  age_var,
                                                                  condactn_var,
                                                                  labor_block_vars,
                                                                  overwrite,
                                                                  strict) {
  age <- .enemdu_ipm_coerce_source_numeric(data[[age_var]], age_var)
  condactn <- .enemdu_ipm_coerce_source_numeric(data[[condactn_var]], condactn_var)

  labor_block <- lapply(labor_block_vars, function(var) {
    if (var %in% names(data)) {
      .enemdu_ipm_coerce_source_numeric(data[[var]], var)
    } else {
      rep(NA_real_, length(age))
    }
  })
  labor_block <- as.data.frame(labor_block, optional = TRUE)
  names(labor_block) <- labor_block_vars
  all_labor_block_missing <- !stats::complete.cases(!is.na(labor_block))
  all_labor_block_missing <- rowSums(!is.na(labor_block)) == 0

  applicable <- !is.na(age) & age >= 18 & age <= 98
  critical_missing <- applicable & (is.na(condactn) | all_labor_block_missing)

  person_deprivation <- rep(0L, length(age))
  person_deprivation[is.na(age)] <- NA_integer_
  person_deprivation[applicable & !is.na(condactn) & condactn %in% 2:8] <- 1L
  person_deprivation[critical_missing] <- NA_integer_

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
      labor_inadequate_employment = list(
        source_vars = c(
          household_id,
          age_var,
          condactn_var,
          intersect(labor_block_vars, names(data))
        ),
        labor_block_vars = labor_block_vars,
        official_condactn_var = condactn_var,
        critical_missing_n = sum(critical_missing),
        output_component = component_var,
        rule = "condactn_in_2_to_8_for_people_age_18_to_98",
        rule_status = "official_syntax_rule",
        note = "Sector variables such as secemp are intentionally ignored."
      )
    ),
    critical_missing = list(
      ipm_i05_desempleo_empleo_inadecuado = critical_missing
    )
  )
}

.enemdu_build_ipm_pension_contribution_component <- function(data,
                                                             component_var,
                                                             household_id,
                                                             age_var,
                                                             employment_var,
                                                             unemployment_var,
                                                             social_security_var,
                                                             social_security_a_var,
                                                             social_security_b_var,
                                                             pension_income_var,
                                                             human_development_bonus_var,
                                                             disability_bonus_var,
                                                             pea_var,
                                                             unemployed_var,
                                                             inactive_var,
                                                             working_age_var,
                                                             condactn_var,
                                                             social_security_contribution_codes,
                                                             social_security_no_contribution_codes,
                                                             social_security_unknown_codes,
                                                             employment_na_as_not_employed,
                                                             overwrite,
                                                             strict) {
  if (component_var %in% names(data) && !isTRUE(overwrite)) {
    return(.enemdu_ipm_noop_component_result(data))
  }

  official_core_vars <- c(
    household_id,
    age_var,
    employment_var,
    social_security_a_var,
    social_security_b_var,
    pension_income_var,
    human_development_bonus_var,
    disability_bonus_var
  )
  official_labor_status_vars <- c(
    unemployed_var,
    inactive_var,
    working_age_var
  )
  official_vars <- c(official_core_vars, official_labor_status_vars)

  official_core_available <- all(official_core_vars %in% names(data))
  official_labor_status_available <- all(official_labor_status_vars %in% names(data))
  unemployment_status_available <- unemployed_var %in% names(data) ||
    unemployment_var %in% names(data)
  derived_labor_status_available <- isTRUE(official_core_available) &&
    isTRUE(unemployment_status_available)

  if (isTRUE(official_core_available) &&
      (isTRUE(official_labor_status_available) ||
        isTRUE(derived_labor_status_available))) {
    rule_status <- if (isTRUE(official_labor_status_available)) {
      "official_syntax_rule"
    } else {
      "official_like_with_derived_labor_status"
    }

    return(.enemdu_build_ipm_pension_contribution_official_component(
      data = data,
      component_var = component_var,
      household_id = household_id,
      age_var = age_var,
      employment_var = employment_var,
      unemployment_var = unemployment_var,
      social_security_a_var = social_security_a_var,
      social_security_b_var = social_security_b_var,
      pension_income_var = pension_income_var,
      human_development_bonus_var = human_development_bonus_var,
      disability_bonus_var = disability_bonus_var,
      pea_var = pea_var,
      unemployed_var = unemployed_var,
      inactive_var = inactive_var,
      working_age_var = working_age_var,
      condactn_var = condactn_var,
      rule_status = rule_status,
      missing_official_source_vars = setdiff(official_vars, names(data)),
      overwrite = overwrite,
      strict = strict
    ))
  }

  required_vars <- c(
    household_id,
    age_var,
    employment_var
  )
  missing_vars <- setdiff(required_vars, names(data))

  if (length(missing_vars) > 0) {
    return(.enemdu_ipm_pending_component_result(
      data = data,
      component_var = component_var,
      reason = paste0(
        "Missing source variables for pension contribution: ",
        paste(missing_vars, collapse = ", "),
        "."
      )
    ))
  }

  age <- .enemdu_ipm_coerce_source_numeric(data[[age_var]], age_var)
  employment <- .enemdu_ipm_coerce_source_numeric(data[[employment_var]], employment_var)
  unemployment_available <- !is.null(unemployment_var) &&
    unemployment_var %in% names(data)

  unemployment <- NULL

  if (isTRUE(unemployment_available)) {
    unemployment <- .enemdu_coerce_ipm_numeric(data[[unemployment_var]])
    unemployment[!is.na(unemployment) & !(unemployment %in% c(0, 1))] <- NA_real_
  }

  person_deprivation <- rep(0L, length(age))
  person_deprivation[is.na(age)] <- NA_integer_

  invalid_employment <- !is.na(employment) & !(employment %in% c(0, 1))
  age_15_plus <- !is.na(age) & age >= 15
  employed <- age_15_plus & !is.na(employment) & employment == 1
  not_employed <- age_15_plus & (
    (!is.na(employment) & employment == 0) |
      (is.na(employment) & isTRUE(employment_na_as_not_employed))
  )
  employment_unknown <- age_15_plus &
    is.na(employment) &
    !isTRUE(employment_na_as_not_employed)
  employed_older <- employed & age >= 65
  employed_non_older <- employed & age < 65
  older_not_employed <- not_employed & age >= 65

  applicable_missing_vars <- character()
  if (any(employed) && !social_security_var %in% names(data)) {
    applicable_missing_vars <- c(applicable_missing_vars, social_security_var)
  }
  benefit_vars <- c(pension_income_var, human_development_bonus_var, disability_bonus_var)
  if (any(older_not_employed)) {
    applicable_missing_vars <- c(
      applicable_missing_vars,
      setdiff(benefit_vars, names(data))
    )
  }

  if (length(applicable_missing_vars) > 0) {
    return(.enemdu_ipm_pending_component_result(
      data = data,
      component_var = component_var,
      reason = paste0(
        "Missing applicable source variables for pension contribution: ",
        paste(unique(applicable_missing_vars), collapse = ", "),
        "."
      )
    ))
  }

  social_security <- rep(NA_real_, length(age))
  if (social_security_var %in% names(data)) {
    social_security <- .enemdu_ipm_coerce_source_numeric(
      data[[social_security_var]],
      social_security_var
    )
  }

  pension_values <- rep(NA_real_, length(age))
  bonus_values <- rep(NA_real_, length(age))
  disability_bonus_values <- rep(NA_real_, length(age))

  if (pension_income_var %in% names(data)) {
    pension_values <- .enemdu_ipm_coerce_source_numeric(
      data[[pension_income_var]],
      pension_income_var
    )
  }
  if (human_development_bonus_var %in% names(data)) {
    bonus_values <- .enemdu_ipm_coerce_source_numeric(
      data[[human_development_bonus_var]],
      human_development_bonus_var
    )
  }
  if (disability_bonus_var %in% names(data)) {
    disability_bonus_values <- .enemdu_ipm_coerce_source_numeric(
      data[[disability_bonus_var]],
      disability_bonus_var
    )
  }

  valid_social_security_codes <- unique(c(
    social_security_contribution_codes,
    social_security_no_contribution_codes,
    social_security_unknown_codes
  ))
  invalid_social_security <- !is.na(social_security) &
    !(social_security %in% valid_social_security_codes)

  if (any(employed & invalid_social_security) && isTRUE(strict)) {
    .enemdu_abort_invalid_ipm_source_codes(
      social_security_var,
      valid_social_security_codes
    )
  }

  contributes <- !is.na(social_security) &
    !invalid_social_security &
    social_security %in% social_security_contribution_codes
  no_contribution <- !is.na(social_security) &
    !invalid_social_security &
    social_security %in% social_security_no_contribution_codes
  unknown_contribution <- is.na(social_security) |
    invalid_social_security |
    social_security %in% social_security_unknown_codes

  pension_invalid <- !is.na(pension_values) & !(pension_values %in% c(1, 2))
  bonus_invalid <- !is.na(bonus_values) & !(bonus_values %in% c(1, 2))
  disability_bonus_invalid <- !is.na(disability_bonus_values) &
    !(disability_bonus_values %in% c(1, 2))

  pension_yes <- !is.na(pension_values) & !pension_invalid & pension_values == 1
  bonus_yes <- !is.na(bonus_values) & !bonus_invalid & bonus_values == 1
  disability_bonus_yes <- !is.na(disability_bonus_values) &
    !disability_bonus_invalid &
    disability_bonus_values == 1
  pension_no <- !is.na(pension_values) & !pension_invalid & pension_values == 2
  bonus_no <- !is.na(bonus_values) & !bonus_invalid & bonus_values == 2
  disability_bonus_no <- !is.na(disability_bonus_values) &
    !disability_bonus_invalid &
    disability_bonus_values == 2

  benefit_yes <- pension_yes | bonus_yes | disability_bonus_yes
  benefit_all_no <- pension_no & bonus_no & disability_bonus_no
  benefit_unknown <- !benefit_yes & !benefit_all_no

  if (any(older_not_employed & !benefit_yes & pension_invalid) && isTRUE(strict)) {
    .enemdu_abort_invalid_ipm_source_codes(pension_income_var, c(1, 2))
  }
  if (any(older_not_employed & !benefit_yes & bonus_invalid) && isTRUE(strict)) {
    .enemdu_abort_invalid_ipm_source_codes(human_development_bonus_var, c(1, 2))
  }
  if (any(older_not_employed & !benefit_yes & disability_bonus_invalid) && isTRUE(strict)) {
    .enemdu_abort_invalid_ipm_source_codes(disability_bonus_var, c(1, 2))
  }

  if (any(age_15_plus & invalid_employment) && isTRUE(strict)) {
    .enemdu_abort_invalid_ipm_source_codes(employment_var, c(0, 1))
  }

  person_deprivation[employment_unknown] <- NA_integer_
  person_deprivation[age_15_plus & invalid_employment] <- NA_integer_

  person_deprivation[employed & contributes] <- 0L

  person_deprivation[employed_non_older & no_contribution] <- 1L
  person_deprivation[employed_non_older & unknown_contribution] <- NA_integer_

  person_deprivation[employed_older & benefit_yes] <- 0L
  person_deprivation[employed_older & no_contribution & benefit_all_no] <- 1L
  person_deprivation[employed_older & no_contribution & benefit_unknown] <- NA_integer_
  person_deprivation[employed_older & unknown_contribution & !benefit_yes] <- NA_integer_

  person_deprivation[older_not_employed & benefit_yes] <- 0L
  person_deprivation[older_not_employed & benefit_all_no] <- 1L
  person_deprivation[older_not_employed & benefit_unknown] <- NA_integer_

  component <- .enemdu_ipm_household_max_complete(
    household_id = data[[household_id]],
    values = person_deprivation
  )
  household_na_n <- sum(is.na(component))
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
      pension_contribution = list(
        source_vars = c(
          household_id,
          age_var,
          employment_var,
          intersect(
            c(
              social_security_var,
              pension_income_var,
              human_development_bonus_var,
              disability_bonus_var
            ),
            names(data)
          )
        ),
        contribution_codes = social_security_contribution_codes,
        no_contribution_codes = social_security_no_contribution_codes,
        unknown_codes = social_security_unknown_codes,
        pension_exception_age_min = 65,
        disability_bonus_note = "The disability bonus input is treated as a profile-specific proxy.",
        employment_na_as_not_employed = isTRUE(employment_na_as_not_employed),
        employment_na_treated_as_not_employed_15_plus_n = sum(
          age_15_plus & is.na(employment) & isTRUE(employment_na_as_not_employed)
        ),
        remaining_unknown_employment_cases_n = sum(
          employment_unknown | (age_15_plus & invalid_employment)
        ),
        occupied_15_plus_evaluated_n = sum(employed),
        occupied_15_plus_unknown_contribution_n = sum(employed & unknown_contribution),
        older_non_employed_evaluated_n = sum(older_not_employed),
        older_non_employed_unknown_benefit_status_n = sum(
          older_not_employed & benefit_unknown
        ),
        household_na_n = household_na_n,
        unemployment_var_available = !is.null(unemployment),
        unemployment_var_used_for_diagnostics = if (!is.null(unemployment)) {
          unemployment_var
        } else {
          NA_character_
        },
        unemployment_missing_n = if (!is.null(unemployment)) {
          sum(is.na(unemployment))
        } else {
          NA_integer_
        },
        output_component = component_var,
        rule_status = "proxy_fallback_not_official_syntax",
        missing_official_source_vars = setdiff(official_vars, names(data)),
        official_validation_status = "not_officially_validated"
      )
    )
  )
}

.enemdu_ipm_i06_labor_status_source <- function(data,
                                                age,
                                                employment,
                                                unemployment_var,
                                                unemployed_var,
                                                inactive_var,
                                                working_age_var,
                                                pea_var,
                                                condactn_var) {
  n <- length(age)

  if (working_age_var %in% names(data)) {
    pet <- .enemdu_ipm_coerce_source_numeric(data[[working_age_var]], working_age_var)
    pet_source <- "provided_pet"
    pet_derived_n <- NA_integer_
  } else {
    pet <- rep(0L, n)
    pet[is.na(age)] <- NA_integer_
    pet[!is.na(age) & age >= 15 & age <= 98] <- 1L
    pet[!is.na(age) & (age < 15 | age == 99)] <- 0L
    pet_source <- "derived_from_age"
    pet_derived_n <- as.integer(sum(pet == 1L, na.rm = TRUE))
  }

  if (unemployed_var %in% names(data)) {
    desem <- .enemdu_ipm_coerce_source_numeric(data[[unemployed_var]], unemployed_var)
    desem_source <- "provided_desem"
    desem_derived_n <- NA_integer_
  } else if (unemployment_var %in% names(data)) {
    unemployment <- .enemdu_ipm_coerce_source_numeric(data[[unemployment_var]], unemployment_var)
    desem <- rep(0L, n)
    desem[!is.na(unemployment) & unemployment == 1] <- 1L
    desem_source <- "derived_from_desempleo"
    desem_derived_n <- as.integer(sum(desem == 1L, na.rm = TRUE))
  } else {
    desem <- rep(NA_real_, n)
    desem_source <- "not_available"
    desem_derived_n <- NA_integer_
  }

  if (pea_var %in% names(data)) {
    pea <- .enemdu_ipm_coerce_source_numeric(data[[pea_var]], pea_var)
    pea_source <- "provided_pea"
    pea_derived_n <- NA_integer_
  } else if (!identical(desem_source, "not_available")) {
    pea <- rep(NA_integer_, n)
    not_pet <- !is.na(pet) & pet == 0
    pet_1 <- !is.na(pet) & pet == 1
    employed <- !is.na(employment) & employment == 1
    unemployed <- !is.na(desem) & desem == 1

    pea[not_pet] <- 0L
    pea[pet_1 & (employed | unemployed)] <- 1L
    pea[pet_1 & !employed & !unemployed] <- 0L
    pea_source <- "derived_from_empleo_desem"
    pea_derived_n <- as.integer(sum(pea == 1L, na.rm = TRUE))
  } else {
    pea <- rep(NA_real_, n)
    pea_source <- "not_available"
    pea_derived_n <- NA_integer_
  }

  if (inactive_var %in% names(data)) {
    pei <- .enemdu_ipm_coerce_source_numeric(data[[inactive_var]], inactive_var)
    pei_source <- "provided_pei"
    pei_derived_n <- NA_integer_
  } else if (!identical(pea_source, "not_available")) {
    pei <- rep(NA_integer_, n)
    pei[!is.na(pet) & pet == 0] <- 0L
    pei[!is.na(pet) & pet == 1 & !is.na(pea) & pea == 0] <- 1L
    pei[!is.na(pet) & pet == 1 & !is.na(pea) & pea == 1] <- 0L
    pei_source <- "derived_from_pet_pea"
    pei_derived_n <- as.integer(sum(pei == 1L, na.rm = TRUE))
  } else {
    pei <- rep(NA_real_, n)
    pei_source <- "not_available"
    pei_derived_n <- NA_integer_
  }

  condactn_source_var <- .enemdu_ipm_first_existing_var(data, condactn_var)
  condact_pet_consistency_n <- NA_integer_
  condact_pea_mismatch_n <- NA_integer_
  condact_pei_mismatch_n <- NA_integer_
  labor_status_validation <- "not_checked"

  if (!is.na(condactn_source_var)) {
    condact <- .enemdu_ipm_coerce_source_numeric(data[[condactn_source_var]], condactn_source_var)
    condact_pet <- !is.na(condact) & condact %in% 1:9
    condact_pea <- !is.na(condact) & condact %in% 1:8
    condact_pei <- !is.na(condact) & condact == 9

    condact_pet_consistency_n <- as.integer(sum(
      condact_pet & !is.na(pet) & pet == 1L
    ))
    condact_pea_mismatch_n <- as.integer(sum(
      condact_pea & (is.na(pea) | pea != 1L)
    ))
    condact_pei_mismatch_n <- as.integer(sum(
      condact_pei & (is.na(pei) | pei != 1L)
    ))
    labor_status_validation <- if (
      condact_pea_mismatch_n == 0L &&
        condact_pei_mismatch_n == 0L
    ) {
      "consistent_with_condact"
    } else {
      "mismatch_detected"
    }
  }

  list(
    pet = pet,
    pea = pea,
    pei = pei,
    desem = desem,
    pet_source = pet_source,
    pea_source = pea_source,
    pei_source = pei_source,
    desem_source = desem_source,
    labor_status_policy = if (
      identical(pet_source, "provided_pet") &&
        identical(pei_source, "provided_pei") &&
        identical(desem_source, "provided_desem")
    ) {
      "provided_official_labor_status"
    } else {
      "one_na_membership_flags"
    },
    labor_status_validation = labor_status_validation,
    pet_derived_n = pet_derived_n,
    pea_derived_n = pea_derived_n,
    pei_derived_n = pei_derived_n,
    desem_derived_n = desem_derived_n,
    pet_missing_n = as.integer(sum(is.na(pet))),
    pea_missing_n = as.integer(sum(is.na(pea))),
    pei_missing_n = as.integer(sum(is.na(pei))),
    condactn_source_var = condactn_source_var,
    condact_pet_consistency_n = condact_pet_consistency_n,
    condact_pea_mismatch_n = condact_pea_mismatch_n,
    condact_pei_mismatch_n = condact_pei_mismatch_n
  )
}

.enemdu_build_ipm_pension_contribution_official_component <- function(data,
                                                                      component_var,
                                                                      household_id,
                                                                      age_var,
                                                                      employment_var,
                                                                      unemployment_var,
                                                                      social_security_a_var,
                                                                      social_security_b_var,
                                                                      pension_income_var,
                                                                      human_development_bonus_var,
                                                                      disability_bonus_var,
                                                                      pea_var,
                                                                      unemployed_var,
                                                                      inactive_var,
                                                                      working_age_var,
                                                                      condactn_var,
                                                                      rule_status,
                                                                      missing_official_source_vars,
                                                                      overwrite,
                                                                      strict) {
  age <- .enemdu_ipm_coerce_source_numeric(data[[age_var]], age_var)
  employment <- .enemdu_ipm_coerce_source_numeric(data[[employment_var]], employment_var)
  social_security_a <- .enemdu_ipm_coerce_source_numeric(
    data[[social_security_a_var]],
    social_security_a_var
  )
  social_security_b <- .enemdu_ipm_coerce_source_numeric(
    data[[social_security_b_var]],
    social_security_b_var
  )
  pension <- .enemdu_ipm_coerce_source_numeric(data[[pension_income_var]], pension_income_var)
  bonus <- .enemdu_ipm_coerce_source_numeric(
    data[[human_development_bonus_var]],
    human_development_bonus_var
  )
  disability_bonus <- .enemdu_ipm_coerce_source_numeric(
    data[[disability_bonus_var]],
    disability_bonus_var
  )
  labor_status <- .enemdu_ipm_i06_labor_status_source(
    data = data,
    age = age,
    employment = employment,
    unemployment_var = unemployment_var,
    unemployed_var = unemployed_var,
    inactive_var = inactive_var,
    working_age_var = working_age_var,
    pea_var = pea_var,
    condactn_var = condactn_var
  )
  unemployed <- labor_status$desem
  inactive <- labor_status$pei
  working_age <- labor_status$pet

  age_15_98 <- !is.na(age) & age >= 15 & age <= 98
  working_age_out <- age_15_98 & !is.na(working_age) & working_age == 0
  employed <- age_15_98 & !is.na(employment) & employment == 1
  employed_non_older <- employed & !is.na(age) & age < 65
  employed_older <- employed & !is.na(age) & age >= 65
  older_not_employed <- age_15_98 &
    !is.na(age) &
    age >= 65 &
    ((!is.na(unemployed) & unemployed == 1) | (!is.na(inactive) & inactive == 1))

  contribution_observed <- !is.na(social_security_a) & !is.na(social_security_b)
  no_contribution <- contribution_observed &
    social_security_a %in% 5:10 &
    social_security_b %in% 5:10
  contributes <- contribution_observed & !no_contribution

  pension_yes <- !is.na(pension) & pension == 1
  pension_no <- !is.na(pension) & pension == 2
  bonus_yes <- !is.na(bonus) & bonus == 1
  bonus_no <- !is.na(bonus) & bonus == 2
  disability_bonus_yes <- !is.na(disability_bonus) & disability_bonus == 1
  disability_bonus_no <- !is.na(disability_bonus) & disability_bonus == 2
  disability_exception_yes <- !is.na(working_age) & working_age == 1 & disability_bonus_yes
  benefit_yes <- pension_yes | (pension_no & bonus_yes) | disability_exception_yes
  benefit_all_no <- pension_no & bonus_no & disability_bonus_no
  benefit_unknown <- !benefit_yes & !benefit_all_no

  person_deprivation <- rep(0L, length(age))
  person_deprivation[is.na(age)] <- NA_integer_

  person_deprivation[employed_non_older & no_contribution] <- 1L
  person_deprivation[employed_non_older & contributes] <- 0L
  person_deprivation[employed_non_older & disability_exception_yes] <- 0L

  person_deprivation[employed_older & contributes] <- 0L
  person_deprivation[employed_older & no_contribution] <- 1L
  person_deprivation[employed_older & benefit_yes] <- 0L

  person_deprivation[older_not_employed & benefit_all_no] <- 1L
  person_deprivation[older_not_employed & benefit_yes] <- 0L

  person_deprivation[working_age_out] <- NA_integer_

  critical_missing <- !working_age_out & (
    (employed_non_older & !contribution_observed) |
      (employed_older & !contribution_observed & !benefit_yes) |
      (employed_older & no_contribution & benefit_unknown) |
      (older_not_employed & benefit_unknown)
  )
  person_deprivation[critical_missing] <- NA_integer_

  component <- .enemdu_ipm_household_max_complete(
    household_id = data[[household_id]],
    values = person_deprivation
  )
  household_na_n <- sum(is.na(component))
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
      pension_contribution = list(
        source_vars = c(
          household_id,
          age_var,
          employment_var,
          social_security_a_var,
          social_security_b_var,
          pension_income_var,
          human_development_bonus_var,
          disability_bonus_var,
          intersect(
            c(
              unemployment_var,
              unemployed_var,
              inactive_var,
              working_age_var,
              pea_var,
              labor_status$condactn_source_var
            ),
            names(data)
          )
        ),
        pet_source = labor_status$pet_source,
        pea_source = labor_status$pea_source,
        pei_source = labor_status$pei_source,
        desem_source = labor_status$desem_source,
        labor_status_policy = labor_status$labor_status_policy,
        labor_status_validation = labor_status$labor_status_validation,
        pet_derived_n = labor_status$pet_derived_n,
        pea_derived_n = labor_status$pea_derived_n,
        pei_derived_n = labor_status$pei_derived_n,
        desem_derived_n = labor_status$desem_derived_n,
        pet_missing_n = labor_status$pet_missing_n,
        pea_missing_n = labor_status$pea_missing_n,
        pei_missing_n = labor_status$pei_missing_n,
        condact_pet_consistency_n = labor_status$condact_pet_consistency_n,
        condact_pea_mismatch_n = labor_status$condact_pea_mismatch_n,
        condact_pei_mismatch_n = labor_status$condact_pei_mismatch_n,
        pension_exception_age_min = 65,
        critical_missing_n = sum(critical_missing),
        occupied_15_plus_evaluated_n = sum(employed),
        occupied_15_plus_unknown_contribution_n = sum(employed & !contribution_observed),
        older_non_employed_evaluated_n = sum(older_not_employed),
        older_non_employed_unknown_benefit_status_n = sum(
          older_not_employed & benefit_unknown
        ),
        household_na_n = household_na_n,
        output_component = component_var,
        rule_status = rule_status,
        missing_official_source_vars = missing_official_source_vars,
        official_validation_status = "not_officially_validated"
      )
    ),
    critical_missing = list(
      ipm_i06_no_contribucion_pensiones = critical_missing
    )
  )
}

.enemdu_build_ipm_housing_deficit_component <- function(data,
                                                        component_var,
                                                        household_id,
                                                        roof_material_var,
                                                        roof_state_var,
                                                        floor_material_var,
                                                        floor_state_var,
                                                        wall_material_var,
                                                        wall_state_var,
                                                        housing_material_valid_codes,
                                                        housing_state_valid_codes,
                                                        deficit_roof_material_codes,
                                                        deficit_floor_material_codes,
                                                        deficit_wall_material_codes,
                                                        deficit_state_codes,
                                                        overwrite,
                                                        strict) {
  if (component_var %in% names(data) && !isTRUE(overwrite)) {
    return(.enemdu_ipm_noop_component_result(data))
  }

  required_vars <- c(
    household_id,
    roof_material_var,
    roof_state_var,
    floor_material_var,
    floor_state_var,
    wall_material_var,
    wall_state_var
  )
  missing_vars <- setdiff(required_vars, names(data))

  if (length(missing_vars) > 0) {
    return(.enemdu_ipm_pending_component_result(
      data = data,
      component_var = component_var,
      reason = paste0(
        "Missing source variables for housing deficit: ",
        paste(missing_vars, collapse = ", "),
        "."
      )
    ))
  }

  roof_material <- .enemdu_ipm_coerce_source_numeric(data[[roof_material_var]], roof_material_var)
  floor_material <- .enemdu_ipm_coerce_source_numeric(data[[floor_material_var]], floor_material_var)
  wall_material <- .enemdu_ipm_coerce_source_numeric(data[[wall_material_var]], wall_material_var)
  roof_state <- .enemdu_ipm_coerce_source_numeric(data[[roof_state_var]], roof_state_var)
  floor_state <- .enemdu_ipm_coerce_source_numeric(data[[floor_state_var]], floor_state_var)
  wall_state <- .enemdu_ipm_coerce_source_numeric(data[[wall_state_var]], wall_state_var)
  invalid_roof_state <- !is.na(roof_state) & !(roof_state %in% housing_state_valid_codes)
  invalid_floor_state <- !is.na(floor_state) & !(floor_state %in% housing_state_valid_codes)
  invalid_wall_state <- !is.na(wall_state) & !(wall_state %in% housing_state_valid_codes)

  if (any(invalid_roof_state | invalid_floor_state | invalid_wall_state) && isTRUE(strict)) {
    .enemdu_abort_invalid_ipm_source_codes(
      paste(c(roof_state_var, floor_state_var, wall_state_var), collapse = ", "),
      housing_state_valid_codes
    )
  }

  techo <- rep(NA_integer_, length(roof_material))
  techo[
    (roof_material == 1 & roof_state %in% 1:2) |
      (roof_state == 1 & roof_material %in% 2:4)
  ] <- 1L
  techo[
    (roof_material == 1 & roof_state == 3) |
      (roof_state == 2 & roof_material %in% 2:4)
  ] <- 2L
  techo[
    (roof_state == 3 & roof_material %in% 2:4) |
      roof_material %in% c(5, 6)
  ] <- 3L

  pared <- rep(NA_integer_, length(wall_material))
  pared[
    (wall_material == 1 & wall_state %in% 1:2) |
      (wall_material == 2 & wall_state == 1)
  ] <- 1L
  pared[
    (wall_material == 1 & wall_state == 3) |
      (wall_material == 2 & wall_state == 2) |
      (wall_material %in% 3:5 & wall_state %in% 1:2)
  ] <- 2L
  pared[
    (wall_state == 3 & wall_material %in% 2:5) |
      wall_material %in% c(6, 7)
  ] <- 3L

  piso <- rep(NA_integer_, length(floor_material))
  piso[
    (floor_material <= 3 & floor_state == 1) |
      (floor_material <= 3 & floor_state == 2) |
      (floor_state == 1 & floor_material %in% 4:5)
  ] <- 1L
  piso[
    (floor_material <= 3 & floor_state == 3) |
      (floor_state == 2 & floor_material %in% 4:5) |
      (floor_material == 6 & floor_state == 1)
  ] <- 2L
  piso[
    (floor_state == 3 & floor_material %in% c(4, 5, 6)) |
      (floor_material == 6 & floor_state == 2) |
      floor_material %in% c(7, 8)
  ] <- 3L

  tipviv <- rep(NA_integer_, length(techo))
  tipviv[
    (techo == 1 & pared == 1 & piso %in% 1:3) |
      (techo == 1 & pared == 2 & piso == 1)
  ] <- 1L
  tipviv[
    (techo == 1 & pared == 2 & piso %in% 2:3) |
      (techo == 1 & pared == 3 & piso %in% 1:2) |
      (techo == 2 & pared == 1 & piso %in% 1:2) |
      (techo == 2 & pared == 2 & piso %in% 1:2) |
      (techo == 3 & pared == 1 & piso %in% 1:2) |
      (techo == 3 & pared == 2 & piso == 1)
  ] <- 2L
  tipviv[
    (techo == 1 & pared == 3 & piso == 3) |
      (techo == 2 & pared == 1 & piso == 3) |
      (techo == 2 & pared == 2 & piso == 3) |
      (techo == 2 & pared == 3 & piso %in% 1:3) |
      (techo == 3 & pared == 1 & piso == 3) |
      (techo == 3 & pared == 2 & piso %in% 1:3) |
      (techo == 3 & pared == 3 & piso %in% 1:3)
  ] <- 3L

  critical_missing <- is.na(roof_material) |
    is.na(roof_state) |
    is.na(floor_material) |
    is.na(floor_state) |
    is.na(wall_material) |
    is.na(wall_state) |
    invalid_roof_state |
    invalid_floor_state |
    invalid_wall_state

  row_component <- rep(NA_integer_, length(tipviv))
  row_component[tipviv == 1] <- 0L
  row_component[tipviv %in% c(2, 3)] <- 1L
  row_component[critical_missing] <- NA_integer_

  component <- .enemdu_ipm_household_max_complete(
    household_id = data[[household_id]],
    values = row_component
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
      housing_deficit = list(
        source_vars = required_vars,
        critical_missing_n = sum(critical_missing),
        techo_unknown_n = sum(is.na(techo) & !critical_missing),
        pared_unknown_n = sum(is.na(pared) & !critical_missing),
        piso_unknown_n = sum(is.na(piso) & !critical_missing),
        tipviv_unknown_n = sum(is.na(tipviv) & !critical_missing),
        output_component = component_var,
        rule = "official_techo_pared_piso_tipviv_classification",
        rule_status = "official_syntax_rule"
      )
    ),
    critical_missing = list(
      ipm_i10_deficit_habitacional = critical_missing
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
  critical_missing <- is.na(area) | is.na(sanitation)

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
        critical_missing_n = sum(critical_missing),
        rule = "urban_vi09_2_to_5_or_rural_vi09_3_to_5",
        rule_status = "official_syntax_rule"
      )
    ),
    critical_missing = list(
      ipm_i11_sin_saneamiento_excretas = critical_missing | invalid_sanitation
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
  critical_missing <- is.na(garbage)

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
        critical_missing_n = sum(critical_missing),
        rule = "deprived_when_vi13_in_1_3_4_5",
        rule_status = "official_syntax_rule"
      )
    ),
    critical_missing = list(
      ipm_i12_sin_recoleccion_basura = critical_missing | invalid_garbage
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
    variables_used = list(),
    critical_missing = list()
  )
}

.enemdu_ipm_pending_component_result <- function(data, component_var, reason) {
  list(
    data = data,
    built_components = character(),
    pending_components = component_var,
    pending_reasons = stats::setNames(list(reason), component_var),
    variables_used = list(),
    critical_missing = list()
  )
}

.enemdu_ipm_critical_missing_info <- function(critical_missing,
                                              component_vars,
                                              household_id) {
  n <- length(household_id)

  if (length(critical_missing) == 0) {
    person_flag <- rep(0L, n)
    household_flag <- .enemdu_ipm_household_any_flag(household_id, person_flag)

    return(list(
      person_flag = person_flag,
      household_flag = household_flag,
      by_component = tibble::tibble(
        indicator_id = character(),
        component_var = character(),
        critical_missing_person_rows = integer(),
        critical_missing_households = integer()
      ),
      person_rows_n = 0L,
      households_n = 0L
    ))
  }

  normalized <- lapply(critical_missing, function(x) {
    out <- rep(FALSE, n)
    len <- min(length(x), n)
    out[seq_len(len)] <- isTRUE(FALSE)
    out[seq_len(len)] <- !is.na(x[seq_len(len)]) & x[seq_len(len)]
    out
  })

  person_any <- Reduce(`|`, normalized)
  person_flag <- as.integer(person_any)
  household_flag <- .enemdu_ipm_household_any_flag(household_id, person_flag)

  by_component <- lapply(names(normalized), function(indicator_id) {
    flag <- normalized[[indicator_id]]
    tibble::tibble(
      indicator_id = indicator_id,
      component_var = unname(component_vars[[indicator_id]] %||% NA_character_),
      critical_missing_person_rows = as.integer(sum(flag)),
      critical_missing_households = as.integer(sum(
        tapply(flag, as.character(household_id), any, na.rm = TRUE)
      ))
    )
  })
  by_component <- do.call(rbind, by_component)

  list(
    person_flag = person_flag,
    household_flag = household_flag,
    by_component = tibble::as_tibble(by_component),
    person_rows_n = as.integer(sum(person_flag == 1L)),
    households_n = as.integer(length(unique(
      as.character(household_id)[household_flag == 1L]
    )))
  )
}

.enemdu_ipm_household_any_flag <- function(household_id, values) {
  ids <- as.character(household_id)
  flag <- !is.na(values) & values == 1L
  household_values <- tapply(flag, ids, any, na.rm = TRUE)
  as.integer(household_values[ids])
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

.enemdu_ipm_coerce_binary_source <- function(values, var, strict) {
  numeric_values <- .enemdu_ipm_coerce_source_numeric(values, var)
  invalid <- !is.na(numeric_values) & !(numeric_values %in% c(0, 1))

  if (any(invalid) && isTRUE(strict)) {
    .enemdu_abort_invalid_ipm_source_codes(var, c(0, 1))
  }

  numeric_values[invalid] <- NA_real_
  numeric_values
}

.enemdu_ipm_yes_no_source <- function(values, var, strict) {
  numeric_values <- .enemdu_ipm_coerce_source_numeric(values, var)
  invalid <- !is.na(numeric_values) & !(numeric_values %in% c(1, 2))

  if (any(invalid) && isTRUE(strict)) {
    .enemdu_abort_invalid_ipm_source_codes(var, c(1, 2))
  }

  numeric_values[invalid] <- NA_real_

  list(
    yes = !is.na(numeric_values) & numeric_values == 1,
    no = !is.na(numeric_values) & numeric_values == 2,
    unknown = is.na(numeric_values)
  )
}

.enemdu_ipm_optional_code_set <- function(codes, arg) {
  if (is.null(codes)) {
    return(numeric(0))
  }

  .enemdu_ipm_validate_code_set(codes, arg)
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

.enemdu_ipm_single_positive_number <- function(value, arg) {
  numeric_value <- suppressWarnings(as.numeric(value))

  if (
    length(numeric_value) != 1 ||
      is.na(numeric_value) ||
      !is.finite(numeric_value) ||
      numeric_value <= 0
  ) {
    rlang::abort(
      message = glue::glue("`{arg}` must be a single positive number."),
      class = c("enemdu_error_invalid_ipm_component_input", "enemdu_error")
    )
  }

  numeric_value
}

.enemdu_ipm_single_logical <- function(value, arg) {
  if (
    !is.logical(value) ||
      length(value) != 1 ||
      is.na(value)
  ) {
    rlang::abort(
      message = glue::glue("`{arg}` must be a single TRUE or FALSE value."),
      class = c("enemdu_error_invalid_ipm_component_input", "enemdu_error")
    )
  }

  value
}

.enemdu_ipm_bachillerato_completed_2025 <- function(education_level,
                                                    education_grade,
                                                    completed_levels,
                                                    completed_min_grade) {
  if (length(completed_levels) > 0 || length(completed_min_grade) > 0) {
    if (length(completed_levels) == 0 || length(completed_min_grade) == 0) {
      rlang::abort(
        message = paste(
          "`bachillerato_completed_levels` and",
          "`bachillerato_completed_min_grade` must be supplied together."
        ),
        class = c("enemdu_error_invalid_ipm_component_input", "enemdu_error")
      )
    }

    if (length(completed_min_grade) == 1) {
      completed_min_grade <- rep(completed_min_grade, length(completed_levels))
    }

    if (length(completed_min_grade) != length(completed_levels)) {
      rlang::abort(
        message = paste(
          "`bachillerato_completed_min_grade` must have length 1 or match",
          "`bachillerato_completed_levels`."
        ),
        class = c("enemdu_error_invalid_ipm_component_input", "enemdu_error")
      )
    }

    completed <- rep(FALSE, length(education_level))

    for (i in seq_along(completed_levels)) {
      rows <- !is.na(education_level) & education_level == completed_levels[[i]]
      completed[rows & is.na(education_grade)] <- NA
      observed <- rows & !is.na(education_grade)
      completed[observed] <- education_grade[observed] >= completed_min_grade[[i]]
    }

    completed[is.na(education_level)] <- NA
    return(completed)
  }

  schooling_years <- .enemdu_ipm_schooling_years_2025(
    education_level = education_level,
    education_grade = education_grade
  )
  completed <- schooling_years >= 13
  completed[is.na(schooling_years)] <- NA
  completed
}

.enemdu_ipm_official_bachillerato_completed <- function(education_level,
                                                        education_grade,
                                                        include_postgraduate) {
  completed <- rep(FALSE, length(education_level))
  completed[is.na(education_level)] <- NA

  completed[
    !is.na(education_level) &
      education_level == 7 &
      !is.na(education_grade) &
      education_grade >= 3
  ] <- TRUE
  completed[
    !is.na(education_level) &
      education_level == 6 &
      !is.na(education_grade) &
      education_grade >= 6
  ] <- TRUE

  higher_levels <- if (isTRUE(include_postgraduate)) {
    8:10
  } else {
    8:9
  }
  completed[!is.na(education_level) & education_level %in% higher_levels] <- TRUE

  completed
}

.enemdu_ipm_label_codes <- function(values) {
  labels <- attr(values, "labels", exact = TRUE)

  if (is.null(labels) || length(labels) == 0) {
    return(numeric(0))
  }

  numeric_labels <- suppressWarnings(as.numeric(labels))
  unique(numeric_labels[!is.na(numeric_labels)])
}

.enemdu_ipm_infer_codes_from_labels <- function(values, patterns) {
  labels <- attr(values, "labels", exact = TRUE)

  if (is.null(labels) || length(labels) == 0 || is.null(names(labels))) {
    return(numeric(0))
  }

  label_text <- tolower(iconv(names(labels), to = "ASCII//TRANSLIT"))
  matched <- rep(FALSE, length(label_text))

  for (pattern in patterns) {
    matched <- matched | grepl(pattern, label_text, perl = TRUE)
  }

  numeric_labels <- suppressWarnings(as.numeric(labels[matched]))
  unique(numeric_labels[!is.na(numeric_labels)])
}

.enemdu_ipm_infer_code_set_from_labels <- function(data, vars) {
  codes <- unlist(lapply(vars, function(var) {
    .enemdu_ipm_label_codes(data[[var]])
  }), use.names = FALSE)

  unique(codes[!is.na(codes)])
}

.enemdu_ipm_invalid_against_optional_codes <- function(values, valid_codes) {
  if (length(valid_codes) == 0) {
    return(rep(FALSE, length(values)))
  }

  !is.na(values) & !(values %in% valid_codes)
}

.enemdu_ipm_any_true <- function(...) {
  values <- list(...)

  if (length(values) == 0) {
    return(logical())
  }

  out <- values[[1]]

  if (length(values) > 1) {
    for (i in 2:length(values)) {
      out <- out | values[[i]]
    }
  }

  out
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

.enemdu_abort_invalid_ipm_source_values <- function(var) {
  rlang::abort(
    message = glue::glue(
      "IPM source variable `{var}` contains sentinel or invalid values for this rule."
    ),
    class = c("enemdu_error_invalid_ipm_source_values", "enemdu_error")
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

.enemdu_ipm_schooling_years_2025_info <- function(education_level,
                                                  education_grade,
                                                  unknown_policy = c(
                                                    "preserve_na",
                                                    "official_recode_to_zero"
                                                  )) {
  unknown_policy <- match.arg(unknown_policy)
  years <- rep(NA_real_, length(education_level))

  years[!is.na(education_level) & !is.na(education_grade) &
    education_level == 1 & education_grade == 0] <- 0
  years[!is.na(education_level) & !is.na(education_grade) &
    education_level == 2 & education_grade == 0] <- 0
  years[!is.na(education_level) & !is.na(education_grade) &
    education_level == 2 & education_grade == 1] <- 2
  years[!is.na(education_level) & !is.na(education_grade) &
    education_level == 2 & education_grade == 2] <- 4
  years[!is.na(education_level) & !is.na(education_grade) &
    education_level == 2 & education_grade == 3] <- 6
  years[!is.na(education_level) & !is.na(education_grade) &
    education_level == 2 & education_grade == 4] <- 7
  years[!is.na(education_level) & education_level == 3] <- 1

  primary <- !is.na(education_level) & education_level == 4 & !is.na(education_grade)
  years[primary] <- 1 + education_grade[primary]

  basic <- !is.na(education_level) & education_level == 5 & !is.na(education_grade)
  years[basic] <- education_grade[basic]

  secondary <- !is.na(education_level) & education_level == 6 & !is.na(education_grade)
  years[secondary] <- 7 + education_grade[secondary]

  upper_secondary <- !is.na(education_level) & education_level == 7 & !is.na(education_grade)
  years[upper_secondary] <- 10 + education_grade[upper_secondary]

  higher <- !is.na(education_level) &
    education_level %in% c(8, 9) &
    !is.na(education_grade)
  years[higher] <- 13 + education_grade[higher]

  postgraduate <- !is.na(education_level) &
    education_level == 10 &
    !is.na(education_grade)
  years[postgraduate] <- 18 + education_grade[postgraduate]

  invalid_grade <- !is.na(education_grade) & education_grade < 0
  years[invalid_grade] <- NA_real_

  grade_required <- !is.na(education_level) &
    education_level %in% c(1, 2, 4, 5, 6, 7, 8, 9, 10)
  missing_required_grade <- grade_required & is.na(education_grade)
  missing_inputs <- is.na(education_level) | missing_required_grade
  unmatched_observed <- !is.na(education_level) &
    !is.na(education_grade) &
    !missing_required_grade &
    !invalid_grade &
    is.na(years)
  unknown_before_policy <- is.na(years)
  converted_to_zero <- rep(FALSE, length(years))
  unknown_converted_to_zero <- rep(FALSE, length(years))
  missing_converted_to_zero <- rep(FALSE, length(years))

  if (identical(unknown_policy, "official_recode_to_zero")) {
    converted_to_zero <- unknown_before_policy
    unknown_converted_to_zero <- unknown_before_policy
    missing_converted_to_zero <- unknown_before_policy & missing_inputs
    years[unknown_before_policy] <- 0
  }

  list(
    years = years,
    converted_to_zero = converted_to_zero,
    unmatched_observed = unmatched_observed,
    missing_required_grade = missing_required_grade,
    missing_inputs = missing_inputs,
    unknown_converted_to_zero = unknown_converted_to_zero,
    missing_converted_to_zero = missing_converted_to_zero,
    unknown_policy = unknown_policy
  )
}

.enemdu_ipm_schooling_years_2025 <- function(education_level,
                                             education_grade) {
  .enemdu_ipm_schooling_years_2025_info(
    education_level = education_level,
    education_grade = education_grade
  )$years
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
