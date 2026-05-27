#' Validate inputs for NBI component derivation
#'
#' Reports whether variables required to derive final NBI components are present.
#' This preflight does not stop by default; it returns a diagnostic table.
#'
#' @param data A data frame.
#' @param profile NBI derivation profile.
#' @param household_id Household identifier variable.
#' @param person_id Person identifier variable.
#' @param area_var Optional area/domain variable.
#' @param registry NBI derivation registry.
#'
#' @return A tibble with variable-level input diagnostics and a
#' `preflight_passed` attribute.
#' @export
enemdu_validate_nbi_component_inputs <- function(data,
                                                 profile = "enemdu_2025_anual",
                                                 household_id = "id_hogar",
                                                 person_id = "p01",
                                                 area_var = "area",
                                                 registry = enemdu_nbi_derivation_registry()) {
  if (!is.data.frame(data)) {
    .enemdu_abort_invalid_data(caller = "enemdu_validate_nbi_component_inputs")
  }

  profile_contract <- .enemdu_nbi_profile(profile)
  .enemdu_validate_nbi_derivation_registry(registry)
  .enemdu_abort_missing_nbi_profile(profile, registry)

  profile_required_vars <- setdiff(profile_contract$required_vars, c("id_hogar", "p01"))

  required_vars <- unique(c(
    household_id,
    person_id,
    profile_required_vars
  ))
  optional_vars <- unique(c(
    area_var,
    profile_contract$optional_vars
  ))
  optional_vars <- setdiff(optional_vars, required_vars)
  variables <- c(required_vars, optional_vars)

  roles <- vapply(variables, function(variable) {
    .enemdu_nbi_input_role(
      variable = variable,
      household_id = household_id,
      person_id = person_id,
      area_var = area_var,
      profile = profile_contract
    )
  }, character(1))

  required <- variables %in% required_vars
  present <- variables %in% names(data)

  issue <- ifelse(
    present,
    "ok",
    ifelse(required, "missing_required_variable", "missing_optional_variable")
  )
  message <- ifelse(
    present,
    "Variable is present.",
    ifelse(required, "Required variable is missing.", "Optional variable is missing.")
  )

  out <- tibble::tibble(
    variable = variables,
    required = required,
    present = present,
    role = roles,
    issue = issue,
    message = message
  )

  attr(out, "preflight_passed") <- !any(out$required & !out$present)
  class(out) <- unique(c("enemdu_nbi_component_input_preflight", class(out)))
  out
}

#' Build final NBI components from ENEMDU questionnaire variables
#'
#' Derives `comp1` through `comp5` from raw ENEMDU questionnaire variables for a
#' supported profile. The output is designed to be passed to
#' `enemdu_build_nbi_flags()`.
#'
#' This function does not compute `knbi`, `nbi`, or `xnbi`, and it does not
#' implement TPM, TPEM, intensity, or IPM.
#'
#' @param data A data frame.
#' @param profile NBI derivation profile.
#' @param household_id Household identifier variable.
#' @param person_id Person identifier variable.
#' @param hsize_var Household-size variable. If absent, it is built from
#' `household_id`.
#' @param area_var Optional area/domain variable used by the input preflight.
#' @param overwrite Logical. If `TRUE`, overwrite existing `comp1`-`comp5`.
#' @param strict Logical. If `TRUE`, abort when required inputs are missing.
#' @param registry NBI derivation registry.
#'
#' @return A data frame with `comp1` through `comp5`.
#' @export
#'
#' @examples
#' data <- tibble::tibble(
#'   id_hogar = c("h1", "h1", "h2", "h2"),
#'   p01 = c(1, 2, 1, 2),
#'   p03 = c(40, 8, 45, 10),
#'   p04 = c(1, 3, 1, 3),
#'   p07 = c(2, 2, 2, 1),
#'   p10a = c(1, 3, 4, 3),
#'   p10b = c(0, 2, 5, 3),
#'   empleo = c(0, 0, 1, 0),
#'   vi04a = c(7, 7, 1, 1),
#'   vi05a = c(1, 1, 1, 1),
#'   vi07 = c(0, 0, 1, 1),
#'   vi09 = c(5, 5, 1, 1),
#'   vi10 = c(1, 1, 1, 1),
#'   vi10a = c(1, 1, 1, 1),
#'   area = c(1, 1, 2, 2)
#' )
#'
#' out <- enemdu_build_nbi_components(data)
#' enemdu_build_nbi_flags(out)
enemdu_build_nbi_components <- function(data,
                                        profile = "enemdu_2025_anual",
                                        household_id = "id_hogar",
                                        person_id = "p01",
                                        hsize_var = "hsize",
                                        area_var = "area",
                                        overwrite = FALSE,
                                        strict = TRUE,
                                        registry = enemdu_nbi_derivation_registry()) {
  if (!is.data.frame(data)) {
    .enemdu_abort_invalid_data(caller = "enemdu_build_nbi_components")
  }

  profile_contract <- .enemdu_nbi_profile(profile)
  preflight <- enemdu_validate_nbi_component_inputs(
    data = data,
    profile = profile,
    household_id = household_id,
    person_id = person_id,
    area_var = area_var,
    registry = registry
  )

  if (isTRUE(strict) && !isTRUE(attr(preflight, "preflight_passed"))) {
    .enemdu_abort_missing_nbi_component_inputs(preflight)
  }

  output_vars <- paste0("comp", 1:5)
  existing_outputs <- intersect(output_vars, names(data))

  if (length(existing_outputs) > 0 && !isTRUE(overwrite)) {
    rlang::abort(
      message = glue::glue(
        "NBI component variables already exist: {paste(existing_outputs, collapse = ', ')}. ",
        "Use `overwrite = TRUE` to replace them."
      ),
      class = c("enemdu_error_existing_nbi_components", "enemdu_error")
    )
  }

  .enemdu_abort_missing_vars(
    vars = unique(c(
      household_id,
      person_id,
      setdiff(profile_contract$required_vars, c("id_hogar", "p01"))
    )),
    names_data = names(data),
    caller = "enemdu_build_nbi_components"
  )

  out <- data

  if (!hsize_var %in% names(out)) {
    out[[hsize_var]] <- .enemdu_nbi_make_hsize(out[[household_id]])
  }

  hsize <- .enemdu_nbi_to_numeric(out[[hsize_var]])
  floor_material <- .enemdu_nbi_to_numeric(out[["vi04a"]])
  wall_material <- .enemdu_nbi_to_numeric(out[["vi05a"]])
  bedrooms <- .enemdu_nbi_to_numeric(out[["vi07"]])
  sanitation <- .enemdu_nbi_to_numeric(out[["vi09"]])
  water_source <- .enemdu_nbi_to_numeric(out[["vi10"]])
  water_pipe <- .enemdu_nbi_to_numeric(out[["vi10a"]])
  age <- .enemdu_nbi_to_numeric(out[["p03"]])
  attendance <- .enemdu_nbi_to_numeric(out[["p07"]])
  relationship <- .enemdu_nbi_to_numeric(out[["p04"]])
  education_level <- .enemdu_nbi_to_numeric(out[["p10a"]])
  education_grade <- .enemdu_nbi_to_numeric(out[["p10b"]])
  employment <- .enemdu_nbi_to_numeric(out[["empleo"]])

  comp1 <- .enemdu_nbi_or_deprivation(
    floor_material %in% profile_contract$floor_deprivation_codes,
    wall_material %in% profile_contract$wall_deprivation_codes,
    is.na(floor_material),
    is.na(wall_material)
  )

  comp2 <- .enemdu_nbi_overcrowding(
    hsize = hsize,
    bedrooms = bedrooms
  )

  sanitation_deprivation <- .enemdu_nbi_in_codes(
    values = sanitation,
    codes = profile_contract$sanitation_deprivation_codes
  )

  water_source_deprivation <- .enemdu_nbi_not_in_codes(
    values = water_source,
    codes = profile_contract$water_public_codes
  )

  water_pipe_deprivation <- .enemdu_nbi_not_in_codes(
    values = water_pipe,
    codes = profile_contract$water_pipe_codes
  )

  water_deprivation <- .enemdu_nbi_or_deprivation(
    water_source_deprivation,
    water_pipe_deprivation,
    is.na(water_source_deprivation),
    is.na(water_pipe_deprivation)
  )

  comp3 <- .enemdu_nbi_or_deprivation(
    sanitation_deprivation,
    water_deprivation == 1,
    is.na(sanitation_deprivation),
    is.na(water_deprivation)
  )

  child_not_attending <- .enemdu_nbi_school_deprivation(
    age = age,
    attendance = attendance,
    school_age_min = profile_contract$school_age_min,
    school_age_max = profile_contract$school_age_max,
    attendance_yes_codes = profile_contract$attendance_yes_codes
  )
  comp4 <- .enemdu_nbi_household_max(
    household_id = out[[household_id]],
    values = child_not_attending
  )

  head_schooling <- .enemdu_nbi_years_schooling(
    education_level = education_level,
    education_grade = education_grade
  )
  is_head <- relationship %in% profile_contract$household_head_codes
  head_low_schooling <- .enemdu_nbi_household_first(
    household_id = out[[household_id]],
    values = ifelse(is_head, head_schooling <= profile_contract$head_schooling_max, NA)
  )
  occupied <- .enemdu_nbi_occupied_indicator(
    employment = employment,
    occupied_codes = profile_contract$occupied_codes,
    missing_as_not_occupied = isTRUE(profile_contract$employment_missing_as_not_occupied)
  )
  occupied_count <- .enemdu_nbi_household_sum_complete(
    household_id = out[[household_id]],
    values = occupied
  )
  dependency_ratio <- hsize / occupied_count
  dependency_ratio[occupied_count == 0] <- Inf
  dependency_ratio[is.na(hsize) | is.na(occupied_count)] <- NA_real_

  comp5 <- .enemdu_nbi_and_deprivation(
    head_low_schooling,
    dependency_ratio > profile_contract$dependency_ratio_threshold
  )

  out[["comp1"]] <- comp1
  out[["comp2"]] <- comp2
  out[["comp3"]] <- comp3
  out[["comp4"]] <- comp4
  out[["comp5"]] <- comp5

  attr(out, "nbi_derivation_profile") <- profile
  attr(out, "nbi_derivation_preflight") <- preflight
  attr(out, "nbi_derivation_policy") <- list(
    profile = profile,
    component_vars = output_vars,
    household_id = household_id,
    person_id = person_id,
    hsize_var = hsize_var,
    note = paste(
      "NBI components are derived from documented ENEMDU questionnaire variables.",
      "Rules are profile-based and require official benchmark comparison before validation claims."
    )
  )

  class(out) <- unique(c("enemdu_tbl", class(out)))
  out
}

.enemdu_validate_nbi_derivation_registry <- function(registry) {
  required_cols <- c(
    "profile_id",
    "component_id",
    "component_var",
    "component_label",
    "source_vars",
    "rule_id",
    "rule_description",
    "source_status",
    "source_note"
  )

  missing_cols <- setdiff(required_cols, names(registry))

  if (length(missing_cols) > 0) {
    .enemdu_abort_invalid_registry(
      registry_name = "nbi_derivation_registry",
      message = glue::glue("Missing columns: {paste(missing_cols, collapse = ', ')}."),
      caller = ".enemdu_validate_nbi_derivation_registry"
    )
  }

  invisible(TRUE)
}

.enemdu_abort_missing_nbi_profile <- function(profile, registry) {
  if (!profile %in% registry$profile_id) {
    rlang::abort(
      message = glue::glue("NBI derivation profile `{profile}` was not found in the registry."),
      class = c("enemdu_error_invalid_nbi_derivation_profile", "enemdu_error")
    )
  }

  invisible(TRUE)
}

.enemdu_abort_missing_nbi_component_inputs <- function(preflight) {
  missing_required <- preflight[preflight$required & !preflight$present, , drop = FALSE]

  rlang::abort(
    message = glue::glue(
      "Missing NBI component input variables: {paste(missing_required$variable, collapse = ', ')}."
    ),
    class = c("enemdu_error_missing_nbi_component_inputs", "enemdu_error"),
    preflight = preflight
  )
}

.enemdu_nbi_profile <- function(profile) {
  profile <- as.character(profile)

  profiles <- list(
    enemdu_2025_anual = list(
      required_vars = c(
        "id_hogar",
        "p01",
        "p03",
        "p04",
        "p07",
        "p10a",
        "p10b",
        "vi04a",
        "vi05a",
        "vi07",
        "vi09",
        "vi10",
        "vi10a",
        "empleo"
      ),
      optional_vars = c("hsize", "condactn"),
      floor_deprivation_codes = c(7, 8),
      wall_deprivation_codes = c(6, 7),
      sanitation_deprivation_codes = c(4, 5, 6),
      water_public_codes = c(1),
      water_pipe_codes = c(1),
      attendance_yes_codes = c(1),
      household_head_codes = c(1),
      occupied_codes = c(1),
      employment_missing_as_not_occupied = TRUE,
      school_age_min = 6,
      school_age_max = 12,
      head_schooling_max = 2,
      dependency_ratio_threshold = 3
    )
  )

  if (!profile %in% names(profiles)) {
    rlang::abort(
      message = glue::glue("NBI derivation profile `{profile}` is not implemented."),
      class = c("enemdu_error_invalid_nbi_derivation_profile", "enemdu_error")
    )
  }

  profiles[[profile]]
}

.enemdu_nbi_input_role <- function(variable,
                                   household_id,
                                   person_id,
                                   area_var,
                                   profile) {
  if (identical(variable, household_id)) {
    return("household_id")
  }
  if (identical(variable, person_id)) {
    return("person_id")
  }
  if (!is.null(area_var) && identical(variable, area_var)) {
    return("area")
  }
  if (variable %in% c("vi04a", "vi05a", "vi07", "vi09", "vi10", "vi10a")) {
    return("housing")
  }
  if (variable %in% c("p03", "p04", "p07", "p10a", "p10b")) {
    return("person")
  }
  if (variable %in% c("empleo", "condactn")) {
    return("employment")
  }
  if (variable %in% profile$optional_vars) {
    return("optional")
  }
  "required"
}

.enemdu_nbi_to_numeric <- function(values) {
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

  out <- suppressWarnings(as.numeric(raw))
  out[missing] <- NA_real_
  out
}

.enemdu_nbi_in_codes <- function(values, codes) {
  out <- rep(NA, length(values))
  observed <- !is.na(values)
  out[observed] <- values[observed] %in% codes
  out
}

.enemdu_nbi_not_in_codes <- function(values, codes) {
  in_codes <- .enemdu_nbi_in_codes(values = values, codes = codes)
  out <- rep(NA, length(in_codes))
  observed <- !is.na(in_codes)
  out[observed] <- !in_codes[observed]
  out
}

.enemdu_nbi_make_hsize <- function(household_id) {
  ids <- as.character(household_id)
  counts <- table(ids, useNA = "no")
  as.integer(counts[ids])
}

.enemdu_nbi_or_deprivation <- function(left,
                                       right,
                                       left_missing,
                                       right_missing) {
  out <- rep(NA_integer_, length(left))

  observed_deprivation <- (!is.na(left) & left) | (!is.na(right) & right)
  observed_no_deprivation <- (!left_missing & !left) & (!right_missing & !right)

  out[observed_deprivation] <- 1L
  out[observed_no_deprivation] <- 0L
  out
}

.enemdu_nbi_and_deprivation <- function(left, right) {
  out <- rep(NA_integer_, length(left))
  out[!is.na(left) & !is.na(right) & left & right] <- 1L
  out[(!is.na(left) & !left) | (!is.na(right) & !right)] <- 0L
  out
}

.enemdu_nbi_overcrowding <- function(hsize, bedrooms) {
  out <- rep(NA_integer_, length(hsize))
  out[!is.na(bedrooms) & bedrooms == 0] <- 1L

  valid_ratio <- !is.na(hsize) & !is.na(bedrooms) & bedrooms > 0
  out[valid_ratio] <- as.integer((hsize[valid_ratio] / bedrooms[valid_ratio]) > 3)
  out
}

.enemdu_nbi_school_deprivation <- function(age,
                                           attendance,
                                           school_age_min,
                                           school_age_max,
                                           attendance_yes_codes) {
  school_age <- !is.na(age) & age >= school_age_min & age <= school_age_max
  out <- rep(0L, length(age))
  out[is.na(age)] <- NA_integer_
  out[school_age & is.na(attendance)] <- NA_integer_
  out[school_age & !is.na(attendance)] <-
    as.integer(!(attendance[school_age & !is.na(attendance)] %in% attendance_yes_codes))
  out
}

.enemdu_nbi_household_max <- function(household_id, values) {
  .enemdu_nbi_household_apply(
    household_id = household_id,
    values = values,
    fn = function(x) {
      if (any(x == 1, na.rm = TRUE)) {
        return(1L)
      }
      if (all(!is.na(x) & x == 0)) {
        return(0L)
      }
      NA_integer_
    }
  )
}

.enemdu_nbi_household_sum <- function(household_id, values) {
  .enemdu_nbi_household_apply(
    household_id = household_id,
    values = values,
    fn = function(x) {
      if (all(is.na(x))) {
        return(NA_real_)
      }
      sum(x, na.rm = TRUE)
    }
  )
}


.enemdu_nbi_household_sum_complete <- function(household_id, values) {
  .enemdu_nbi_household_apply(
    household_id = household_id,
    values = values,
    fn = function(x) {
      if (any(is.na(x))) {
        return(NA_real_)
      }
      sum(x)
    }
  )
}

.enemdu_nbi_household_first <- function(household_id, values) {
  .enemdu_nbi_household_apply(
    household_id = household_id,
    values = values,
    fn = function(x) {
      x <- x[!is.na(x)]
      if (length(x) == 0) {
        return(NA)
      }
      x[[1]]
    }
  )
}

.enemdu_nbi_household_apply <- function(household_id, values, fn) {
  ids <- as.character(household_id)
  split_values <- split(values, ids)
  household_values <- vapply(split_values, fn, numeric(1))
  as.vector(household_values[ids])
}

.enemdu_nbi_years_schooling <- function(education_level,
                                        education_grade) {
  years <- rep(NA_real_, length(education_level))

  no_schooling <- !is.na(education_level) & education_level == 1
  years[no_schooling] <- 0

  valid <- !is.na(education_level) &
    !is.na(education_grade) &
    !no_schooling

  years[valid & education_level <= 2] <-
    education_grade[valid & education_level <= 2]

  years[valid & education_level == 3] <-
    6 + education_grade[valid & education_level == 3]

  years[valid & education_level >= 4] <-
    12 + education_grade[valid & education_level >= 4]

  years
}

.enemdu_nbi_occupied_indicator <- function(employment,
                                           occupied_codes,
                                           missing_as_not_occupied = FALSE) {
  out <- rep(NA_real_, length(employment))

  observed <- !is.na(employment)
  out[observed] <- as.numeric(employment[observed] %in% occupied_codes)

  if (isTRUE(missing_as_not_occupied)) {
    out[is.na(employment)] <- 0
  }

  out
}
