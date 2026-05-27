#' Build registered IPM component indicators
#'
#' Builds the IPM component columns that are auditable in the current package
#' contract and marks the remaining registered components as pending. The
#' current operational rules cover public-network water, overcrowding, and
#' extreme income poverty when a precomputed binary poverty flag is supplied.
#'
#' This function does not invent rules for education, labor, pensions, housing
#' deficit, sanitation, or garbage collection. When `strict = TRUE`, the
#' function aborts if the full set of 12 registered IPM components cannot be
#' completed from implemented rules or accepted precomputed component columns.
#' When `strict = FALSE`, pending components are returned as `NA_integer_` and
#' documented in the diagnostics attribute.
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
  water_public_network_codes = 1
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
    overwrite = overwrite,
    strict = strict
  )
  out <- extreme_poverty$data
  built_components <- unique(c(built_components, extreme_poverty$built_components))
  pending_components <- unique(c(pending_components, extreme_poverty$pending_components))
  pending_reasons <- c(pending_reasons, extreme_poverty$pending_reasons)
  variables_used <- c(variables_used, extreme_poverty$variables_used)

  unavailable_indicator_ids <- setdiff(
    names(component_vars),
    c(
      "ipm_i07_pobreza_extrema_ingresos",
      "ipm_i08_sin_agua_red_publica",
      "ipm_i09_hacinamiento"
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

  if (!extreme_poverty_var %in% names(data)) {
    reason <- paste0(
      "Precomputed extreme income poverty variable `",
      extreme_poverty_var,
      "` is not available. The public builder does not derive poverty lines."
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
    values = out[[extreme_poverty_var]],
    var = extreme_poverty_var,
    strict = strict
  )

  list(
    data = out,
    built_components = component_var,
    pending_components = character(),
    pending_reasons = list(),
    variables_used = list(
      extreme_income_poverty = list(
        source_var = extreme_poverty_var,
        output_component = component_var,
        rule = "copy_precomputed_binary_extreme_poverty_flag"
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
