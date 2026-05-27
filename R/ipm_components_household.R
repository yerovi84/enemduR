.enemdu_build_ipm_low_risk_household_components <- function(
  data,
  household_id = "id_hogar",
  person_id = "p01",
  hsize_var = "hsize",
  water_var = "vi10",
  bedrooms_var = "vi07",
  water_public_network_codes = 1,
  overwrite = FALSE,
  strict = TRUE
) {
  caller <- ".enemdu_build_ipm_low_risk_household_components"

  if (!is.data.frame(data)) {
    .enemdu_abort_invalid_data(caller = caller)
  }

  household_id <- .enemdu_ipm_single_var_name(household_id, "household_id", caller)
  person_id <- .enemdu_ipm_single_var_name(person_id, "person_id", caller)
  hsize_var <- .enemdu_ipm_single_var_name(hsize_var, "hsize_var", caller)
  water_var <- .enemdu_ipm_single_var_name(water_var, "water_var", caller)
  bedrooms_var <- .enemdu_ipm_single_var_name(bedrooms_var, "bedrooms_var", caller)

  output_components <- .enemdu_ipm_low_risk_household_component_names()
  existing_outputs <- intersect(output_components, names(data))

  if (length(existing_outputs) > 0 && !isTRUE(overwrite)) {
    rlang::abort(
      message = glue::glue(
        "IPM household component variables already exist: ",
        "{paste(existing_outputs, collapse = ', ')}. ",
        "Use `overwrite = TRUE` to replace them."
      ),
      class = c("enemdu_error_existing_ipm_household_components", "enemdu_error"),
      existing_vars = existing_outputs
    )
  }

  hsize_was_derived <- !hsize_var %in% names(data)
  required_vars <- c(household_id, water_var, bedrooms_var)

  if (isTRUE(hsize_was_derived)) {
    required_vars <- c(required_vars, person_id)
  } else {
    required_vars <- c(required_vars, hsize_var)
  }

  .enemdu_abort_missing_vars(
    vars = unique(required_vars),
    names_data = names(data),
    caller = caller
  )

  out <- data

  household_values <- out[[household_id]]
  water_source <- .enemdu_ipm_coerce_household_numeric(
    values = out[[water_var]],
    var = water_var,
    caller = caller
  )
  bedrooms <- .enemdu_ipm_coerce_household_numeric(
    values = out[[bedrooms_var]],
    var = bedrooms_var,
    caller = caller
  )
  public_network_codes <- .enemdu_ipm_validate_public_network_codes(
    water_public_network_codes = water_public_network_codes,
    caller = caller
  )

  if (isTRUE(hsize_was_derived)) {
    if (isTRUE(strict) && any(is.na(household_values) | is.na(out[[person_id]]))) {
      .enemdu_abort_missing_ipm_household_source(
        var = paste(household_id, person_id, sep = ", "),
        caller = caller
      )
    }

    hsize <- .enemdu_ipm_make_hsize(
      household_id = household_values,
      person_id = out[[person_id]]
    )
    hsize_var_used <- "derived_from_household_id_and_person_id"
  } else {
    hsize <- .enemdu_ipm_coerce_household_numeric(
      values = out[[hsize_var]],
      var = hsize_var,
      caller = caller
    )
    hsize_var_used <- hsize_var
  }

  if (isTRUE(strict) && any(is.na(household_values))) {
    .enemdu_abort_missing_ipm_household_source(var = household_id, caller = caller)
  }

  if (isTRUE(strict) && any(is.na(water_source))) {
    .enemdu_abort_missing_ipm_household_source(var = water_var, caller = caller)
  }

  if (isTRUE(strict) && any(is.na(bedrooms))) {
    .enemdu_abort_missing_ipm_household_source(var = bedrooms_var, caller = caller)
  }

  if (isTRUE(strict) && any(is.na(hsize))) {
    .enemdu_abort_missing_ipm_household_source(var = hsize_var_used, caller = caller)
  }

  invalid_bedrooms <- !is.na(bedrooms) & bedrooms < 0
  invalid_hsize <- !is.na(hsize) & hsize <= 0

  if (isTRUE(strict) && any(invalid_bedrooms)) {
    .enemdu_abort_invalid_ipm_household_source(
      var = bedrooms_var,
      message = "Sleeping-room counts must be greater than or equal to zero.",
      caller = caller
    )
  }

  if (isTRUE(strict) && any(invalid_hsize)) {
    .enemdu_abort_invalid_ipm_household_source(
      var = hsize_var_used,
      message = "Household size must be positive.",
      caller = caller
    )
  }

  bedrooms[invalid_bedrooms] <- NA_real_
  hsize[invalid_hsize] <- NA_real_

  water_component <- .enemdu_ipm_water_public_network_component(
    water_source = water_source,
    public_network_codes = public_network_codes
  )
  overcrowding_component <- .enemdu_ipm_overcrowding_component(
    hsize = hsize,
    bedrooms = bedrooms
  )

  out[[output_components[["water"]]]] <- water_component
  out[[output_components[["overcrowding"]]]] <- overcrowding_component

  zero_bedroom_households <- unique(
    as.character(household_values[!is.na(bedrooms) & bedrooms == 0])
  )

  attr(out, "ipm_low_risk_household_component_diagnostics") <- list(
    n_rows = nrow(out),
    n_households = length(unique(household_values[!is.na(household_values)])),
    household_id = household_id,
    hsize_var_used = hsize_var_used,
    hsize_was_derived = isTRUE(hsize_was_derived),
    water_var = water_var,
    bedrooms_var = bedrooms_var,
    water_public_network_codes = public_network_codes,
    output_components = unname(output_components),
    n_water_na = sum(is.na(water_component)),
    n_overcrowding_na = sum(is.na(overcrowding_component)),
    n_zero_bedrooms = length(zero_bedroom_households),
    strict = isTRUE(strict)
  )

  class(out) <- unique(c("enemdu_tbl", class(out)))
  out
}

.enemdu_ipm_low_risk_household_component_names <- function() {
  registry <- .enemdu_ipm_component_registry()
  required_cols <- c("indicator_id", "expected_component_name")
  missing_cols <- setdiff(required_cols, names(registry))

  if (length(missing_cols) > 0) {
    .enemdu_abort_invalid_registry(
      registry_name = "ipm_component_registry",
      message = glue::glue("Missing columns: {paste(missing_cols, collapse = ', ')}."),
      caller = ".enemdu_ipm_low_risk_household_component_names"
    )
  }

  indicator_ids <- c(
    water = "ipm_i08_sin_agua_red_publica",
    overcrowding = "ipm_i09_hacinamiento"
  )
  matched <- match(indicator_ids, registry$indicator_id)

  if (any(is.na(matched))) {
    .enemdu_abort_invalid_registry(
      registry_name = "ipm_component_registry",
      message = "The low-risk IPM household indicator IDs were not found.",
      caller = ".enemdu_ipm_low_risk_household_component_names"
    )
  }

  component_names <- as.character(registry$expected_component_name[matched])

  if (
    any(is.na(component_names)) ||
      any(!nzchar(component_names)) ||
      anyDuplicated(component_names) > 0
  ) {
    .enemdu_abort_invalid_registry(
      registry_name = "ipm_component_registry",
      message = "Low-risk IPM household component names must be unique and non-empty.",
      caller = ".enemdu_ipm_low_risk_household_component_names"
    )
  }

  stats::setNames(component_names, names(indicator_ids))
}

.enemdu_ipm_single_var_name <- function(var, arg, caller) {
  var <- as.character(var)

  if (length(var) != 1 || is.na(var) || !nzchar(var)) {
    rlang::abort(
      message = glue::glue("`{arg}` must be a single non-empty variable name."),
      class = c("enemdu_error_invalid_ipm_household_component_input", "enemdu_error")
    )
  }

  var
}

.enemdu_ipm_coerce_household_numeric <- function(values, var, caller) {
  missing <- is.na(values)
  numeric_values <- .enemdu_coerce_ipm_numeric(values)
  invalid_conversion <- !missing & is.na(numeric_values)

  if (any(invalid_conversion)) {
    .enemdu_abort_invalid_ipm_household_source(
      var = var,
      message = "Source values must be visible numeric codes.",
      caller = caller
    )
  }

  numeric_values
}

.enemdu_ipm_validate_public_network_codes <- function(water_public_network_codes,
                                                      caller) {
  codes <- .enemdu_ipm_coerce_household_numeric(
    values = water_public_network_codes,
    var = "water_public_network_codes",
    caller = caller
  )

  if (
    length(codes) == 0 ||
      any(is.na(codes)) ||
      any(!is.finite(codes))
  ) {
    .enemdu_abort_invalid_ipm_household_source(
      var = "water_public_network_codes",
      message = "Public-network water codes must be finite numeric values.",
      caller = caller
    )
  }

  unique(codes)
}

.enemdu_ipm_make_hsize <- function(household_id, person_id) {
  household_id <- as.character(household_id)
  person_id <- as.character(person_id)
  valid <- !is.na(household_id) & !is.na(person_id)

  if (!any(valid)) {
    return(rep(NA_real_, length(household_id)))
  }

  counts <- tapply(
    person_id[valid],
    household_id[valid],
    function(x) length(unique(x))
  )

  as.numeric(counts[household_id])
}

.enemdu_ipm_water_public_network_component <- function(water_source,
                                                       public_network_codes) {
  out <- rep(NA_integer_, length(water_source))
  observed <- !is.na(water_source)

  out[observed] <- as.integer(!(water_source[observed] %in% public_network_codes))
  out
}

.enemdu_ipm_overcrowding_component <- function(hsize, bedrooms) {
  out <- rep(NA_integer_, length(hsize))

  zero_bedrooms <- !is.na(bedrooms) & bedrooms == 0
  out[zero_bedrooms] <- 1L

  valid_ratio <- !is.na(hsize) & !is.na(bedrooms) & bedrooms > 0
  out[valid_ratio] <- as.integer((hsize[valid_ratio] / bedrooms[valid_ratio]) > 3)

  out
}

.enemdu_abort_missing_ipm_household_source <- function(var, caller) {
  rlang::abort(
    message = glue::glue("IPM household source variable `{var}` contains missing values."),
    class = c("enemdu_error_missing_ipm_household_source", "enemdu_error")
  )
}

.enemdu_abort_invalid_ipm_household_source <- function(var, message, caller) {
  rlang::abort(
    message = glue::glue("Invalid IPM household source variable `{var}` in `{caller}()`: {message}"),
    class = c("enemdu_error_invalid_ipm_household_source", "enemdu_error")
  )
}
