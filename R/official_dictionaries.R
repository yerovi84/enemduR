#' Official ENEMDU dictionary core registry
#'
#' Reads the package registry that records core variables observed in official
#' ENEMDU dictionaries loaded during the development process. This registry is
#' intentionally narrow: it does not replace full official dictionaries; it
#' records the variables needed to audit the current package contracts.
#'
#' @return A tibble with core official dictionary variables.
#' @export
enemdu_official_dictionary_core_registry <- function() {
  registry <- .enemdu_read_csv_registry("official_dictionary_core_registry.csv")
  .enemdu_validate_official_dictionary_core_registry(registry)
  registry
}

#' Validate internal contracts against official dictionary core variables
#'
#' Compares the core official dictionary registry with the package variable
#' catalog, domain variable registry, and optional bonus registry. This function
#' is designed as an audit layer: it flags mismatches without silently changing
#' package contracts.
#'
#' @param survey_type Optional survey type filter: `"mensual"`, `"trimestral"` or
#' `"anual"`.
#' @param registry Official dictionary core registry.
#' @param variable_catalog Package variable catalog.
#' @param domain_variable_registry Package domain variable registry.
#' @param optional_bonus_registry Package optional bonus registry.
#' @param emit Logical. If `TRUE`, emits an informational summary.
#'
#' @return A tibble with validation status by official variable.
#' @export
enemdu_validate_official_dictionary_core <- function(
    survey_type = NULL,
    registry = enemdu_official_dictionary_core_registry(),
    variable_catalog = enemdu_variable_catalog(),
    domain_variable_registry = enemdu_domain_variable_registry(),
    optional_bonus_registry = enemdu_optional_bonus_registry(),
    emit = TRUE) {
  .enemdu_validate_official_dictionary_core_registry(registry)

  if (!is.null(survey_type)) {
    survey_type <- .enemdu_normalize_survey_type(
      survey_type = survey_type,
      caller = "enemdu_validate_official_dictionary_core"
    )

    registry <- registry[registry$survey_type == survey_type, , drop = FALSE]
  }

  if (nrow(registry) == 0) {
    out <- .enemdu_empty_official_dictionary_validation()
    attr(out, "official_dictionary_validation_policy") <- list(
      survey_type = survey_type %||% NA_character_,
      note = "No rows were available after filtering."
    )
    return(out)
  }

  .enemdu_abort_missing_vars(
    vars = c("variable", "aliases"),
    names_data = names(variable_catalog),
    caller = "enemdu_validate_official_dictionary_core"
  )

  .enemdu_abort_missing_vars(
    vars = c("variable", "domain_level"),
    names_data = names(domain_variable_registry),
    caller = "enemdu_validate_official_dictionary_core"
  )

  .enemdu_abort_missing_vars(
    vars = c("variable", "bonus_label"),
    names_data = names(optional_bonus_registry),
    caller = "enemdu_validate_official_dictionary_core"
  )

  rows <- vector("list", nrow(registry))

  for (i in seq_len(nrow(registry))) {
    row <- registry[i, , drop = FALSE]

    catalog_match <- .enemdu_official_dictionary_catalog_match(
      official_variable = row$official_variable[[1]],
      package_expected_variable = row$package_expected_variable[[1]],
      variable_catalog = variable_catalog
    )

    domain_match <- .enemdu_official_dictionary_domain_match(
      official_variable = row$official_variable[[1]],
      package_expected_variable = catalog_match$package_variable,
      domain_level = row$domain_level[[1]],
      domain_variable_registry = domain_variable_registry
    )

    bonus_match <- .enemdu_official_dictionary_bonus_match(
      official_variable = row$official_variable[[1]],
      official_description = row$official_description[[1]],
      optional_bonus_registry = optional_bonus_registry
    )

    validation_flag <- .enemdu_official_dictionary_validation_flag(
      required_core = row$required_core[[1]],
      role = row$role[[1]],
      catalog_status = catalog_match$catalog_status,
      domain_level = row$domain_level[[1]],
      domain_status = domain_match$domain_status,
      bonus_label_status = bonus_match$bonus_label_status
    )

    severity <- .enemdu_official_dictionary_validation_severity(validation_flag)

    rows[[i]] <- tibble::tibble(
      survey_type = row$survey_type[[1]],
      period = row$period[[1]],
      dictionary_frequency = row$dictionary_frequency[[1]],
      dictionary_file_scope = row$dictionary_file_scope[[1]],
      official_variable = row$official_variable[[1]],
      official_description = row$official_description[[1]],
      role = row$role[[1]],
      required_core = as.logical(row$required_core[[1]]),
      package_expected_variable = row$package_expected_variable[[1]],
      package_variable = catalog_match$package_variable,
      catalog_match_type = catalog_match$catalog_match_type,
      catalog_status = catalog_match$catalog_status,
      domain_level = row$domain_level[[1]],
      domain_status = domain_match$domain_status,
      optional_bonus_label = bonus_match$optional_bonus_label,
      bonus_label_status = bonus_match$bonus_label_status,
      validation_flag = validation_flag,
      severity = severity,
      source_status = row$source_status[[1]],
      notes = row$notes[[1]]
    )
  }

  out <- do.call(rbind, rows)
  row.names(out) <- NULL
  out <- tibble::as_tibble(out)

  attr(out, "official_dictionary_validation_policy") <- list(
    survey_type = survey_type %||% "all",
    note = paste(
      "This validation compares official dictionary core variables against internal package contracts.",
      "It does not mutate registries or rename variables automatically."
    )
  )

  if (isTRUE(emit)) {
    n_fail <- sum(out$severity == "error", na.rm = TRUE)
    n_warning <- sum(out$severity == "warning", na.rm = TRUE)

    rlang::inform(
      message = glue::glue(
        "Official dictionary core validation completed: ",
        "{n_fail} error-level issue(s), {n_warning} warning-level issue(s)."
      ),
      class = c("enemdu_message_official_dictionary_validation", "enemdu_message")
    )
  }

  class(out) <- unique(c("enemdu_official_dictionary_validation", class(out)))
  out
}

.enemdu_validate_official_dictionary_core_registry <- function(registry) {
  required_cols <- c(
    "survey_type",
    "period",
    "dictionary_frequency",
    "dictionary_file_scope",
    "dictionary_file",
    "official_variable",
    "official_description",
    "package_expected_variable",
    "expected_catalog_match",
    "role",
    "required_core",
    "domain_level",
    "source_status",
    "notes"
  )

  missing_cols <- setdiff(required_cols, names(registry))

  if (length(missing_cols) > 0) {
    .enemdu_abort_invalid_registry(
      registry_name = "official_dictionary_core_registry",
      message = glue::glue("Missing columns: {paste(missing_cols, collapse = ', ')}."),
      caller = ".enemdu_validate_official_dictionary_core_registry"
    )
  }

  bad_survey_types <- setdiff(
    unique(registry$survey_type),
    .enemdu_supported_survey_types()
  )

  if (length(bad_survey_types) > 0) {
    .enemdu_abort_invalid_registry(
      registry_name = "official_dictionary_core_registry",
      message = glue::glue("Unsupported survey_type values: {paste(bad_survey_types, collapse = ', ')}."),
      caller = ".enemdu_validate_official_dictionary_core_registry"
    )
  }

  invisible(TRUE)
}

.enemdu_official_dictionary_catalog_match <- function(official_variable,
                                                      package_expected_variable,
                                                      variable_catalog) {
  official_variable <- as.character(official_variable)
  package_expected_variable <- as.character(package_expected_variable)

  exact_idx <- which(variable_catalog$variable == official_variable)

  if (length(exact_idx) >= 1) {
    return(list(
      package_variable = variable_catalog$variable[[exact_idx[[1]]]],
      catalog_match_type = "exact",
      catalog_status = "covered_by_variable_catalog"
    ))
  }

  alias_idx <- which(vapply(
    variable_catalog$aliases,
    function(alias_string) {
      official_variable %in% .enemdu_parse_pipe_values(alias_string)
    },
    logical(1)
  ))

  if (length(alias_idx) >= 1) {
    return(list(
      package_variable = variable_catalog$variable[[alias_idx[[1]]]],
      catalog_match_type = "alias",
      catalog_status = "covered_by_variable_catalog"
    ))
  }

  expected_idx <- which(variable_catalog$variable == package_expected_variable)

  if (length(expected_idx) >= 1) {
    return(list(
      package_variable = variable_catalog$variable[[expected_idx[[1]]]],
      catalog_match_type = "expected_variable",
      catalog_status = "covered_by_expected_variable"
    ))
  }

  expected_alias_idx <- which(vapply(
    variable_catalog$aliases,
    function(alias_string) {
      package_expected_variable %in% .enemdu_parse_pipe_values(alias_string)
    },
    logical(1)
  ))

  if (length(expected_alias_idx) >= 1) {
    return(list(
      package_variable = variable_catalog$variable[[expected_alias_idx[[1]]]],
      catalog_match_type = "expected_alias",
      catalog_status = "covered_by_expected_alias"
    ))
  }

  list(
    package_variable = NA_character_,
    catalog_match_type = "none",
    catalog_status = "missing_from_variable_catalog"
  )
}

.enemdu_official_dictionary_domain_match <- function(official_variable,
                                                     package_expected_variable,
                                                     domain_level,
                                                     domain_variable_registry) {
  if (is.null(domain_level) || is.na(domain_level) || !nzchar(domain_level)) {
    return(list(domain_status = "not_domain_variable"))
  }

  official_variable <- as.character(official_variable)
  package_expected_variable <- as.character(package_expected_variable)

  exact_match <- any(
    domain_variable_registry$variable == official_variable &
      domain_variable_registry$domain_level == domain_level
  )

  if (isTRUE(exact_match)) {
    return(list(domain_status = "covered_by_domain_registry"))
  }

  expected_match <- any(
    domain_variable_registry$variable == package_expected_variable &
      domain_variable_registry$domain_level == domain_level
  )

  if (isTRUE(expected_match)) {
    return(list(domain_status = "covered_by_expected_domain_variable"))
  }

  list(domain_status = "missing_from_domain_variable_registry")
}

.enemdu_official_dictionary_bonus_match <- function(official_variable,
                                                    official_description,
                                                    optional_bonus_registry) {
  official_variable <- as.character(official_variable)
  official_description <- as.character(official_description)

  row <- optional_bonus_registry[
    optional_bonus_registry$variable == official_variable,
    ,
    drop = FALSE
  ]

  if (nrow(row) == 0) {
    return(list(
      optional_bonus_label = NA_character_,
      bonus_label_status = "not_optional_bonus"
    ))
  }

  label <- as.character(row$bonus_label[[1]])

  official_mentions_disability <- grepl(
    "discapacidad",
    official_description,
    ignore.case = TRUE
  )

  label_mentions_disability <- grepl(
    "discapacidad",
    label,
    ignore.case = TRUE
  )

  if (isTRUE(official_mentions_disability) && !isTRUE(label_mentions_disability)) {
    return(list(
      optional_bonus_label = label,
      bonus_label_status = "possible_label_mismatch"
    ))
  }

  list(
    optional_bonus_label = label,
    bonus_label_status = "label_consistent"
  )
}

.enemdu_official_dictionary_validation_flag <- function(required_core,
                                                        role,
                                                        catalog_status,
                                                        domain_level,
                                                        domain_status,
                                                        bonus_label_status) {
  required_core <- as.logical(required_core)
  role <- as.character(role)

  if (isTRUE(required_core) && identical(catalog_status, "missing_from_variable_catalog")) {
    if (!is.na(domain_level) &&
        nzchar(domain_level) &&
        domain_status %in% c("covered_by_domain_registry", "covered_by_expected_domain_variable")) {
      return("warning_domain_variable_not_in_variable_catalog")
    }

    return("fail_required_core_missing_from_variable_catalog")
  }

  if (!is.na(domain_level) &&
      nzchar(domain_level) &&
      identical(domain_status, "missing_from_domain_variable_registry")) {
    return("fail_domain_variable_missing_from_domain_registry")
  }

  if (identical(bonus_label_status, "possible_label_mismatch")) {
    return("warning_optional_bonus_label_mismatch")
  }

  if (!isTRUE(required_core) && identical(catalog_status, "missing_from_variable_catalog")) {
    return("informational_observed_variable_not_cataloged")
  }

  "pass"
}

.enemdu_official_dictionary_validation_severity <- function(validation_flag) {
  if (grepl("^fail_", validation_flag)) {
    return("error")
  }

  if (grepl("^warning_", validation_flag)) {
    return("warning")
  }

  if (grepl("^informational_", validation_flag)) {
    return("info")
  }

  "ok"
}

.enemdu_empty_official_dictionary_validation <- function() {
  tibble::tibble(
    survey_type = character(),
    period = character(),
    dictionary_frequency = character(),
    dictionary_file_scope = character(),
    official_variable = character(),
    official_description = character(),
    role = character(),
    required_core = logical(),
    package_expected_variable = character(),
    package_variable = character(),
    catalog_match_type = character(),
    catalog_status = character(),
    domain_level = character(),
    domain_status = character(),
    optional_bonus_label = character(),
    bonus_label_status = character(),
    validation_flag = character(),
    severity = character(),
    source_status = character(),
    notes = character()
  )
}
