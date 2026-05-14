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

#' Read an official ENEMDU dictionary file
#'
#' Reads an official ENEMDU dictionary file in `.xlsx`, `.xls`, `.ods`, or
#' `.xlsm` format and normalizes it into a long tibble with one row per variable.
#'
#' The official files usually include metadata rows before the actual dictionary
#' body. This function detects the row whose first column is equivalent to
#' `"Nombre del campo"` and reads the rows below it as variable names and
#' descriptions.
#'
#' @param path Path to an official dictionary file.
#' @param survey_type Optional survey type. If `NULL`, inferred from file name
#' when possible.
#' @param period Optional period. If `NULL`, inferred from file name when
#' possible.
#' @param dictionary_frequency Optional label such as `"Mensual"`,
#' `"Trimestral"` or `"Anual"`. If `NULL`, inferred when possible.
#' @param dictionary_file_scope Optional scope such as `"persona"`,
#' `"vivienda_hogar"`, `"vivienda"` or `"consumidor"`. If `NULL`, inferred
#' when possible.
#' @param sheet Sheet name or position. Defaults to first sheet.
#'
#' @return A tibble with official dictionary variables.
#' @export
enemdu_read_official_dictionary_file <- function(path,
                                                 survey_type = NULL,
                                                 period = NULL,
                                                 dictionary_frequency = NULL,
                                                 dictionary_file_scope = NULL,
                                                 sheet = 1) {
  if (missing(path) || is.null(path) || length(path) != 1 || is.na(path) || !nzchar(path)) {
    .enemdu_abort_missing_argument(
      "path",
      caller = "enemdu_read_official_dictionary_file"
    )
  }

  if (!file.exists(path)) {
    rlang::abort(
      message = glue::glue("Official dictionary file does not exist: `{path}`."),
      class = c("enemdu_error_missing_official_dictionary_file", "enemdu_error")
    )
  }

  raw <- .enemdu_read_official_dictionary_raw_table(
    path = path,
    sheet = sheet
  )

  out <- .enemdu_tidy_official_dictionary_table(
    raw = raw,
    source_file = basename(path),
    source_sheet = as.character(sheet),
    survey_type = survey_type %||% .enemdu_infer_survey_type_from_dictionary_file(path),
    period = period %||% .enemdu_infer_period_from_dictionary_file(path),
    dictionary_frequency = dictionary_frequency %||% .enemdu_infer_dictionary_frequency_from_file(path),
    dictionary_file_scope = dictionary_file_scope %||% .enemdu_infer_dictionary_scope_from_file(path)
  )

  attr(out, "official_dictionary_source") <- list(
    path = path,
    sheet = sheet,
    reader_backend = .enemdu_dictionary_reader_backend(path),
    note = "Dictionary parsed from official file using the first column as variable name and the second column as description."
  )

  class(out) <- unique(c("enemdu_official_dictionary", class(out)))
  out
}

#' Read official ENEMDU dictionaries from a ZIP file
#'
#' Extracts a ZIP file containing official ENEMDU dictionaries and reads all
#' files whose names include `Diccionario`.
#'
#' @param zip_path Path to a ZIP file.
#' @param pattern File-name pattern used to select dictionary files.
#' @param exdir Optional extraction directory.
#'
#' @return A tibble combining all parsed official dictionaries.
#' @export
enemdu_read_official_dictionary_zip <- function(zip_path,
                                                pattern = "Diccionario",
                                                exdir = NULL) {
  if (missing(zip_path) || is.null(zip_path) || length(zip_path) != 1 || is.na(zip_path) || !nzchar(zip_path)) {
    .enemdu_abort_missing_argument(
      "zip_path",
      caller = "enemdu_read_official_dictionary_zip"
    )
  }

  if (!file.exists(zip_path)) {
    rlang::abort(
      message = glue::glue("Official dictionary ZIP file does not exist: `{zip_path}`."),
      class = c("enemdu_error_missing_official_dictionary_zip", "enemdu_error")
    )
  }

  exdir <- exdir %||% tempfile("enemdu_official_dictionary_zip_")
  dir.create(exdir, recursive = TRUE, showWarnings = FALSE)

  files <- utils::unzip(zipfile = zip_path, exdir = exdir)

  dictionary_files <- files[
    grepl(pattern, basename(files), ignore.case = TRUE) &
      grepl("\\.(xlsx|xls|ods|xlsm)$", basename(files), ignore.case = TRUE)
  ]

  if (length(dictionary_files) == 0) {
    rlang::abort(
      message = glue::glue("No official dictionary files matching `{pattern}` were found in `{zip_path}`."),
      class = c("enemdu_error_no_dictionary_files_in_zip", "enemdu_error")
    )
  }

  pieces <- lapply(dictionary_files, enemdu_read_official_dictionary_file)
  out <- do.call(rbind, pieces)
  row.names(out) <- NULL
  out <- tibble::as_tibble(out)

  attr(out, "official_dictionary_zip_source") <- list(
    zip_path = zip_path,
    files = basename(dictionary_files),
    note = "Dictionary files were extracted and parsed from ZIP."
  )

  class(out) <- unique(c("enemdu_official_dictionary", class(out)))
  out
}

#' Validate a data frame against an official ENEMDU dictionary
#'
#' Compares the columns of a data frame against a parsed official dictionary.
#' This function does not mutate the data; it returns an auditable validation
#' table.
#'
#' @param data A data frame to validate.
#' @param dictionary Official dictionary tibble produced by
#' `enemdu_read_official_dictionary_file()` or an equivalent tibble with a
#' `variable` column.
#' @param required_vars Optional character vector of variables that must exist
#' in `data`.
#' @param check_all_dictionary_vars Logical. If `TRUE`, every dictionary variable
#' missing from data is flagged.
#' @param missing_dictionary_as_error Logical. If `TRUE`, missing dictionary
#' variables are severity `"error"` when `check_all_dictionary_vars = TRUE`.
#' @param allow_extra Logical. If `FALSE`, columns in data absent from the
#' dictionary are marked as warning-level issues.
#'
#' @return A tibble with validation status by variable.
#' @export
enemdu_validate_data_against_official_dictionary <- function(data,
                                                             dictionary,
                                                             required_vars = NULL,
                                                             check_all_dictionary_vars = FALSE,
                                                             missing_dictionary_as_error = FALSE,
                                                             allow_extra = TRUE) {
  if (!is.data.frame(data)) {
    .enemdu_abort_invalid_data(
      caller = "enemdu_validate_data_against_official_dictionary"
    )
  }

  dictionary_vars <- .enemdu_dictionary_variable_set(dictionary)
  data_vars <- names(data)

  required_vars <- required_vars %||% character(0)
  required_vars <- unique(as.character(required_vars))

  if (isTRUE(check_all_dictionary_vars)) {
    universe <- unique(c(dictionary_vars, data_vars, required_vars))
  } else {
    universe <- unique(c(data_vars, required_vars))
  }

  rows <- lapply(universe, function(var) {
    in_dictionary <- var %in% dictionary_vars
    in_data <- var %in% data_vars
    required <- var %in% required_vars

    status <- .enemdu_data_dictionary_validation_status(
      in_dictionary = in_dictionary,
      in_data = in_data,
      required = required,
      check_all_dictionary_vars = check_all_dictionary_vars,
      allow_extra = allow_extra
    )

    severity <- .enemdu_data_dictionary_validation_severity(
      status = status,
      missing_dictionary_as_error = missing_dictionary_as_error
    )

    tibble::tibble(
      variable = var,
      in_dictionary = in_dictionary,
      in_data = in_data,
      required = required,
      validation_status = status,
      severity = severity
    )
  })

  out <- do.call(rbind, rows)
  row.names(out) <- NULL
  out <- tibble::as_tibble(out)

  attr(out, "official_dictionary_data_validation_policy") <- list(
    required_vars = required_vars,
    check_all_dictionary_vars = check_all_dictionary_vars,
    missing_dictionary_as_error = missing_dictionary_as_error,
    allow_extra = allow_extra,
    note = "Validation compares data column names with official dictionary variables."
  )

  class(out) <- unique(c("enemdu_official_dictionary_data_validation", class(out)))
  out
}

#' Validate the package variable catalog against an official dictionary
#'
#' Compares `variable_catalog.csv` with an official ENEMDU dictionary. Aliases
#' declared in the package catalog are respected, so official names such as
#' `id_hogar` may validate the canonical internal variable `idhogar`.
#'
#' @param dictionary Official dictionary tibble.
#' @param variable_catalog Package variable catalog.
#' @param required_core_only Logical. If `TRUE`, validates only rows where
#' `required_core` is `TRUE`.
#'
#' @return A tibble with validation status by catalog row.
#' @export
enemdu_validate_catalog_against_official_dictionary <- function(
    dictionary,
    variable_catalog = enemdu_variable_catalog(),
    required_core_only = FALSE) {
  dictionary_vars <- .enemdu_dictionary_variable_set(dictionary)

  .enemdu_abort_missing_vars(
    vars = c("variable", "aliases", "required_core"),
    names_data = names(variable_catalog),
    caller = "enemdu_validate_catalog_against_official_dictionary"
  )

  catalog <- variable_catalog

  if (isTRUE(required_core_only)) {
    catalog <- catalog[as.logical(catalog$required_core), , drop = FALSE]
  }

  rows <- lapply(seq_len(nrow(catalog)), function(i) {
    variable <- as.character(catalog$variable[[i]])
    aliases <- .enemdu_parse_pipe_values(catalog$aliases[[i]])
    candidates <- unique(c(variable, aliases))
    matches <- intersect(candidates, dictionary_vars)

    matched_by <- if (length(matches) == 0) {
      NA_character_
    } else if (variable %in% matches) {
      "variable"
    } else {
      "alias"
    }

    matched_dictionary_variable <- if (length(matches) == 0) {
      NA_character_
    } else {
      matches[[1]]
    }

    status <- if (length(matches) == 0) {
      "missing_from_official_dictionary"
    } else {
      "covered_by_official_dictionary"
    }

    severity <- if (identical(status, "missing_from_official_dictionary") &&
                    isTRUE(as.logical(catalog$required_core[[i]]))) {
      "warning"
    } else if (identical(status, "missing_from_official_dictionary")) {
      "info"
    } else {
      "ok"
    }

    tibble::tibble(
      variable = variable,
      aliases = as.character(catalog$aliases[[i]]),
      required_core = as.logical(catalog$required_core[[i]]),
      matched_dictionary_variable = matched_dictionary_variable,
      matched_by = matched_by,
      validation_status = status,
      severity = severity
    )
  })

  out <- do.call(rbind, rows)
  row.names(out) <- NULL
  out <- tibble::as_tibble(out)

  attr(out, "official_dictionary_catalog_validation_policy") <- list(
    required_core_only = required_core_only,
    note = "Validation compares package variable catalog against official dictionary variables, allowing aliases."
  )

  class(out) <- unique(c("enemdu_official_dictionary_catalog_validation", class(out)))
  out
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

.enemdu_read_official_dictionary_raw_table <- function(path,
                                                       sheet = 1) {
  backend <- .enemdu_dictionary_reader_backend(path)

  if (identical(backend, "readxl")) {
    if (!requireNamespace("readxl", quietly = TRUE)) {
      rlang::abort(
        message = paste(
          "`readxl` is required to read Excel official ENEMDU dictionary files.",
          "Install it with install.packages('readxl')."
        ),
        class = c("enemdu_error_missing_readxl_dependency", "enemdu_error")
      )
    }

    return(readxl::read_excel(
      path = path,
      sheet = sheet,
      col_names = FALSE,
      .name_repair = "minimal"
    ))
  }

  if (identical(backend, "readODS")) {
    if (!requireNamespace("readODS", quietly = TRUE)) {
      rlang::abort(
        message = paste(
          "`readODS` is required to read ODS official ENEMDU dictionary files.",
          "Install it with install.packages('readODS')."
        ),
        class = c("enemdu_error_missing_readods_dependency", "enemdu_error")
      )
    }

    return(readODS::read_ods(
      path = path,
      sheet = sheet,
      col_names = FALSE
    ))
  }

  rlang::abort(
    message = glue::glue(
      "Unsupported official dictionary file extension `.{.enemdu_dictionary_file_extension(path)}`."
    ),
    class = c("enemdu_error_unsupported_official_dictionary_extension", "enemdu_error")
  )
}

.enemdu_dictionary_file_extension <- function(path) {
  tolower(tools::file_ext(path))
}

.enemdu_dictionary_reader_backend <- function(path) {
  ext <- .enemdu_dictionary_file_extension(path)

  if (ext %in% c("xlsx", "xls", "xlsm")) {
    return("readxl")
  }

  if (identical(ext, "ods")) {
    return("readODS")
  }

  "unsupported"
}

.enemdu_tidy_official_dictionary_table <- function(raw,
                                                   source_file,
                                                   source_sheet,
                                                   survey_type,
                                                   period,
                                                   dictionary_frequency,
                                                   dictionary_file_scope) {
  raw <- as.data.frame(raw, stringsAsFactors = FALSE)

  if (ncol(raw) < 2) {
    rlang::abort(
      message = "Official dictionary table must have at least two columns.",
      class = c("enemdu_error_invalid_official_dictionary_table", "enemdu_error")
    )
  }

  first_col <- .enemdu_normalize_dictionary_text(raw[[1]])
  header_row <- which(first_col == "nombre del campo")

  if (length(header_row) == 0) {
    rlang::abort(
      message = glue::glue("Could not find `Nombre del campo` header in `{source_file}`."),
      class = c("enemdu_error_official_dictionary_header_not_found", "enemdu_error")
    )
  }

  header_row <- header_row[[1]]
  start <- header_row + 1L

  if (start > nrow(raw)) {
    return(.enemdu_empty_official_dictionary())
  }

  body <- raw[start:nrow(raw), c(1, 2), drop = FALSE]
  names(body) <- c("variable", "description")

  body$variable <- trimws(as.character(body$variable))
  body$description <- trimws(as.character(body$description))

  body <- body[!is.na(body$variable) & nzchar(body$variable), , drop = FALSE]
  body <- body[body$variable != "NA", , drop = FALSE]

  if (nrow(body) == 0) {
    return(.enemdu_empty_official_dictionary())
  }

  out <- tibble::tibble(
    survey_type = survey_type %||% NA_character_,
    period = period %||% NA_character_,
    dictionary_frequency = dictionary_frequency %||% NA_character_,
    dictionary_file_scope = dictionary_file_scope %||% NA_character_,
    dictionary_file = source_file,
    dictionary_sheet = source_sheet,
    variable = body$variable,
    description = body$description
  )

  out
}

.enemdu_dictionary_variable_set <- function(dictionary) {
  if (!is.data.frame(dictionary)) {
    .enemdu_abort_invalid_data(caller = ".enemdu_dictionary_variable_set")
  }

  if ("variable" %in% names(dictionary)) {
    vars <- dictionary$variable
  } else if ("official_variable" %in% names(dictionary)) {
    vars <- dictionary$official_variable
  } else {
    .enemdu_abort_missing_vars(
      vars = "variable",
      names_data = names(dictionary),
      caller = ".enemdu_dictionary_variable_set"
    )
  }

  vars <- unique(trimws(as.character(vars)))
  vars[!is.na(vars) & nzchar(vars)]
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

.enemdu_data_dictionary_validation_status <- function(in_dictionary,
                                                      in_data,
                                                      required,
                                                      check_all_dictionary_vars,
                                                      allow_extra) {
  if (isTRUE(required) && !isTRUE(in_data)) {
    return("missing_required_variable")
  }

  if (isTRUE(in_data) && isTRUE(in_dictionary)) {
    return("present_in_data_and_dictionary")
  }

  if (isTRUE(in_data) && !isTRUE(in_dictionary)) {
    if (isTRUE(allow_extra)) {
      return("extra_data_variable_allowed")
    }

    return("extra_data_variable_not_in_dictionary")
  }

  if (!isTRUE(in_data) && isTRUE(in_dictionary) && isTRUE(check_all_dictionary_vars)) {
    return("dictionary_variable_missing_from_data")
  }

  "not_evaluated"
}

.enemdu_data_dictionary_validation_severity <- function(status,
                                                        missing_dictionary_as_error) {
  if (identical(status, "missing_required_variable")) {
    return("error")
  }

  if (identical(status, "extra_data_variable_not_in_dictionary")) {
    return("warning")
  }

  if (identical(status, "dictionary_variable_missing_from_data")) {
    if (isTRUE(missing_dictionary_as_error)) {
      return("error")
    }

    return("warning")
  }

  if (identical(status, "not_evaluated")) {
    return("info")
  }

  "ok"
}

.enemdu_infer_survey_type_from_dictionary_file <- function(path) {
  x <- tolower(basename(path))

  if (grepl("anual", x)) {
    return("anual")
  }

  if (grepl("trimestre|trimestral", x)) {
    return("trimestral")
  }

  "mensual"
}

.enemdu_infer_dictionary_frequency_from_file <- function(path) {
  survey_type <- .enemdu_infer_survey_type_from_dictionary_file(path)

  if (identical(survey_type, "anual")) {
    return("Anual")
  }

  if (identical(survey_type, "trimestral")) {
    return("Trimestral")
  }

  "Mensual"
}

.enemdu_infer_dictionary_scope_from_file <- function(path) {
  x <- tolower(basename(path))

  if (grepl("persona", x)) {
    return("persona")
  }

  if (grepl("vivienda_hogar", x)) {
    return("vivienda_hogar")
  }

  if (grepl("vivienda", x)) {
    return("vivienda")
  }

  if (grepl("consumidor", x)) {
    return("consumidor")
  }

  NA_character_
}

.enemdu_infer_period_from_dictionary_file <- function(path) {
  x <- basename(path)

  if (grepl("anual_2025|2025_anual", x, ignore.case = TRUE)) {
    return("2025")
  }

  if (grepl("2026_I_trimestre|2026_I_TRIMESTRE", x, ignore.case = TRUE)) {
    return("2026-I")
  }

  if (grepl("2026_03", x, ignore.case = TRUE)) {
    return("2026-03")
  }

  year <- regmatches(x, regexpr("20[0-9]{2}", x))

  if (length(year) == 1 && nzchar(year)) {
    return(year)
  }

  NA_character_
}

.enemdu_normalize_dictionary_text <- function(x) {
  x <- as.character(x)
  x <- trimws(x)
  x <- tolower(x)
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x <- gsub("[[:space:]]+", " ", x)
  x
}

.enemdu_empty_official_dictionary <- function() {
  tibble::tibble(
    survey_type = character(),
    period = character(),
    dictionary_frequency = character(),
    dictionary_file_scope = character(),
    dictionary_file = character(),
    dictionary_sheet = character(),
    variable = character(),
    description = character()
  )
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
