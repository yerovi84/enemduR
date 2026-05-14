#' Required raw ENEMDU microdata variables
#'
#' Returns a conservative set of raw ENEMDU variables expected for structural
#' validation of current monthly, quarterly, or annual microdata.
#'
#' This helper is designed for validation of raw or near-raw ENEMDU files. It
#' therefore uses `id_hogar` as the raw official household identifier, while the
#' package can later standardize it to the internal canonical `idhogar`.
#'
#' @param survey_type Optional survey type: `"mensual"`, `"trimestral"` or
#' `"anual"`.
#' @param include_design Logical. Include `upm`, `estrato`, and `fexp`.
#' @param include_domain Logical. Include expected design-domain variables by
#' survey type.
#' @param include_household_id Logical. Include `id_hogar`.
#' @param include_income_core Logical. Include core income variables used in the
#' current income contract.
#' @param include_social_bonuses Logical. Include p75/p76 and p77/p78 social
#' bonus pairs.
#'
#' @return Character vector of required raw variables.
#' @export
enemdu_required_microdata_variables <- function(survey_type = NULL,
                                                include_design = TRUE,
                                                include_domain = TRUE,
                                                include_household_id = TRUE,
                                                include_income_core = TRUE,
                                                include_social_bonuses = TRUE) {
  vars <- character(0)

  if (isTRUE(include_design)) {
    vars <- c(vars, "upm", "estrato", "fexp")
  }

  if (isTRUE(include_household_id)) {
    vars <- c(vars, "id_hogar")
  }

  if (isTRUE(include_domain)) {
    if (is.null(survey_type)) {
      vars <- c(vars, "area")
    } else {
      survey_type <- .enemdu_normalize_survey_type(
        survey_type = survey_type,
        caller = "enemdu_required_microdata_variables"
      )

      if (identical(survey_type, "mensual")) {
        vars <- c(vars, "area")
      }

      if (identical(survey_type, "trimestral")) {
        vars <- c(vars, "area", "ciudad")
      }

      if (identical(survey_type, "anual")) {
        vars <- c(vars, "area", "ciudad", "prov")
      }
    }
  }

  if (isTRUE(include_income_core)) {
    vars <- c(
      vars,
      "p63", "p64b", "p65", "p66", "p67", "p68b",
      "p69", "p70b",
      "p71a", "p71b",
      "p72a", "p72b",
      "p73a", "p73b",
      "p74a", "p74b",
      "p75", "p76"
    )
  }

  if (isTRUE(include_social_bonuses)) {
    vars <- c(vars, "p75", "p76", "p77", "p78")
  }

  unique(vars)
}

#' Validate ENEMDU microdata against an official dictionary
#'
#' Validates a data frame against an official ENEMDU dictionary and the current
#' package structural contract. It checks variable presence, required variables,
#' and social bonus amount/receipt pairs.
#'
#' @param data A data frame.
#' @param dictionary Optional official dictionary tibble produced by
#' `enemdu_read_official_dictionary_file()` or
#' `enemdu_read_official_dictionary_zip()`.
#' @param survey_type Optional survey type.
#' @param required_vars Optional extra required variables.
#' @param include_design Logical. Include design variables in required checks.
#' @param include_domain Logical. Include domain variables in required checks.
#' @param include_household_id Logical. Include household identifier in required
#' checks.
#' @param include_income_core Logical. Include core income variables in required
#' checks.
#' @param include_social_bonuses Logical. Include p75/p76 and p77/p78 social
#' bonus pair checks.
#' @param allow_extra Logical. If `FALSE`, data columns not present in dictionary
#' are warning-level issues.
#' @param emit Logical. If `TRUE`, emits a short validation summary.
#'
#' @return A tibble with validation results.
#' @export
enemdu_validate_microdata_against_dictionary <- function(
    data,
    dictionary = NULL,
    survey_type = NULL,
    required_vars = NULL,
    include_design = TRUE,
    include_domain = TRUE,
    include_household_id = TRUE,
    include_income_core = TRUE,
    include_social_bonuses = TRUE,
    allow_extra = TRUE,
    emit = TRUE) {
  if (!is.data.frame(data)) {
    .enemdu_abort_invalid_data(
      caller = "enemdu_validate_microdata_against_dictionary"
    )
  }

  contract_required <- enemdu_required_microdata_variables(
    survey_type = survey_type,
    include_design = include_design,
    include_domain = include_domain,
    include_household_id = include_household_id,
    include_income_core = include_income_core,
    include_social_bonuses = include_social_bonuses
  )

  required_vars <- unique(c(contract_required, required_vars %||% character(0)))

  if (is.null(dictionary)) {
    dictionary <- tibble::tibble(
      variable = unique(c(names(data), required_vars)),
      description = NA_character_
    )
  }

  base <- enemdu_validate_data_against_official_dictionary(
    data = data,
    dictionary = dictionary,
    required_vars = required_vars,
    check_all_dictionary_vars = FALSE,
    missing_dictionary_as_error = FALSE,
    allow_extra = allow_extra
  )

  base[["check_type"]] <- "variable_presence"
  base[["pair_id"]] <- NA_character_
  base[["amount_variable"]] <- NA_character_
  base[["condition_variable"]] <- NA_character_
  base[["message"]] <- .enemdu_microdata_validation_message(
    validation_status = base$validation_status,
    variable = base$variable
  )

  pair_rows <- tibble::tibble()

  if (isTRUE(include_social_bonuses)) {
    pair_rows <- .enemdu_validate_social_bonus_pairs_in_microdata(
      data = data,
      dictionary = dictionary
    )
  }

  out <- .enemdu_bind_microdata_validation_rows(base, pair_rows)

  attr(out, "microdata_dictionary_validation_policy") <- list(
    survey_type = survey_type %||% NA_character_,
    required_vars = required_vars,
    include_design = include_design,
    include_domain = include_domain,
    include_household_id = include_household_id,
    include_income_core = include_income_core,
    include_social_bonuses = include_social_bonuses,
    allow_extra = allow_extra,
    note = paste(
      "Validation checks raw microdata variable presence against official dictionary and package contract.",
      "It does not mutate the data."
    )
  )

  if (isTRUE(emit)) {
    n_error <- sum(out$severity == "error", na.rm = TRUE)
    n_warning <- sum(out$severity == "warning", na.rm = TRUE)

    rlang::inform(
      message = glue::glue(
        "Microdata dictionary validation completed: ",
        "{n_error} error-level issue(s), {n_warning} warning-level issue(s)."
      ),
      class = c("enemdu_message_microdata_dictionary_validation", "enemdu_message")
    )
  }

  class(out) <- unique(c("enemdu_microdata_dictionary_validation", class(out)))
  out
}

#' Validate a microdata file against an official dictionary
#'
#' Reads a microdata file and validates its columns against an official ENEMDU
#' dictionary.
#'
#' Supported formats:
#' - `.dta`
#' - `.sav`
#' - `.rds`
#' - `.csv`
#'
#' @param path Path to a microdata file.
#' @param dictionary Official dictionary tibble.
#' @param n_max Optional maximum number of rows to read. Useful for fast
#' structural validation.
#' @param ... Additional arguments passed to
#' `enemdu_validate_microdata_against_dictionary()`.
#'
#' @return A tibble with validation results.
#' @export
enemdu_validate_microdata_file_against_dictionary <- function(path,
                                                              dictionary,
                                                              n_max = NULL,
                                                              ...) {
  data <- .enemdu_read_microdata_file_for_validation(
    path = path,
    n_max = n_max
  )

  enemdu_validate_microdata_against_dictionary(
    data = data,
    dictionary = dictionary,
    ...
  )
}

#' Validate a microdata file against official dictionaries in a ZIP
#'
#' Reads official dictionaries from a ZIP file, optionally filters by dictionary
#' scope and survey type, then validates a microdata file against the resulting
#' dictionary.
#'
#' @param data_path Path to a microdata file.
#' @param dictionary_zip_path Path to a ZIP file containing official dictionary
#' files.
#' @param dictionary_file_scope Optional dictionary scope filter. Defaults to
#' `"persona"`.
#' @param survey_type Optional survey type filter.
#' @param n_max Optional maximum number of microdata rows to read.
#' @param ... Additional arguments passed to
#' `enemdu_validate_microdata_against_dictionary()`.
#'
#' @return A tibble with validation results.
#' @export
enemdu_validate_microdata_file_against_dictionary_zip <- function(
    data_path,
    dictionary_zip_path,
    dictionary_file_scope = "persona",
    survey_type = NULL,
    n_max = NULL,
    ...) {
  dictionary <- enemdu_read_official_dictionary_zip(dictionary_zip_path)

  if (!is.null(dictionary_file_scope) &&
      "dictionary_file_scope" %in% names(dictionary)) {
    dictionary <- dictionary[
      dictionary$dictionary_file_scope %in% dictionary_file_scope,
      ,
      drop = FALSE
    ]
  }

  if (!is.null(survey_type) && "survey_type" %in% names(dictionary)) {
    survey_type <- .enemdu_normalize_survey_type(
      survey_type = survey_type,
      caller = "enemdu_validate_microdata_file_against_dictionary_zip"
    )

    dictionary <- dictionary[
      dictionary$survey_type == survey_type,
      ,
      drop = FALSE
    ]
  }

  if (nrow(dictionary) == 0) {
    rlang::abort(
      message = "No dictionary rows remained after filtering dictionary ZIP contents.",
      class = c("enemdu_error_empty_filtered_official_dictionary", "enemdu_error")
    )
  }

  enemdu_validate_microdata_file_against_dictionary(
    path = data_path,
    dictionary = dictionary,
    survey_type = survey_type,
    n_max = n_max,
    ...
  )
}

.enemdu_read_microdata_file_for_validation <- function(path,
                                                       n_max = NULL) {
  if (missing(path) || is.null(path) || length(path) != 1 || is.na(path) || !nzchar(path)) {
    .enemdu_abort_missing_argument(
      "path",
      caller = ".enemdu_read_microdata_file_for_validation"
    )
  }

  if (!file.exists(path)) {
    rlang::abort(
      message = glue::glue("Microdata file does not exist: `{path}`."),
      class = c("enemdu_error_missing_microdata_file", "enemdu_error")
    )
  }

  ext <- tolower(tools::file_ext(path))

  data <- tryCatch(
    {
      if (identical(ext, "dta")) {
        haven::read_dta(path, n_max = n_max %||% Inf)
      } else if (identical(ext, "sav")) {
        haven::read_sav(path, n_max = n_max %||% Inf)
      } else if (identical(ext, "rds")) {
        readRDS(path)
      } else if (identical(ext, "csv")) {
        if (is.null(n_max)) {
          readr::read_csv(path, show_col_types = FALSE)
        } else {
          readr::read_csv(path, n_max = n_max, show_col_types = FALSE)
        }
      } else {
        rlang::abort(
          message = glue::glue("Unsupported microdata file extension `.{ext}`."),
          class = c("enemdu_error_unsupported_microdata_extension", "enemdu_error")
        )
      }
    },
    error = function(e) {
      rlang::abort(
        message = glue::glue(
          "Could not read microdata file `{path}` for validation: {e$message}"
        ),
        parent = e,
        class = c("enemdu_error_microdata_file_read_failed", "enemdu_error")
      )
    }
  )

  if (!is.data.frame(data)) {
    .enemdu_abort_invalid_data(
      caller = ".enemdu_read_microdata_file_for_validation"
    )
  }

  data
}

.enemdu_validate_social_bonus_pairs_in_microdata <- function(data,
                                                             dictionary = NULL) {
  pairs <- tibble::tibble(
    pair_id = c("bono_desarrollo_humano", "bono_discapacidad"),
    condition_variable = c("p75", "p77"),
    amount_variable = c("p76", "p78")
  )

  dictionary_vars <- if (is.null(dictionary)) {
    character(0)
  } else {
    .enemdu_dictionary_variable_set(dictionary)
  }

  rows <- lapply(seq_len(nrow(pairs)), function(i) {
    condition_var <- pairs$condition_variable[[i]]
    amount_var <- pairs$amount_variable[[i]]

    condition_in_data <- condition_var %in% names(data)
    amount_in_data <- amount_var %in% names(data)

    condition_in_dictionary <- if (length(dictionary_vars) == 0) {
      NA
    } else {
      condition_var %in% dictionary_vars
    }

    amount_in_dictionary <- if (length(dictionary_vars) == 0) {
      NA
    } else {
      amount_var %in% dictionary_vars
    }

    pair_status <- .enemdu_social_bonus_pair_status(
      condition_in_data = condition_in_data,
      amount_in_data = amount_in_data
    )

    severity <- .enemdu_social_bonus_pair_severity(pair_status)

    tibble::tibble(
      variable = paste(condition_var, amount_var, sep = "/"),
      in_dictionary = isTRUE(condition_in_dictionary) && isTRUE(amount_in_dictionary),
      in_data = isTRUE(condition_in_data) && isTRUE(amount_in_data),
      required = TRUE,
      validation_status = pair_status,
      severity = severity,
      check_type = "social_bonus_pair",
      pair_id = pairs$pair_id[[i]],
      amount_variable = amount_var,
      condition_variable = condition_var,
      message = .enemdu_social_bonus_pair_message(
        pair_id = pairs$pair_id[[i]],
        condition_variable = condition_var,
        amount_variable = amount_var,
        pair_status = pair_status
      )
    )
  })

  out <- do.call(rbind, rows)
  row.names(out) <- NULL
  tibble::as_tibble(out)
}

.enemdu_social_bonus_pair_status <- function(condition_in_data,
                                             amount_in_data) {
  if (isTRUE(condition_in_data) && isTRUE(amount_in_data)) {
    return("pair_complete")
  }

  if (isTRUE(condition_in_data) && !isTRUE(amount_in_data)) {
    return("missing_amount_variable")
  }

  if (!isTRUE(condition_in_data) && isTRUE(amount_in_data)) {
    return("missing_condition_variable")
  }

  "missing_pair"
}

.enemdu_social_bonus_pair_severity <- function(pair_status) {
  if (identical(pair_status, "pair_complete")) {
    return("ok")
  }

  "error"
}

.enemdu_social_bonus_pair_message <- function(pair_id,
                                              condition_variable,
                                              amount_variable,
                                              pair_status) {
  if (identical(pair_status, "pair_complete")) {
    return(glue::glue(
      "Pair `{pair_id}` is complete: `{condition_variable}` validates `{amount_variable}`."
    ))
  }

  if (identical(pair_status, "missing_amount_variable")) {
    return(glue::glue(
      "Pair `{pair_id}` is incomplete: `{condition_variable}` exists but `{amount_variable}` is missing."
    ))
  }

  if (identical(pair_status, "missing_condition_variable")) {
    return(glue::glue(
      "Pair `{pair_id}` is incomplete: `{amount_variable}` exists but `{condition_variable}` is missing."
    ))
  }

  glue::glue(
    "Pair `{pair_id}` is missing: neither `{condition_variable}` nor `{amount_variable}` is available."
  )
}

.enemdu_microdata_validation_message <- function(validation_status,
                                                 variable) {
  vapply(
    seq_along(validation_status),
    function(i) {
      status <- validation_status[[i]]
      var <- variable[[i]]

      if (identical(status, "missing_required_variable")) {
        return(glue::glue("Required variable `{var}` is missing from data."))
      }

      if (identical(status, "present_in_data_and_dictionary")) {
        return(glue::glue("Variable `{var}` is present in data and dictionary."))
      }

      if (identical(status, "extra_data_variable_allowed")) {
        return(glue::glue("Variable `{var}` is present in data and allowed as extra variable."))
      }

      if (identical(status, "extra_data_variable_not_in_dictionary")) {
        return(glue::glue("Variable `{var}` is present in data but absent from dictionary."))
      }

      if (identical(status, "dictionary_variable_missing_from_data")) {
        return(glue::glue("Dictionary variable `{var}` is missing from data."))
      }

      glue::glue("Variable `{var}` was not evaluated.")
    },
    character(1)
  )
}

.enemdu_bind_microdata_validation_rows <- function(base,
                                                   pair_rows) {
  required_cols <- c(
    "variable",
    "in_dictionary",
    "in_data",
    "required",
    "validation_status",
    "severity",
    "check_type",
    "pair_id",
    "amount_variable",
    "condition_variable",
    "message"
  )

  for (col in required_cols) {
    if (!col %in% names(base)) {
      base[[col]] <- NA
    }

    if (!col %in% names(pair_rows)) {
      pair_rows[[col]] <- NA
    }
  }

  out <- rbind(
    as.data.frame(base[required_cols], stringsAsFactors = FALSE),
    as.data.frame(pair_rows[required_cols], stringsAsFactors = FALSE)
  )

  row.names(out) <- NULL
  tibble::as_tibble(out)
}
