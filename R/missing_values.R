#' Return the ENEMDU missing-code registry
#'
#' Reads the metadata registry that defines special missing-value codes,
#' sentinel nonresponse codes, structural skips, and project-internal rules.
#'
#' @return A tibble with missing-code rules.
#' @export
enemdu_missing_code_registry <- function() {
  .enemdu_missing_code_registry()
}

#' Detect registered sentinel values in ENEMDU data
#'
#' Scans selected variables for special codes registered in
#' `missing_code_registry.csv`. This function does not modify the data.
#'
#' @param data A data frame.
#' @param registry Optional missing-code registry. Defaults to package registry.
#' @param vars Optional character vector of variables to scan.
#' @param applies_to Scope of rules to use. Use `"all"` to include all rules.
#'
#' @return A tibble with detected codes and counts.
#' @export
enemdu_detect_sentinel_values <- function(data,
                                          registry = enemdu_missing_code_registry(),
                                          vars = NULL,
                                          applies_to = "all") {
  if (!is.data.frame(data)) {
    .enemdu_abort_invalid_data(caller = "enemdu_detect_sentinel_values")
  }

  .enemdu_validate_missing_registry(registry)

  if (is.null(vars)) {
    vars <- intersect(unique(registry$variable), names(data))
  } else {
    .enemdu_abort_missing_vars(
      vars = vars,
      names_data = names(data),
      caller = "enemdu_detect_sentinel_values"
    )
  }

  registry <- .enemdu_filter_missing_registry(
    registry = registry,
    vars = vars,
    applies_to = applies_to
  )

  if (nrow(registry) == 0) {
    return(tibble::tibble(
      variable = character(),
      code = character(),
      code_type = character(),
      meaning = character(),
      action = character(),
      n = integer(),
      pct = numeric(),
      source_status = character(),
      source_note = character()
    ))
  }

  pieces <- vector("list", nrow(registry))

  for (i in seq_len(nrow(registry))) {
    var <- registry$variable[i]
    code <- as.character(registry$code[i])

    values_chr <- .enemdu_values_as_character(data[[var]])
    detected <- !is.na(values_chr) & values_chr == code
    n_detected <- sum(detected)

    pieces[[i]] <- tibble::tibble(
      variable = var,
      code = code,
      code_type = registry$code_type[i],
      meaning = registry$meaning[i],
      action = registry$action[i],
      n = n_detected,
      pct = if (length(values_chr) == 0) NA_real_ else n_detected / length(values_chr),
      source_status = registry$source_status[i],
      source_note = registry$source_note[i]
    )
  }

  out <- do.call(rbind, pieces)
  out <- out[out$n > 0, , drop = FALSE]
  row.names(out) <- NULL

  class(out) <- unique(c("enemdu_sentinel_report", class(out)))
  out
}

#' Build a missing-value report
#'
#' Reports system missing values and registered special codes. This function is
#' diagnostic and does not modify the data.
#'
#' @param data A data frame.
#' @param registry Optional missing-code registry. Defaults to package registry.
#' @param vars Optional character vector of variables to report.
#' @param include_na Logical. If `TRUE`, includes system `NA` counts.
#' @param applies_to Scope of rules to use. Use `"all"` to include all rules.
#'
#' @return A tibble with missing and sentinel counts.
#' @export
enemdu_missing_report <- function(data,
                                  registry = enemdu_missing_code_registry(),
                                  vars = NULL,
                                  include_na = TRUE,
                                  applies_to = "all") {
  if (!is.data.frame(data)) {
    .enemdu_abort_invalid_data(caller = "enemdu_missing_report")
  }

  .enemdu_validate_missing_registry(registry)

  if (is.null(vars)) {
    vars <- intersect(unique(registry$variable), names(data))
  } else {
    .enemdu_abort_missing_vars(
      vars = vars,
      names_data = names(data),
      caller = "enemdu_missing_report"
    )
  }

  sentinel_report <- enemdu_detect_sentinel_values(
    data = data,
    registry = registry,
    vars = vars,
    applies_to = applies_to
  )

  na_report <- tibble::tibble(
    variable = character(),
    code = character(),
    code_type = character(),
    meaning = character(),
    action = character(),
    n = integer(),
    pct = numeric(),
    source_status = character(),
    source_note = character()
  )

  if (isTRUE(include_na)) {
    na_pieces <- vector("list", length(vars))

    for (i in seq_along(vars)) {
      var <- vars[[i]]
      n_na <- sum(is.na(data[[var]]))

      na_pieces[[i]] <- tibble::tibble(
        variable = var,
        code = NA_character_,
        code_type = "system_missing",
        meaning = "System missing value read by R.",
        action = "keep_na",
        n = n_na,
        pct = if (nrow(data) == 0) NA_real_ else n_na / nrow(data),
        source_status = "system",
        source_note = "R NA value."
      )
    }

    na_report <- do.call(rbind, na_pieces)
  }

  out <- rbind(na_report, sentinel_report)
  row.names(out) <- NULL

  class(out) <- unique(c("enemdu_missing_report", class(out)))
  out
}

#' Normalize registered missing values
#'
#' Replaces registered special missing or nonresponse codes with `NA` only when
#' the registry explicitly authorizes `action = "set_na"`. This function is
#' intentionally conservative: it does not perform global recoding.
#'
#' @param data A data frame.
#' @param registry Optional missing-code registry. Defaults to package registry.
#' @param vars Optional variables to normalize. If omitted, uses registered
#' variables present in the data.
#' @param applies_to Scope of rules to apply. Common values are
#' `"analysis"`, `"income_derivation"`, `"representativity"`, and `"all"`.
#' @param keep_raw Logical. If `TRUE`, creates raw backup variables before
#' recoding.
#' @param raw_suffix Suffix for raw backup variables.
#' @param create_flags Logical. If `TRUE`, creates missing audit variables.
#' @param flag_suffix Suffix for logical missing flags.
#' @param code_suffix Suffix for detected code variables.
#' @param type_suffix Suffix for detected missing-type variables.
#' @param overwrite_raw Logical. If `TRUE`, overwrites existing raw backup
#' variables.
#' @param strict Logical. If `TRUE`, errors when registered variables are absent.
#'
#' @return A data frame with registered codes normalized and audit attributes.
#' @export
enemdu_normalize_missing_values <- function(data,
                                            registry = enemdu_missing_code_registry(),
                                            vars = NULL,
                                            applies_to = "analysis",
                                            keep_raw = TRUE,
                                            raw_suffix = "_raw",
                                            create_flags = TRUE,
                                            flag_suffix = "_missing_flag",
                                            code_suffix = "_missing_code",
                                            type_suffix = "_missing_type",
                                            overwrite_raw = FALSE,
                                            strict = FALSE) {
  if (!is.data.frame(data)) {
    .enemdu_abort_invalid_data(caller = "enemdu_normalize_missing_values")
  }

  .enemdu_validate_missing_registry(registry)

  if (is.null(vars)) {
    vars <- intersect(unique(registry$variable), names(data))
  } else {
    missing_vars <- setdiff(vars, names(data))

    if (length(missing_vars) > 0 && isTRUE(strict)) {
      .enemdu_abort_missing_vars(
        vars = vars,
        names_data = names(data),
        caller = "enemdu_normalize_missing_values"
      )
    }

    vars <- intersect(vars, names(data))
  }

  if (length(vars) == 0) {
    .enemdu_warn_missing_normalization(
      message = "No variables were available for missing-value normalization.",
      caller = "enemdu_normalize_missing_values"
    )

    attr(data, "missing_normalization_log") <- tibble::tibble(
      variable = character(),
      code = character(),
      n_replaced = integer(),
      action = character(),
      source_status = character()
    )

    return(data)
  }

  registry <- .enemdu_filter_missing_registry(
    registry = registry,
    vars = vars,
    applies_to = applies_to
  )

  registry <- registry[registry$action == "set_na", , drop = FALSE]

  if (nrow(registry) == 0) {
    attr(data, "missing_normalization_log") <- tibble::tibble(
      variable = character(),
      code = character(),
      n_replaced = integer(),
      action = character(),
      source_status = character()
    )

    return(data)
  }

  out <- data
  log_rows <- list()
  log_index <- 1L

  for (var in unique(registry$variable)) {
    var_rules <- registry[registry$variable == var, , drop = FALSE]

    if (isTRUE(keep_raw)) {
      raw_name <- paste0(var, raw_suffix)

      if (raw_name %in% names(out) && !isTRUE(overwrite_raw)) {
        rlang::abort(
          message = glue::glue(
            "Raw backup variable `{raw_name}` already exists. ",
            "Use `overwrite_raw = TRUE` to replace it."
          ),
          class = c("enemdu_error_existing_raw_backup", "enemdu_error")
        )
      }

      out[[raw_name]] <- out[[var]]
    }

    values_chr <- .enemdu_values_as_character(out[[var]])

    if (isTRUE(create_flags)) {
      out[[paste0(var, flag_suffix)]] <- FALSE
      out[[paste0(var, code_suffix)]] <- NA_character_
      out[[paste0(var, type_suffix)]] <- NA_character_
    }

    for (i in seq_len(nrow(var_rules))) {
      code <- as.character(var_rules$code[i])
      idx <- !is.na(values_chr) & values_chr == code
      n_replaced <- sum(idx)

      if (n_replaced > 0) {
        out[[var]][idx] <- NA

        if (isTRUE(create_flags)) {
          out[[paste0(var, flag_suffix)]][idx] <- TRUE
          out[[paste0(var, code_suffix)]][idx] <- code
          out[[paste0(var, type_suffix)]][idx] <- var_rules$code_type[i]
        }
      }

      log_rows[[log_index]] <- tibble::tibble(
        variable = var,
        code = code,
        n_replaced = n_replaced,
        action = var_rules$action[i],
        code_type = var_rules$code_type[i],
        source_status = var_rules$source_status[i],
        source_note = var_rules$source_note[i]
      )

      log_index <- log_index + 1L
    }
  }

  normalization_log <- do.call(rbind, log_rows)
  attr(out, "missing_normalization_log") <- normalization_log
  attr(out, "missing_normalization_policy") <- list(
    applies_to = applies_to,
    keep_raw = keep_raw,
    create_flags = create_flags,
    note = paste(
      "Only registry-authorized codes with action = 'set_na' were normalized.",
      "No global recoding was applied."
    )
  )

  class(out) <- unique(c("enemdu_tbl", class(out)))
  out
}

.enemdu_validate_missing_registry <- function(registry) {
  required_cols <- c(
    "variable",
    "variable_group",
    "code",
    "code_type",
    "meaning",
    "action",
    "applies_to",
    "source_status",
    "source_note"
  )

  missing_cols <- setdiff(required_cols, names(registry))

  if (length(missing_cols) > 0) {
    .enemdu_abort_invalid_registry(
      registry_name = "missing_code_registry",
      message = glue::glue("Missing columns: {paste(missing_cols, collapse = ', ')}."),
      caller = ".enemdu_validate_missing_registry"
    )
  }

  invisible(TRUE)
}

.enemdu_filter_missing_registry <- function(registry, vars, applies_to = "all") {
  registry <- registry[registry$variable %in% vars, , drop = FALSE]

  if (!identical(applies_to, "all")) {
    registry <- registry[
      registry$applies_to %in% c(applies_to, "analysis", "all"),
      ,
      drop = FALSE
    ]
  }

  registry
}

.enemdu_values_as_character <- function(x) {
  if (inherits(x, "haven_labelled")) {
    x <- haven::zap_labels(x)
  }

  if (is.factor(x)) {
    x <- as.character(x)
  }

  if (is.numeric(x) || is.integer(x)) {
    out <- ifelse(is.na(x), NA_character_, format(x, scientific = FALSE, trim = TRUE))
    out <- sub("\\.0+$", "", out)
    return(out)
  }

  if (is.logical(x)) {
    return(ifelse(is.na(x), NA_character_, as.character(as.integer(x))))
  }

  out <- as.character(x)
  out[is.na(x)] <- NA_character_
  trimws(out)
}
