#' Return the ENEMDU value-range registry
#'
#' Reads the registry that defines valid ranges, ignored sentinel codes, and
#' variable-level value constraints.
#'
#' @return A tibble with value-range rules.
#' @export
enemdu_value_range_registry <- function() {
  .enemdu_value_range_registry()
}

#' Validate ENEMDU value ranges
#'
#' Checks numeric ranges and allowed values declared in
#' `value_range_registry.csv`. Registered sentinel codes can be ignored during
#' range validation so that missing-value normalization and range validation
#' remain conceptually separate.
#'
#' @param data A data frame.
#' @param registry Optional value-range registry.
#' @param vars Optional variables to validate.
#' @param strict Logical. If `TRUE`, errors when variables in `vars` are absent.
#'
#' @return A validation report tibble.
#' @export
enemdu_validate_value_ranges <- function(data,
                                         registry = enemdu_value_range_registry(),
                                         vars = NULL,
                                         strict = FALSE) {
  if (!is.data.frame(data)) {
    .enemdu_abort_invalid_data(caller = "enemdu_validate_value_ranges")
  }

  .enemdu_validate_value_range_registry(registry)

  if (is.null(vars)) {
    vars <- intersect(unique(registry$variable), names(data))
  } else {
    missing_vars <- setdiff(vars, names(data))

    if (length(missing_vars) > 0 && isTRUE(strict)) {
      .enemdu_abort_missing_vars(
        vars = vars,
        names_data = names(data),
        caller = "enemdu_validate_value_ranges"
      )
    }

    vars <- intersect(vars, names(data))
  }

  registry <- registry[registry$variable %in% vars, , drop = FALSE]

  if (nrow(registry) == 0) {
    return(tibble::tibble(
      check_id = character(),
      check_type = character(),
      severity = character(),
      status = character(),
      variable = character(),
      message = character(),
      n_affected = integer(),
      details = character()
    ))
  }

  pieces <- vector("list", nrow(registry))

  for (i in seq_len(nrow(registry))) {
    rule <- registry[i, , drop = FALSE]
    var <- rule$variable[[1]]
    x <- data[[var]]

    ignored_codes <- .enemdu_parse_pipe_values(rule$ignore_codes[[1]])
    values_chr <- .enemdu_values_as_character(x)
    ignored <- !is.na(values_chr) & values_chr %in% ignored_codes

    invalid <- rep(FALSE, length(x))
    evaluated <- !is.na(x) & !ignored

    allowed_values <- .enemdu_parse_pipe_values(rule$allowed_values[[1]])

    if (length(allowed_values) > 0) {
      invalid[evaluated] <- !(values_chr[evaluated] %in% allowed_values)
    } else {
      if (!is.numeric(x) && !is.integer(x)) {
        invalid[evaluated] <- TRUE
      } else {
        min_value <- suppressWarnings(as.numeric(rule$min_value[[1]]))
        max_value <- suppressWarnings(as.numeric(rule$max_value[[1]]))
        min_inclusive <- isTRUE(rule$min_inclusive[[1]])
        max_inclusive <- isTRUE(rule$max_inclusive[[1]])

        if (!is.na(min_value)) {
          if (min_inclusive) {
            invalid[evaluated] <- invalid[evaluated] | x[evaluated] < min_value
          } else {
            invalid[evaluated] <- invalid[evaluated] | x[evaluated] <= min_value
          }
        }

        if (!is.na(max_value)) {
          if (max_inclusive) {
            invalid[evaluated] <- invalid[evaluated] | x[evaluated] > max_value
          } else {
            invalid[evaluated] <- invalid[evaluated] | x[evaluated] >= max_value
          }
        }
      }
    }

    n_invalid <- sum(invalid, na.rm = TRUE)
    status <- if (n_invalid == 0) "pass" else "fail"
    severity <- if (n_invalid == 0) "info" else rule$severity[[1]]

    pieces[[i]] <- tibble::tibble(
      check_id = paste0("value_range_", var),
      check_type = "value_range",
      severity = severity,
      status = status,
      variable = var,
      message = if (n_invalid == 0) {
        glue::glue("Variable `{var}` satisfies the declared value-range rule.")
      } else {
        glue::glue("Variable `{var}` has {n_invalid} values outside the declared range or allowed set.")
      },
      n_affected = n_invalid,
      details = rule$description[[1]]
    )
  }

  out <- do.call(rbind, pieces)
  row.names(out) <- NULL

  class(out) <- unique(c("enemdu_value_range_report", "enemdu_validation_report", class(out)))
  out
}

.enemdu_validate_value_range_registry <- function(registry) {
  required_cols <- c(
    "variable",
    "variable_group",
    "min_value",
    "max_value",
    "min_inclusive",
    "max_inclusive",
    "allowed_values",
    "ignore_codes",
    "severity",
    "source_status",
    "description"
  )

  missing_cols <- setdiff(required_cols, names(registry))

  if (length(missing_cols) > 0) {
    .enemdu_abort_invalid_registry(
      registry_name = "value_range_registry",
      message = glue::glue("Missing columns: {paste(missing_cols, collapse = ', ')}."),
      caller = ".enemdu_validate_value_range_registry"
    )
  }

  invisible(TRUE)
}

.enemdu_parse_pipe_values <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x) || !nzchar(x)) {
    return(character(0))
  }

  trimws(unlist(strsplit(as.character(x), "\\|")))
}
