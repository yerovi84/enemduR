#' Return the ENEMDU poverty-line registry
#'
#' Reads the poverty-line registry. The package registry is intentionally
#' conservative: it provides the structure required for auditable poverty-line
#' management but does not invent official poverty-line values.
#'
#' @return A tibble with poverty-line metadata.
#' @export
enemdu_poverty_line_registry <- function() {
  .enemdu_poverty_line_registry()
}

#' Validate poverty-line registry
#'
#' Validates the structure and, optionally, the operational readiness of poverty
#' lines. Operational readiness requires positive line values, currency,
#' traceable source notes, and non-pending source status.
#'
#' @param registry Poverty-line registry. Defaults to the package registry.
#' @param require_valid Logical. If `TRUE`, rows with missing, non-positive, or
#' pending poverty-line values are marked as failures.
#'
#' @return A tibble with validation results by row.
#' @export
enemdu_validate_poverty_lines <- function(registry = enemdu_poverty_line_registry(),
                                          require_valid = FALSE) {
  .enemdu_validate_poverty_line_registry(registry)

  if (nrow(registry) == 0) {
    return(tibble::tibble(
      row_id = integer(),
      period = character(),
      line_type = character(),
      status = character(),
      severity = character(),
      message = character()
    ))
  }

  pieces <- vector("list", nrow(registry))

  for (i in seq_len(nrow(registry))) {
    row <- registry[i, , drop = FALSE]

    line_value <- suppressWarnings(as.numeric(row$line_value[[1]]))
    currency <- as.character(row$currency[[1]])
    source_status <- as.character(row$source_status[[1]])
    source_note <- as.character(row$source_note[[1]])

    failures <- character(0)

    if (isTRUE(require_valid)) {
      if (is.na(line_value) || line_value <= 0) {
        failures <- c(failures, "line_value must be numeric and greater than zero")
      }

      if (is.na(currency) || !nzchar(currency)) {
        failures <- c(failures, "currency must be declared")
      }

      if (is.na(source_note) || !nzchar(source_note)) {
        failures <- c(failures, "source_note must be declared")
      }

      if (is.na(source_status) || source_status %in% c("pending_review", "template", "")) {
        failures <- c(failures, "source_status must not be pending_review or template")
      }
    }

    status <- if (length(failures) == 0) "pass" else "fail"

    pieces[[i]] <- tibble::tibble(
      row_id = i,
      period = as.character(row$period[[1]]),
      line_type = as.character(row$line_type[[1]]),
      status = status,
      severity = if (identical(status, "pass")) "info" else "error",
      message = if (identical(status, "pass")) {
        "Poverty-line row satisfies the requested validation level."
      } else {
        paste(failures, collapse = "; ")
      }
    )
  }

  out <- do.call(rbind, pieces)
  row.names(out) <- NULL

  class(out) <- unique(c("enemdu_poverty_line_validation", class(out)))
  out
}

#' Get a poverty line for a given period
#'
#' Retrieves one auditable poverty-line row for a period and line type. In strict
#' mode, the function refuses pending, missing, non-positive, or non-traceable
#' poverty lines.
#'
#' @param period Period identifier, for example `"2024-12"`.
#' @param line_type One of `"poverty"` or `"extreme_poverty"`.
#' @param registry Poverty-line registry. Defaults to the package registry.
#' @param mode One of `"strict"` or `"allow_pending"`.
#'
#' @return A one-row tibble with the resolved poverty line.
#' @export
enemdu_get_poverty_line <- function(period,
                                    line_type = c("poverty", "extreme_poverty"),
                                    registry = enemdu_poverty_line_registry(),
                                    mode = c("strict", "allow_pending")) {
  if (missing(period) || is.null(period) || length(period) != 1 || is.na(period)) {
    .enemdu_abort_missing_argument("period", caller = "enemdu_get_poverty_line")
  }

  line_type <- match.arg(line_type)
  mode <- match.arg(mode)

  .enemdu_validate_poverty_line_registry(registry)

  period <- as.character(period)

  row <- registry[
    as.character(registry$period) == period &
      as.character(registry$line_type) == line_type,
    ,
    drop = FALSE
  ]

  if (nrow(row) != 1) {
    .enemdu_abort_missing_poverty_line(
      period = period,
      line_type = line_type,
      caller = "enemdu_get_poverty_line"
    )
  }

  validation <- enemdu_validate_poverty_lines(
    registry = row,
    require_valid = identical(mode, "strict")
  )

  if (any(validation$status == "fail")) {
    .enemdu_abort_invalid_poverty_line(
      message = paste(validation$message, collapse = "; "),
      caller = "enemdu_get_poverty_line"
    )
  }

  row$line_value <- suppressWarnings(as.numeric(row$line_value))
  row$ipc_value <- suppressWarnings(as.numeric(row$ipc_value))
  row$base_line_value <- suppressWarnings(as.numeric(row$base_line_value))

  class(row) <- unique(c("enemdu_poverty_line", class(row)))
  row
}

.enemdu_validate_poverty_line_registry <- function(registry) {
  required_cols <- c(
    "period",
    "period_type",
    "line_type",
    "line_value",
    "currency",
    "ipc_value",
    "base_line_value",
    "base_period",
    "update_method",
    "source_status",
    "source_note",
    "valid_from",
    "valid_to",
    "notes"
  )

  missing_cols <- setdiff(required_cols, names(registry))

  if (length(missing_cols) > 0) {
    .enemdu_abort_invalid_registry(
      registry_name = "poverty_line_registry",
      message = glue::glue("Missing columns: {paste(missing_cols, collapse = ', ')}."),
      caller = ".enemdu_validate_poverty_line_registry"
    )
  }

  allowed_line_types <- c("poverty", "extreme_poverty")
  invalid_line_types <- setdiff(unique(as.character(registry$line_type)), allowed_line_types)

  if (length(invalid_line_types) > 0) {
    .enemdu_abort_invalid_registry(
      registry_name = "poverty_line_registry",
      message = glue::glue(
        "Invalid line_type values: {paste(invalid_line_types, collapse = ', ')}."
      ),
      caller = ".enemdu_validate_poverty_line_registry"
    )
  }

  invisible(TRUE)
}

.enemdu_assert_positive_scalar <- function(x, arg, caller) {
  if (is.null(x) || length(x) != 1 || is.na(x) || !is.numeric(x) || x <= 0) {
    rlang::abort(
      message = glue::glue("`{arg}` must be a single positive numeric value in `{caller}()`."),
      class = c("enemdu_error_invalid_positive_scalar", "enemdu_error")
    )
  }

  invisible(TRUE)
}
