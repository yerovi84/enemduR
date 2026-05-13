#' Validate ENEMDU domain scope
#'
#' Validates whether a requested domain is a design domain for the selected
#' ENEMDU survey type. Domains outside the design scope are not automatically
#' forbidden, but they are classified as analysis domains requiring precision
#' evaluation and clear reporting.
#'
#' @param survey_type One of `"mensual"`, `"trimestral"` or `"anual"`.
#' @param domain_level Optional domain level, for example `"nacional"`,
#' `"urbano_rural"`, `"ciudad_5"`, `"provincia_24"`, `"canton"` or
#' `"subpoblacion_sociodemografica"`.
#' @param domain_var Optional variable name used for the domain.
#' @param group_vars Optional grouping variables. When `domain_level` is not
#' supplied, the function tries to infer domain levels from these variables.
#' @param domain_registry Domain registry. Defaults to package registry.
#' @param domain_variable_registry Domain-variable registry. Defaults to package
#' registry.
#' @param strict Logical. If `TRUE`, errors when the requested domain is not a
#' design domain.
#' @param emit Logical. If `TRUE`, emits an informative message.
#'
#' @return A tibble with domain-scope validation results.
#' @export
enemdu_validate_domain_scope <- function(survey_type,
                                         domain_level = NULL,
                                         domain_var = NULL,
                                         group_vars = NULL,
                                         domain_registry = enemdu_domain_registry(),
                                         domain_variable_registry = enemdu_domain_variable_registry(),
                                         strict = FALSE,
                                         emit = TRUE) {
  survey_type <- .enemdu_normalize_survey_type(
    survey_type = survey_type,
    caller = "enemdu_validate_domain_scope"
  )

  .enemdu_validate_domain_registry(domain_registry)
  .enemdu_validate_domain_variable_registry(domain_variable_registry)

  requested <- .enemdu_build_domain_requests(
    domain_level = domain_level,
    domain_var = domain_var,
    group_vars = group_vars,
    domain_variable_registry = domain_variable_registry
  )

  pieces <- vector("list", nrow(requested))

  for (i in seq_len(nrow(requested))) {
    level_i <- requested$domain_level[[i]]
    var_i <- requested$domain_var[[i]]

    row <- domain_registry[
      domain_registry$survey_type == survey_type &
        domain_registry$domain_level == level_i,
      ,
      drop = FALSE
    ]

    if (nrow(row) == 0) {
      row <- tibble::tibble(
        survey_type = survey_type,
        domain_level = level_i,
        is_design_domain = FALSE,
        requires_precision_evaluation = TRUE,
        description = "Domain level not registered for this survey type."
      )
    }

    is_design_domain <- .enemdu_as_logical(row$is_design_domain[[1]])
    requires_precision <- .enemdu_as_logical(row$requires_precision_evaluation[[1]])

    scope_flag <- if (is_design_domain) {
      "design_domain"
    } else {
      "analysis_domain_requires_precision"
    }

    severity <- if (is_design_domain) {
      "info"
    } else if (isTRUE(strict)) {
      "error"
    } else {
      "warning"
    }

    message <- .enemdu_domain_scope_message(
      survey_type = survey_type,
      domain_level = level_i,
      domain_var = var_i,
      is_design_domain = is_design_domain,
      requires_precision = requires_precision,
      description = row$description[[1]]
    )

    pieces[[i]] <- tibble::tibble(
      survey_type = survey_type,
      domain_var = var_i,
      domain_level = level_i,
      is_design_domain = is_design_domain,
      requires_precision_evaluation = requires_precision,
      scope_flag = scope_flag,
      severity = severity,
      message = message
    )
  }

  out <- do.call(rbind, pieces)
  row.names(out) <- NULL
  out <- tibble::as_tibble(out)

  if (isTRUE(strict) && any(!out$is_design_domain)) {
    rlang::abort(
      message = paste(out$message[!out$is_design_domain], collapse = "\n"),
      class = c("enemdu_error_domain_out_of_scope", "enemdu_error")
    )
  }

  if (isTRUE(emit)) {
    for (msg in out$message) {
      rlang::inform(
        message = msg,
        class = c("enemdu_message_domain_scope", "enemdu_message")
      )
    }
  }

  class(out) <- unique(c("enemdu_domain_scope_report", class(out)))
  out
}

.enemdu_build_domain_requests <- function(domain_level,
                                          domain_var,
                                          group_vars,
                                          domain_variable_registry) {
  if (!is.null(domain_level)) {
    domain_level <- as.character(domain_level)

    if (length(domain_level) == 1) {
      domain_var_out <- domain_var %||% group_vars %||% NA_character_

      return(tibble::tibble(
        domain_var = as.character(domain_var_out)[seq_along(domain_level)],
        domain_level = domain_level
      ))
    }

    domain_var_out <- domain_var %||% group_vars

    if (is.null(domain_var_out)) {
      domain_var_out <- rep(NA_character_, length(domain_level))
    }

    if (length(domain_var_out) != length(domain_level)) {
      rlang::abort(
        message = "`domain_var` and `domain_level` must have compatible lengths.",
        class = c("enemdu_error_domain_request_length", "enemdu_error")
      )
    }

    return(tibble::tibble(
      domain_var = as.character(domain_var_out),
      domain_level = domain_level
    ))
  }

  if (!is.null(group_vars) && length(group_vars) > 0) {
    inferred <- vapply(
      group_vars,
      function(v) {
        .enemdu_infer_domain_level(
          domain_var = v,
          domain_variable_registry = domain_variable_registry
        )
      },
      character(1)
    )

    return(tibble::tibble(
      domain_var = as.character(group_vars),
      domain_level = inferred
    ))
  }

  if (!is.null(domain_var)) {
    inferred <- vapply(
      domain_var,
      function(v) {
        .enemdu_infer_domain_level(
          domain_var = v,
          domain_variable_registry = domain_variable_registry
        )
      },
      character(1)
    )

    return(tibble::tibble(
      domain_var = as.character(domain_var),
      domain_level = inferred
    ))
  }

  tibble::tibble(
    domain_var = NA_character_,
    domain_level = "nacional"
  )
}

.enemdu_infer_domain_level <- function(domain_var,
                                       domain_variable_registry) {
  domain_var <- as.character(domain_var)

  row <- domain_variable_registry[
    domain_variable_registry$variable == domain_var,
    ,
    drop = FALSE
  ]

  if (nrow(row) >= 1) {
    return(as.character(row$domain_level[[1]]))
  }

  "subpoblacion_sociodemografica"
}

.enemdu_domain_scope_message <- function(survey_type,
                                         domain_level,
                                         domain_var,
                                         is_design_domain,
                                         requires_precision,
                                         description) {
  var_label <- if (is.na(domain_var) || !nzchar(domain_var)) {
    "sin variable de dominio explícita"
  } else {
    glue::glue("variable `{domain_var}`")
  }

  if (isTRUE(is_design_domain)) {
    return(glue::glue(
      "Para ENEMDU `{survey_type}`, el dominio `{domain_level}` ({var_label}) está dentro del alcance de diseño. {description}"
    ))
  }

  glue::glue(
    "Para ENEMDU `{survey_type}`, el dominio `{domain_level}` ({var_label}) no está dentro del alcance de diseño. ",
    "Debe tratarse como dominio de análisis y reportarse únicamente con evaluación de precisión. {description}"
  )
}

.enemdu_validate_domain_registry <- function(registry) {
  required_cols <- c(
    "survey_type",
    "domain_level",
    "is_design_domain",
    "requires_precision_evaluation",
    "description"
  )

  missing_cols <- setdiff(required_cols, names(registry))

  if (length(missing_cols) > 0) {
    .enemdu_abort_invalid_registry(
      registry_name = "domain_registry",
      message = glue::glue("Missing columns: {paste(missing_cols, collapse = ', ')}."),
      caller = ".enemdu_validate_domain_registry"
    )
  }

  invisible(TRUE)
}

.enemdu_validate_domain_variable_registry <- function(registry) {
  required_cols <- c(
    "variable",
    "domain_level",
    "source_status",
    "notes"
  )

  missing_cols <- setdiff(required_cols, names(registry))

  if (length(missing_cols) > 0) {
    .enemdu_abort_invalid_registry(
      registry_name = "domain_variable_registry",
      message = glue::glue("Missing columns: {paste(missing_cols, collapse = ', ')}."),
      caller = ".enemdu_validate_domain_variable_registry"
    )
  }

  invisible(TRUE)
}
