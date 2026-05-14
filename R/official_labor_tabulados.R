#' Read official INEC labor tabulations
#'
#' Reads official INEC labor-market tabulations distributed as CSV files or ZIP
#' archives and returns a tidy long-format table. This helper is intended for
#' validation and audit workflows against `enemdu_kpi_employment()` outputs.
#'
#' @param path Path to an official CSV file, a directory containing official CSV
#' files, or a ZIP archive containing official CSV files.
#' @param survey_type ENEMDU survey type. One of `"mensual"`, `"trimestral"` or
#' `"anual"`.
#' @param include_unmapped Logical. If `TRUE`, keeps official rows that do not
#' yet map to an implemented `enemduR` labor indicator.
#' @param encoding File encoding used by official CSV files. Defaults to
#' `"Latin1"`.
#'
#' @return A tibble in long format with official values and mapped indicator ids
#' when available.
#' @export
enemdu_read_official_labor_tabulados <- function(path,
                                                 survey_type = c(
                                                   "mensual",
                                                   "trimestral",
                                                   "anual"
                                                 ),
                                                 include_unmapped = TRUE,
                                                 encoding = "Latin1") {
  if (missing(path) || is.null(path) || length(path) != 1 || !nzchar(path)) {
    .enemdu_abort_missing_argument(
      "path",
      caller = "enemdu_read_official_labor_tabulados"
    )
  }

  survey_type <- .enemdu_normalize_survey_type(
    survey_type = match.arg(survey_type),
    caller = "enemdu_read_official_labor_tabulados"
  )

  if (!file.exists(path) && !dir.exists(path)) {
    rlang::abort(
      message = glue::glue(
        "Path `{path}` does not exist in `enemdu_read_official_labor_tabulados()`."
      ),
      class = c("enemdu_error_missing_official_tabulado_path", "enemdu_error")
    )
  }

  resolved <- .enemdu_resolve_official_labor_tabulado_files(
    path = path,
    survey_type = survey_type
  )

  files <- resolved$files
  cleanup_path <- resolved$cleanup_path

  if (!is.null(cleanup_path)) {
    on.exit(unlink(cleanup_path, recursive = TRUE, force = TRUE), add = TRUE)
  }

  pieces <- vector("list", nrow(files))

  for (i in seq_len(nrow(files))) {
    pieces[[i]] <- .enemdu_parse_official_labor_tabulado_file(
      file = files$source_path[[i]],
      source_file = files$source_file[[i]],
      source_section = files$source_section[[i]],
      survey_type = survey_type,
      encoding = encoding
    )
  }

  out <- do.call(rbind, pieces)
  row.names(out) <- NULL
  out <- tibble::as_tibble(out)

  if (!isTRUE(include_unmapped)) {
    out <- out[!is.na(out$indicator_id) & nzchar(out$indicator_id), , drop = FALSE]
    out <- tibble::as_tibble(out)
  }

  class(out) <- unique(c("enemdu_official_labor_tabulados", class(out)))

  attr(out, "official_labor_tabulados_policy") <- list(
    path = path,
    survey_type = survey_type,
    include_unmapped = include_unmapped,
    encoding = encoding,
    note = paste(
      "Official INEC labor tabulations were parsed into long format.",
      "Published rates are converted to package scale as proportions in [0, 1].",
      "Counts are kept in count scale."
    )
  )

  out
}

#' Compare labor estimates with official INEC labor tabulations
#'
#' Compares `enemdu_kpi_employment()` outputs against official INEC labor
#' tabulations previously read with `enemdu_read_official_labor_tabulados()`.
#'
#' This function is intentionally conservative. It compares only official rows
#' whose estimator is `Indicador`, and only rows that can be mapped to an
#' implemented `indicator_id`.
#'
#' @param estimates A data frame returned by `enemdu_kpi_employment()` or another
#' object with columns `indicator_id` and `estimate`.
#' @param official A data frame returned by
#' `enemdu_read_official_labor_tabulados()`.
#' @param official_period Optional official period to filter, such as `"mar-26"`,
#' `"I - 2026"` or `"2025"`.
#' @param domain_group Official domain group. Defaults to `"Nacional"`.
#' @param domain_label Official domain label. Defaults to `"Total"`.
#' @param tolerance_count Absolute tolerance for counts. Defaults to `1`.
#' @param tolerance_rate Absolute tolerance for rates in package scale. Defaults
#' to `0.0005`, equivalent to 0.05 percentage points.
#' @param strict Logical. If `TRUE`, errors when at least one comparable
#' indicator falls outside tolerance or is missing in package estimates.
#'
#' @return A tibble with official values, package estimates, differences and
#' comparison status.
#' @export
enemdu_compare_labor_tabulados <- function(estimates,
                                           official,
                                           official_period = NULL,
                                           domain_group = "Nacional",
                                           domain_label = "Total",
                                           tolerance_count = 1,
                                           tolerance_rate = 0.0005,
                                           strict = FALSE) {
  if (!is.data.frame(estimates)) {
    .enemdu_abort_invalid_data(caller = "enemdu_compare_labor_tabulados")
  }

  if (!is.data.frame(official)) {
    rlang::abort(
      message = "`official` must be a data frame in `enemdu_compare_labor_tabulados()`.",
      class = c("enemdu_error_invalid_official_tabulado", "enemdu_error")
    )
  }

  .enemdu_abort_missing_vars(
    vars = c("indicator_id", "estimate"),
    names_data = names(estimates),
    caller = "enemdu_compare_labor_tabulados"
  )

  .enemdu_abort_missing_vars(
    vars = c(
      "indicator_id",
      "period",
      "domain_group",
      "domain_label",
      "official_measure",
      "official_value_package_scale",
      "package_scale"
    ),
    names_data = names(official),
    caller = "enemdu_compare_labor_tabulados"
  )

  official_work <- official

  if (!is.null(official_period)) {
    official_work <- official_work[
      .enemdu_official_period_key(official_work$period) %in%
        .enemdu_official_period_key(as.character(official_period)),
      ,
      drop = FALSE
    ]
  }

  if (!is.null(domain_group)) {
    official_work <- official_work[
      .enemdu_label_key(official_work$domain_group) == .enemdu_label_key(domain_group),
      ,
      drop = FALSE
    ]
  }

  if (!is.null(domain_label)) {
    official_work <- official_work[
      .enemdu_label_key(official_work$domain_label) == .enemdu_label_key(domain_label),
      ,
      drop = FALSE
    ]
  }

  official_work <- official_work[
    !is.na(official_work$indicator_id) &
      nzchar(official_work$indicator_id) &
      .enemdu_label_key(official_work$official_measure) == "indicador",
    ,
    drop = FALSE
  ]

  if (nrow(official_work) == 0) {
    rlang::abort(
      message = paste(
        "No comparable official labor tabulation rows were found.",
        "Check `official_period`, `domain_group`, `domain_label`, and indicator mapping."
      ),
      class = c("enemdu_error_no_comparable_official_labor_rows", "enemdu_error")
    )
  }

  duplicated_official <- unique(
    official_work$indicator_id[duplicated(official_work$indicator_id)]
  )

  if (length(duplicated_official) > 0) {
    rlang::abort(
      message = glue::glue(
        "Official tabulations contain duplicated comparable rows for indicator ids: ",
        "{paste(duplicated_official, collapse = ', ')}. ",
        "Filter by a single period and domain before comparing."
      ),
      class = c("enemdu_error_duplicated_official_labor_rows", "enemdu_error")
    )
  }

  estimate_work <- estimates[, c("indicator_id", "estimate"), drop = FALSE]

  duplicated_estimates <- unique(
    estimate_work$indicator_id[duplicated(estimate_work$indicator_id)]
  )

  if (length(duplicated_estimates) > 0) {
    rlang::abort(
      message = glue::glue(
        "Package estimates contain duplicated indicator ids: ",
        "{paste(duplicated_estimates, collapse = ', ')}. ",
        "Pass a single-domain estimate table before comparing."
      ),
      class = c("enemdu_error_duplicated_package_labor_rows", "enemdu_error")
    )
  }

  comparison <- merge(
    official_work,
    estimate_work,
    by = "indicator_id",
    all.x = TRUE,
    all.y = FALSE,
    suffixes = c("_official", "_package")
  )

  comparison[["package_estimate"]] <- comparison[["estimate"]]
  comparison[["estimate"]] <- NULL

  comparison[["absolute_difference"]] <- abs(
    comparison[["package_estimate"]] -
      comparison[["official_value_package_scale"]]
  )

  comparison[["tolerance"]] <- ifelse(
    comparison[["package_scale"]] == "count",
    tolerance_count,
    tolerance_rate
  )

  comparison[["within_tolerance"]] <- comparison[["absolute_difference"]] <=
    comparison[["tolerance"]] + .Machine$double.eps

  comparison[["comparison_status"]] <- ifelse(
    is.na(comparison[["package_estimate"]]),
    "missing_package_estimate",
    ifelse(
      is.na(comparison[["official_value_package_scale"]]),
      "official_value_not_numeric",
      ifelse(
        comparison[["within_tolerance"]],
        "match",
        "outside_tolerance"
      )
    )
  )

  front_cols <- c(
    "indicator_id",
    "official_indicator_label",
    "period",
    "domain_group",
    "domain_label",
    "official_measure",
    "official_value_raw",
    "official_value",
    "official_value_package_scale",
    "package_estimate",
    "absolute_difference",
    "tolerance",
    "within_tolerance",
    "comparison_status"
  )

  comparison <- comparison[c(front_cols, setdiff(names(comparison), front_cols))]
  comparison <- tibble::as_tibble(comparison)

  bad <- comparison$comparison_status %in% c(
    "missing_package_estimate",
    "outside_tolerance"
  )

  if (isTRUE(strict) && any(bad, na.rm = TRUE)) {
    rlang::abort(
      message = glue::glue(
        "Labor comparison found {sum(bad, na.rm = TRUE)} mismatch(es) or missing package estimate(s)."
      ),
      class = c("enemdu_error_labor_tabulados_mismatch", "enemdu_error"),
      comparison = comparison
    )
  }

  class(comparison) <- unique(c("enemdu_labor_tabulados_comparison", class(comparison)))

  attr(comparison, "labor_tabulados_comparison_policy") <- list(
    official_period = official_period,
    domain_group = domain_group,
    domain_label = domain_label,
    tolerance_count = tolerance_count,
    tolerance_rate = tolerance_rate,
    strict = strict,
    note = paste(
      "Counts are compared in count scale.",
      "Rates are compared in package scale as proportions in [0, 1]."
    )
  )

  comparison
}

.enemdu_resolve_official_labor_tabulado_files <- function(path,
                                                          survey_type) {
  cleanup_path <- NULL

  if (dir.exists(path)) {
    base_path <- normalizePath(path, winslash = "/", mustWork = TRUE)
    all_files <- list.files(
      base_path,
      recursive = TRUE,
      full.names = TRUE,
      include.dirs = FALSE
    )
    source_file <- substring(
      normalizePath(all_files, winslash = "/", mustWork = TRUE),
      nchar(base_path) + 2L
    )
  } else if (grepl("\\.zip$", path, ignore.case = TRUE)) {
    cleanup_path <- tempfile("enemdu_official_labor_tabulados_")
    dir.create(cleanup_path, recursive = TRUE, showWarnings = FALSE)
    utils::unzip(path, exdir = cleanup_path)

    base_path <- normalizePath(cleanup_path, winslash = "/", mustWork = TRUE)
    all_files <- list.files(
      base_path,
      recursive = TRUE,
      full.names = TRUE,
      include.dirs = FALSE
    )
    source_file <- substring(
      normalizePath(all_files, winslash = "/", mustWork = TRUE),
      nchar(base_path) + 2L
    )
  } else {
    all_files <- normalizePath(path, winslash = "/", mustWork = TRUE)
    source_file <- basename(all_files)
  }

  normalized_source <- gsub("\\\\", "/", source_file)
  selected <- logical(length(all_files))
  source_section <- rep(NA_character_, length(all_files))

  if (survey_type %in% c("mensual", "trimestral")) {
    is_poblaciones <- basename(normalized_source) == "1. Poblaciones.csv"
    is_tasas <- basename(normalized_source) == "2. Tasas.csv"

    selected <- is_poblaciones | is_tasas
    source_section[is_poblaciones] <- "poblaciones"
    source_section[is_tasas] <- "tasas"
  }

  if (identical(survey_type, "anual")) {
    is_estimadores <- grepl(
      "Mercado_Laboral",
      normalized_source,
      fixed = TRUE
    ) &
      grepl(
        "1\\. Estimadores\\.csv$",
        normalized_source
      )

    selected <- is_estimadores
    source_section[is_estimadores] <- "estimadores"
  }

  if (!any(selected)) {
    rlang::abort(
      message = glue::glue(
        "No official labor tabulation CSV files were detected for survey type `{survey_type}`."
      ),
      class = c("enemdu_error_no_official_labor_tabulados", "enemdu_error")
    )
  }

  files <- tibble::tibble(
    source_path = all_files[selected],
    source_file = normalized_source[selected],
    source_section = source_section[selected]
  )

  list(files = files, cleanup_path = cleanup_path)
}

.enemdu_parse_official_labor_tabulado_file <- function(file,
                                                       source_file,
                                                       source_section,
                                                       survey_type,
                                                       encoding) {
  raw <- .enemdu_read_official_semicolon_csv(
    file = file,
    encoding = encoding
  )

  if (source_section %in% c("poblaciones", "tasas")) {
    header_group_row <- 2L
    header_label_row <- 3L
    data_start_row <- 4L

    if (identical(survey_type, "mensual")) {
      id_cols <- 3L
      period_col <- 2L
      indicator_col <- 3L
    } else {
      id_cols <- 2L
      period_col <- 1L
      indicator_col <- 2L
    }
  } else if (identical(source_section, "estimadores")) {
    header_group_row <- 3L
    header_label_row <- 4L
    data_start_row <- 5L
    id_cols <- 3L
    period_col <- 1L
    indicator_col <- 2L
  } else {
    rlang::abort(
      message = glue::glue("Unsupported official labor source section `{source_section}`."),
      class = c("enemdu_error_unsupported_official_labor_section", "enemdu_error")
    )
  }

  if (nrow(raw) < data_start_row || ncol(raw) <= id_cols) {
    return(.enemdu_empty_official_labor_tabulados())
  }

  header_group <- .enemdu_df_row(raw, header_group_row)
  header_label <- .enemdu_df_row(raw, header_label_row)
  header_group <- .enemdu_fill_right(header_group)

  rows <- list()
  k <- 1L

  for (i in seq.int(data_start_row, nrow(raw))) {
    current <- .enemdu_df_row(raw, i)

    if (all(!nzchar(current[seq_len(min(id_cols, length(current)))]))) {
      next
    }

    period <- current[[period_col]]
    official_indicator_label <- current[[indicator_col]]

    if (!nzchar(official_indicator_label)) {
      next
    }

    official_measure <- if (identical(source_section, "estimadores")) {
      current[[3L]]
    } else {
      "Indicador"
    }

    for (j in seq.int(id_cols + 1L, ncol(raw))) {
      domain_group <- header_group[[j]]
      domain_label <- header_label[[j]]

      if (!nzchar(domain_group) && !nzchar(domain_label)) {
        next
      }

      if (!nzchar(domain_label)) {
        domain_label <- domain_group
      }

      official_value_raw <- current[[j]]

      if (!nzchar(official_value_raw)) {
        next
      }

      official_value <- .enemdu_parse_official_number(official_value_raw)
      official_unit <- .enemdu_official_labor_unit(
        source_section = source_section,
        official_measure = official_measure
      )

      package_scale <- .enemdu_official_labor_package_scale(official_unit)

      official_value_package_scale <- if (
        identical(package_scale, "proportion_0_1") &&
        !is.na(official_value)
      ) {
        official_value / 100
      } else {
        official_value
      }

      indicator_id <- .enemdu_official_labor_indicator_id(
        official_indicator_label = official_indicator_label,
        source_section = source_section
      )

      rows[[k]] <- tibble::tibble(
        survey_type = survey_type,
        source_file = source_file,
        source_section = source_section,
        period = period,
        official_period_key = .enemdu_official_period_key(period),
        official_indicator_label = official_indicator_label,
        official_measure = official_measure,
        indicator_id = indicator_id,
        domain_group = domain_group,
        domain_label = domain_label,
        domain_key = .enemdu_label_key(paste(domain_group, domain_label)),
        official_value_raw = official_value_raw,
        official_value = official_value,
        official_unit = official_unit,
        package_scale = package_scale,
        official_value_package_scale = official_value_package_scale
      )

      k <- k + 1L
    }
  }

  if (length(rows) == 0) {
    return(.enemdu_empty_official_labor_tabulados())
  }

  out <- do.call(rbind, rows)
  row.names(out) <- NULL
  tibble::as_tibble(out)
}

.enemdu_read_official_semicolon_csv <- function(file,
                                                encoding) {
  out <- readr::read_delim(
    file = file,
    delim = ";",
    col_names = FALSE,
    col_types = readr::cols(.default = readr::col_character()),
    locale = readr::locale(encoding = encoding),
    trim_ws = TRUE,
    na = character(),
    show_col_types = FALSE
  )

  out <- as.data.frame(out, stringsAsFactors = FALSE)

  out[] <- lapply(out, function(x) {
    x <- as.character(x)
    x[is.na(x)] <- ""
    trimws(x)
  })

  out
}

.enemdu_df_row <- function(x, i) {
  out <- unname(unlist(x[i, , drop = FALSE], use.names = FALSE))
  out <- as.character(out)
  out[is.na(out)] <- ""
  trimws(out)
}

.enemdu_fill_right <- function(x) {
  out <- as.character(x)
  current <- ""

  for (i in seq_along(out)) {
    if (nzchar(out[[i]])) {
      current <- out[[i]]
    } else {
      out[[i]] <- current
    }
  }

  out
}

.enemdu_parse_official_number <- function(x) {
  x <- trimws(as.character(x))

  if (length(x) != 1 || is.na(x) || !nzchar(x) || x %in% c("-", "NA", "N/A", "n/a")) {
    return(NA_real_)
  }

  x <- gsub("%", "", x, fixed = TRUE)
  x <- gsub("\\s+", "", x)
  x <- gsub("[^0-9,\\.\\-]", "", x)

  if (!nzchar(x) || !grepl("[0-9]", x)) {
    return(NA_real_)
  }

  has_comma <- grepl(",", x, fixed = TRUE)
  has_dot <- grepl(".", x, fixed = TRUE)

  if (has_comma && has_dot) {
    last_comma <- max(gregexpr(",", x, fixed = TRUE)[[1]])
    last_dot <- max(gregexpr(".", x, fixed = TRUE)[[1]])

    decimal_sep <- if (last_comma > last_dot) "," else "."
    thousands_sep <- if (identical(decimal_sep, ",")) "." else ","

    x <- gsub(thousands_sep, "", x, fixed = TRUE)
    x <- sub(decimal_sep, ".", x, fixed = TRUE)

    return(suppressWarnings(as.numeric(x)))
  }

  if (has_comma) {
    return(.enemdu_parse_single_separator_number(x, separator = ","))
  }

  if (has_dot) {
    return(.enemdu_parse_single_separator_number(x, separator = "."))
  }

  suppressWarnings(as.numeric(x))
}

.enemdu_parse_single_separator_number <- function(x,
                                                  separator) {
  parts <- strsplit(x, separator, fixed = TRUE)[[1]]

  if (length(parts) == 1) {
    return(suppressWarnings(as.numeric(x)))
  }

  if (length(parts) > 2) {
    group_lengths <- nchar(parts[-1])

    if (all(group_lengths == 3)) {
      x_clean <- paste(parts, collapse = "")
      return(suppressWarnings(as.numeric(x_clean)))
    }

    decimal_part <- parts[[length(parts)]]
    integer_part <- paste(parts[-length(parts)], collapse = "")
    x_clean <- paste0(integer_part, ".", decimal_part)

    return(suppressWarnings(as.numeric(x_clean)))
  }

  before <- parts[[1]]
  after <- parts[[2]]

  after_digits <- nchar(after)

  if (after_digits %in% c(1L, 2L)) {
    x_clean <- paste0(before, ".", after)
  } else {
    x_clean <- paste0(before, after)
  }

  suppressWarnings(as.numeric(x_clean))
}

.enemdu_official_labor_unit <- function(source_section,
                                        official_measure) {
  measure_key <- .enemdu_label_key(official_measure)

  if (identical(source_section, "poblaciones")) {
    return("count")
  }

  if (identical(measure_key, "coeficiente de variacion")) {
    return("cv_percent")
  }

  "percent"
}

.enemdu_official_labor_package_scale <- function(official_unit) {
  if (identical(official_unit, "count")) {
    return("count")
  }

  if (identical(official_unit, "percent")) {
    return("proportion_0_1")
  }

  "published"
}

.enemdu_official_labor_indicator_id <- function(official_indicator_label,
                                                source_section) {
  key <- .enemdu_label_key(official_indicator_label)

  if (identical(source_section, "poblaciones")) {
    return(switch(
      key,
      "poblacion en edad de trabajar pet" = "labor_pet_total",
      "poblacion economicamente activa" = "labor_pea_total",
      "poblacion economicamente inactiva" = "labor_pei_total",
      "empleo" = "labor_empleo_total",
      "empleo adecuado pleno" = "labor_empleo_adecuado_total",
      "subempleo" = "labor_subempleo_total",
      "subempleo por insuficiencia de tiempo de trabajo" = "labor_subempleo_tiempo_total",
      "subempleo por insuficiencia de ingresos" = "labor_subempleo_ingresos_total",
      "otro empleo no pleno" = "labor_otro_empleo_no_pleno_total",
      "empleo no remunerado" = "labor_empleo_no_remunerado_total",
      "empleo no clasificado" = "labor_empleo_no_clasificado_total",
      "desempleo" = "labor_desempleo_total",
      "desempleo abierto" = "labor_desempleo_abierto_total",
      "desempleo oculto" = "labor_desempleo_oculto_total",
      NA_character_
    ))
  }

  switch(
    key,
    "participacion bruta" = "labor_tasa_participacion_bruta",
    "tasa de participacion bruta" = "labor_tasa_participacion_bruta",
    "participacion global" = "labor_tasa_participacion_global",
    "tasa de participacion global" = "labor_tasa_participacion_global",
    "empleo bruto" = "labor_tasa_ocupacion_bruta",
    "tasa de empleo bruto" = "labor_tasa_ocupacion_bruta",
    "empleo global" = "labor_tasa_ocupacion_global",
    "tasa de empleo global" = "labor_tasa_ocupacion_global",
    "empleo adecuado pleno" = "labor_tasa_empleo_adecuado",
    "tasa de empleo adecuado" = "labor_tasa_empleo_adecuado",
    "subempleo" = "labor_tasa_subempleo",
    "tasa de subempleo" = "labor_tasa_subempleo",
    "subempleo por insuficiencia de tiempo de trabajo" = "labor_tasa_subempleo_tiempo",
    "subempleo por insuficiencia de ingresos" = "labor_tasa_subempleo_ingresos",
    "otro empleo no pleno" = "labor_tasa_otro_empleo_no_pleno",
    "tasa de otro empleo no pleno" = "labor_tasa_otro_empleo_no_pleno",
    "empleo no remunerado" = "labor_tasa_empleo_no_remunerado",
    "tasa de empleo no remunerado" = "labor_tasa_empleo_no_remunerado",
    "empleo no clasificado" = "labor_tasa_empleo_no_clasificado",
    "desempleo" = "labor_tasa_desempleo",
    "tasa de desempleo" = "labor_tasa_desempleo",
    "desempleo abierto" = "labor_tasa_desempleo_abierto",
    "desempleo oculto" = "labor_tasa_desempleo_oculto",
    NA_character_
  )
}

.enemdu_label_key <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""

  out <- suppressWarnings(iconv(x, to = "ASCII//TRANSLIT"))
  out[is.na(out)] <- x[is.na(out)]

  out <- tolower(out)
  out <- gsub("%", "", out, fixed = TRUE)
  out <- gsub("[^a-z0-9]+", " ", out)
  trimws(gsub("\\s+", " ", out))
}

.enemdu_official_period_key <- function(x) {
  .enemdu_label_key(x)
}

.enemdu_empty_official_labor_tabulados <- function() {
  tibble::tibble(
    survey_type = character(),
    source_file = character(),
    source_section = character(),
    period = character(),
    official_period_key = character(),
    official_indicator_label = character(),
    official_measure = character(),
    indicator_id = character(),
    domain_group = character(),
    domain_label = character(),
    domain_key = character(),
    official_value_raw = character(),
    official_value = numeric(),
    official_unit = character(),
    package_scale = character(),
    official_value_package_scale = numeric()
  )
}
