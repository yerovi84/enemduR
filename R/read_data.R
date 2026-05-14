#' Read ENEMDU microdata
#'
#' Reads ENEMDU microdata and returns an object prepared for the `enemduR`
#' workflow.
#'
#' The operational primary format for recent official ENEMDU microdata is
#' `.sav`. The function also supports `.dta` and `.csv` files for interoperability
#' with analytical workflows that export or transform the original source.
#'
#' The function stores the ENEMDU survey type, input format, source path, design
#' variables, and optional period metadata as object attributes. It can also emit
#' a representativity scope message according to the loaded base: monthly,
#' quarterly, or annual.
#'
#' @param path Path to a `.sav`, `.dta` or `.csv` file.
#' @param survey_type One of `"mensual"`, `"trimestral"` or `"anual"`.
#' @param period Optional period identifier used for comparability warnings.
#' Typical values are `"2020-09"`, `"2021-05"` or `"2018"`.
#' @param standardize_names Logical. If `TRUE`, variable names are normalized to
#' lower snake case.
#' @param inform_scope Logical. If `TRUE`, emits a message describing the
#' representativity scope of the loaded survey type.
#' @param encoding Optional file encoding. Passed to `haven::read_sav()` and
#' `haven::read_dta()` for SPSS/Stata files. For CSV files, it is used in
#' `readr::locale()`. If `NULL`, CSV reading uses `"UTF-8"`.
#' @param csv_delim Optional delimiter for CSV files. If `NULL`, the delimiter is
#' detected from the first non-empty lines among comma, semicolon and tab.
#' @param ... Additional arguments passed to the format-specific reader:
#' `haven::read_sav()`, `haven::read_dta()` or `readr::read_delim()`.
#'
#' @return A data frame with class `enemdu_tbl` and basic ENEMDU attributes.
#' @export
enemdu_read_data <- function(path,
                             survey_type,
                             period = NULL,
                             standardize_names = TRUE,
                             inform_scope = TRUE,
                             encoding = NULL,
                             csv_delim = NULL,
                             ...) {
  if (missing(path) || is.null(path) || length(path) != 1 || !nzchar(path)) {
    .enemdu_abort_missing_argument("path", caller = "enemdu_read_data")
  }

  if (!file.exists(path)) {
    rlang::abort(
      message = glue::glue("File does not exist: `{path}`."),
      class = c("enemdu_error_missing_file", "enemdu_error")
    )
  }

  extension <- tolower(tools::file_ext(path))
  supported_extensions <- c("sav", "dta", "csv")

  if (!extension %in% supported_extensions) {
    rlang::abort(
      message = glue::glue(
        "Unsupported file format `{extension}`. ",
        "`enemdu_read_data()` supports `.sav`, `.dta` and `.csv` files. ",
        "For official recent ENEMDU microdata, `.sav` is treated as the operational primary format."
      ),
      class = c("enemdu_error_invalid_file_format", "enemdu_error"),
      supported_extensions = supported_extensions
    )
  }

  survey_type <- .enemdu_normalize_survey_type(
    survey_type,
    caller = "enemdu_read_data"
  )

  data <- switch(
    extension,
    sav = haven::read_sav(file = path, encoding = encoding, ...),
    dta = haven::read_dta(file = path, encoding = encoding, ...),
    csv = {
      csv_delim <- csv_delim %||% .enemdu_detect_csv_delim(path)
      readr::read_delim(
        file = path,
        delim = csv_delim,
        locale = readr::locale(encoding = encoding %||% "UTF-8"),
        show_col_types = FALSE,
        progress = FALSE,
        ...
      )
    }
  )

  if (isTRUE(standardize_names)) {
    data <- enemdu_standardize_names(data)
  }

  attrs_to_set <- list(
    survey_type = survey_type,
    period = period,
    source_path = normalizePath(path, winslash = "/", mustWork = FALSE),
    input_format = extension,
    design_vars = .enemdu_default_design_vars()
  )

  for (nm in names(attrs_to_set)) {
    attr(data, nm) <- attrs_to_set[[nm]]
  }

  class(data) <- unique(c("enemdu_tbl", class(data)))

  if (isTRUE(inform_scope)) {
    scope <- enemdu_representativity_scope(
      survey_type = survey_type,
      emit = TRUE
    )
    attr(data, "representativity_scope") <- scope
  }

  notes <- .enemdu_find_comparability_notes(
    period = period,
    survey_type = survey_type
  )

  if (length(notes) > 0) {
    for (note in notes) {
      .enemdu_warn_comparability(note, caller = "enemdu_read_data")
    }
  }

  data
}

.enemdu_detect_csv_delim <- function(path) {
  lines <- readLines(path, n = 20, warn = FALSE, encoding = "UTF-8")
  lines <- lines[nzchar(trimws(lines))]

  if (length(lines) == 0) {
    return(",")
  }

  sample_text <- paste(lines, collapse = "\n")

  candidates <- c(
    "," = ",",
    ";" = ";",
    "\t" = "\t"
  )

  counts <- vapply(
    candidates,
    function(delim) {
      sum(gregexpr(delim, sample_text, fixed = TRUE)[[1]] > 0)
    },
    numeric(1)
  )

  names(counts)[which.max(counts)]
}
