#' Standardize ENEMDU variable names
#'
#' Applies a stable lower snake-case convention to the names of a data frame.
#'
#' @param data A data frame.
#'
#' @return A data frame with standardized names.
#' @export
enemdu_standardize_names <- function(data) {
  if (!is.data.frame(data)) {
    rlang::abort(
      message = "`data` must be a data frame in `enemdu_standardize_names()`.",
      class = c("enemdu_error_invalid_data", "enemdu_error")
    )
  }

  old_attrs <- attributes(data)
  out <- janitor::clean_names(data)

  custom_attr_names <- setdiff(names(old_attrs), c("names", "row.names", "class"))
  for (nm in custom_attr_names) {
    attr(out, nm) <- old_attrs[[nm]]
  }

  old_class <- old_attrs$class %||% class(data)
  class(out) <- unique(c(setdiff(old_class, c("tbl_df", "tbl", "data.frame")), class(out)))

  out
}
