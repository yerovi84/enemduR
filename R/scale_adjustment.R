#' Build household size from household identifier
#'
#' Builds `hsize` by counting how many records share the same household
#' identifier. This is required for household income construction and for the
#' internal household-scale adjustment used in representativity diagnostics when
#' household-level values are repeated across household members.
#'
#' @param data A data frame.
#' @param household_id Household identifier variable. Defaults to `"idhogar"`.
#' If `"idhogar"` is not present but `"id_hogar"` is present, the function uses
#' `"id_hogar"` automatically.
#' @param hsize_name Name of the output household-size variable. Defaults to
#' `"hsize"`.
#' @param overwrite Logical. If `TRUE`, overwrites an existing `hsize_name`
#' variable.
#'
#' @return A data frame with the household-size variable added.
#' @export
enemdu_build_hsize <- function(data,
                               household_id = "idhogar",
                               hsize_name = "hsize",
                               overwrite = FALSE) {
  if (!is.data.frame(data)) {
    .enemdu_abort_invalid_data(caller = "enemdu_build_hsize")
  }

  resolved_household_id <- .enemdu_resolve_household_id(
    data = data,
    household_id = household_id,
    caller = "enemdu_build_hsize"
  )

  if (hsize_name %in% names(data) && !isTRUE(overwrite)) {
    rlang::abort(
      message = glue::glue(
        "Variable `{hsize_name}` already exists. ",
        "Use `overwrite = TRUE` to replace it."
      ),
      class = c("enemdu_error_existing_hsize", "enemdu_error")
    )
  }

  household_values <- data[[resolved_household_id]]
  hsize <- .enemdu_group_count(household_values)

  data[[hsize_name]] <- hsize

  attr(data, "hsize_variable") <- hsize_name
  attr(data, "household_id_variable") <- resolved_household_id

  class(data) <- unique(c("enemdu_tbl", class(data)))
  data
}

#' Apply internal household scale adjustment
#'
#' Divides selected household-level variables by household size when those
#' variables are repeated across household members and are going to be used in
#' representativity or precision assessment workflows.
#'
#' This adjustment does not replace the official indicator definition. It creates
#' adjusted analytical contribution variables for representativity diagnostics.
#'
#' @param data A data frame.
#' @param vars Character vector of numeric variables to adjust.
#' @param hsize Household-size variable. Defaults to `"hsize"`.
#' @param suffix Suffix for adjusted variables. Defaults to `"_hscale"`.
#' @param overwrite Logical. If `TRUE`, overwrites existing adjusted variables.
#'
#' @return A data frame with adjusted variables added.
#' @export
enemdu_apply_household_scale_adjustment <- function(data,
                                                    vars,
                                                    hsize = "hsize",
                                                    suffix = "_hscale",
                                                    overwrite = FALSE) {
  if (!is.data.frame(data)) {
    .enemdu_abort_invalid_data(caller = "enemdu_apply_household_scale_adjustment")
  }

  if (missing(vars) || is.null(vars) || length(vars) == 0) {
    .enemdu_abort_missing_argument(
      "vars",
      caller = "enemdu_apply_household_scale_adjustment"
    )
  }

  .enemdu_abort_missing_vars(
    vars = c(vars, hsize),
    names_data = names(data),
    caller = "enemdu_apply_household_scale_adjustment"
  )

  if (!is.numeric(data[[hsize]]) || any(data[[hsize]] <= 0, na.rm = TRUE)) {
    .enemdu_abort_invalid_hsize(
      hsize = hsize,
      caller = "enemdu_apply_household_scale_adjustment"
    )
  }

  adjusted_vars <- character(length(vars))

  for (i in seq_along(vars)) {
    var <- vars[[i]]

    if (!is.numeric(data[[var]])) {
      .enemdu_abort_invalid_numeric_var(
        var = var,
        caller = "enemdu_apply_household_scale_adjustment"
      )
    }

    adjusted_name <- paste0(var, suffix)
    adjusted_vars[[i]] <- adjusted_name

    if (adjusted_name %in% names(data) && !isTRUE(overwrite)) {
      rlang::abort(
        message = glue::glue(
          "Adjusted variable `{adjusted_name}` already exists. ",
          "Use `overwrite = TRUE` to replace it."
        ),
        class = c("enemdu_error_existing_adjusted_var", "enemdu_error")
      )
    }

    data[[adjusted_name]] <- data[[var]] / data[[hsize]]
  }

  attr(data, "household_scale_adjustment") <- list(
    original_vars = vars,
    adjusted_vars = adjusted_vars,
    hsize = hsize,
    suffix = suffix,
    note = paste(
      "Internal household scale adjustment applied for representativity diagnostics.",
      "Original variables were not replaced."
    )
  )

  class(data) <- unique(c("enemdu_tbl", class(data)))
  data
}

.enemdu_resolve_household_id <- function(data,
                                         household_id = NULL,
                                         caller = "enemdu_internal") {
  if (!is.data.frame(data)) {
    .enemdu_abort_invalid_data(caller = caller)
  }

  if (!is.null(household_id) && household_id %in% names(data)) {
    return(household_id)
  }

  candidates <- c("idhogar", "id_hogar")
  available <- intersect(candidates, names(data))

  if (length(available) >= 1) {
    return(available[[1]])
  }

  requested <- if (is.null(household_id)) {
    paste(candidates, collapse = " or ")
  } else {
    paste(unique(c(household_id, candidates)), collapse = " or ")
  }

  rlang::abort(
    message = glue::glue(
      "No household identifier was found in `{caller}()`. ",
      "Expected one of: {requested}."
    ),
    class = c("enemdu_error_missing_household_id", "enemdu_error")
  )
}

.enemdu_group_count <- function(group) {
  out <- rep(NA_integer_, length(group))
  valid <- !is.na(group)

  if (!any(valid)) {
    return(out)
  }

  out[valid] <- as.integer(
    stats::ave(
      rep(1L, sum(valid)),
      group[valid],
      FUN = length
    )
  )

  out
}

.enemdu_group_sum <- function(x, group, all_missing_value = 0) {
  out <- rep(NA_real_, length(x))
  valid_group <- !is.na(group)

  if (!any(valid_group)) {
    return(out)
  }

  x_valid <- x[valid_group]
  group_valid <- group[valid_group]

  x_zero <- ifelse(is.na(x_valid), 0, x_valid)
  sum_values <- stats::ave(x_zero, group_valid, FUN = sum)

  all_missing <- stats::ave(
    as.integer(is.na(x_valid)),
    group_valid,
    FUN = function(z) all(z == 1L)
  )

  sum_values[as.logical(all_missing)] <- all_missing_value

  out[valid_group] <- as.numeric(sum_values)
  out
}
