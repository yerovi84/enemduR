#' Estimate NBI poverty KPIs
#'
#' Builds or uses NBI flags derived from final component variables and estimates
#' NBI poverty and extreme NBI poverty with the package survey-design-aware
#' indicator estimator.
#'
#' This function does not reconstruct NBI from raw questionnaire variables and
#' does not claim official validation.
#'
#' @param data A data frame.
#' @param group_vars Optional grouping variables.
#' @param component_vars Final NBI component variables.
#' @param nbi_count_var NBI deprivation-count variable.
#' @param nbi_var NBI poverty flag.
#' @param extreme_nbi_var Extreme NBI poverty flag.
#' @param registry Indicator registry used by `enemdu_indicator_estimate()`.
#' NBI contract rows are added in memory when they are absent.
#' @param official_validation_status Official validation status label.
#' @param official_validation_note Note explaining the official validation
#' status.
#' @param build_flags Logical. If `TRUE`, build NBI flags before estimation.
#' @param overwrite Logical. If `TRUE`, overwrite existing NBI output variables
#' when `build_flags = TRUE`.
#' @param ... Additional arguments passed to `enemdu_indicator_estimate()`.
#'
#' @return A tibble with NBI KPI estimates and contract metadata.
#' @export
#'
#' @examples
#' data <- tibble::tibble(
#'   idhogar = paste0("h", 1:4),
#'   hsize = rep(1L, 4),
#'   upm = 1:4,
#'   estrato = c(1, 1, 2, 2),
#'   fexp = rep(1, 4),
#'   comp1 = c(0, 1, 1, 0),
#'   comp2 = c(0, 0, 1, 0),
#'   comp3 = c(0, 0, 0, 0),
#'   comp4 = c(0, 0, 0, 0),
#'   comp5 = c(0, 0, 0, 0)
#' )
#'
#' enemdu_kpi_nbi(data, sample_n_min = 1)
enemdu_kpi_nbi <- function(data,
                           group_vars = NULL,
                           component_vars = c("comp1", "comp2", "comp3", "comp4", "comp5"),
                           nbi_count_var = "knbi",
                           nbi_var = "nbi",
                           extreme_nbi_var = "xnbi",
                           registry = enemdu_indicator_registry(),
                           official_validation_status = "not_officially_validated",
                           official_validation_note = "NBI estimates are not officially validated unless compared against published official INEC benchmarks.",
                           build_flags = TRUE,
                           overwrite = FALSE,
                           ...) {
  if (!is.data.frame(data)) {
    .enemdu_abort_invalid_data(caller = "enemdu_kpi_nbi")
  }

  if (isTRUE(build_flags)) {
    out_data <- enemdu_build_nbi_flags(
      data = data,
      component_vars = component_vars,
      nbi_count_var = nbi_count_var,
      nbi_var = nbi_var,
      extreme_nbi_var = extreme_nbi_var,
      overwrite = overwrite
    )
  } else {
    .enemdu_abort_missing_vars(
      vars = c(nbi_count_var, nbi_var, extreme_nbi_var),
      names_data = names(data),
      caller = "enemdu_kpi_nbi"
    )
    out_data <- data
  }

  validation <- enemdu_validate_nbi_consistency(
    data = out_data,
    nbi_count_var = nbi_count_var,
    nbi_var = nbi_var,
    extreme_nbi_var = extreme_nbi_var
  )

  if (!identical(validation$validation_status[[1]], "passed")) {
    rlang::abort(
      message = "NBI flags are not consistent with the NBI deprivation count.",
      class = c("enemdu_error_invalid_nbi_consistency", "enemdu_error"),
      validation = validation
    )
  }

  registry <- .enemdu_registry_with_nbi_indicators(registry)

  outputs <- list(
    enemdu_indicator_estimate(
      data = out_data,
      indicator_id = "pobreza_nbi",
      value = nbi_var,
      group_vars = group_vars,
      registry = registry,
      ...
    ),
    enemdu_indicator_estimate(
      data = out_data,
      indicator_id = "pobreza_extrema_nbi",
      value = extreme_nbi_var,
      group_vars = group_vars,
      registry = registry,
      ...
    )
  )

  result <- .enemdu_bind_estimate_rows(outputs)
  result <- tibble::as_tibble(result)
  result[["nbi_source_status"]] <- "final_components_contract"
  result[["official_validation_status"]] <- official_validation_status
  result[["official_validation_note"]] <- official_validation_note
  result[["nbi_component_contract"]] <- paste(component_vars, collapse = "|")

  attr(result, "nbi_kpi_policy") <- list(
    component_vars = component_vars,
    nbi_count_var = nbi_count_var,
    nbi_var = nbi_var,
    extreme_nbi_var = extreme_nbi_var,
    build_flags = build_flags,
    official_validation_status = official_validation_status,
    note = paste(
      "NBI KPIs are estimated from final NBI components only.",
      "Official validation requires comparison against published INEC benchmarks."
    )
  )

  class(result) <- unique(c("enemdu_nbi_kpi", class(result)))
  result
}

.enemdu_registry_with_nbi_indicators <- function(registry) {
  .enemdu_validate_indicator_registry_for_estimation(registry)

  nbi_registry <- .enemdu_nbi_indicator_registry()
  missing_ids <- setdiff(nbi_registry$indicator_id, registry$indicator_id)

  if (length(missing_ids) == 0) {
    return(registry)
  }

  out <- rbind(
    registry,
    nbi_registry[nbi_registry$indicator_id %in% missing_ids, names(registry), drop = FALSE]
  )

  tibble::as_tibble(out)
}

.enemdu_nbi_indicator_registry <- function() {
  tibble::tibble(
    indicator_id = c("pobreza_nbi", "pobreza_extrema_nbi"),
    indicator_label = c(
      "Incidencia de pobreza por NBI",
      "Incidencia de pobreza extrema por NBI"
    ),
    indicator_group = c("nbi", "nbi"),
    unit = c("percentage", "percentage"),
    analysis_level = c("household_repeated_person", "household_repeated_person"),
    estimator_type = c("proportion_0_1", "proportion_0_1"),
    universe = c("personas_en_base_enemdu", "personas_en_base_enemdu"),
    weight = c("fexp", "fexp"),
    required_vars = c("nbi", "xnbi"),
    derived_vars = c("nbi", "xnbi"),
    scale_adjustment_required = c(TRUE, TRUE),
    representativity_required = c(TRUE, TRUE),
    implementation_status = c("contract", "contract"),
    method_note = c(
      "Incidencia de personas en hogares con al menos una necesidad basica insatisfecha.",
      "Incidencia de personas en hogares con dos o mas necesidades basicas insatisfechas."
    )
  )
}
