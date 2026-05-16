microdata_path <- Sys.getenv("ENEMDU_2025_12_SAV")
output_dir <- Sys.getenv("ENEMDU_REPRO_OUTPUT")

if (!nzchar(microdata_path)) {
  stop(
    "Environment variable ENEMDU_2025_12_SAV must point to the December 2025 ENEMDU .sav file.",
    call. = FALSE
  )
}

if (!file.exists(microdata_path)) {
  stop(
    "The file declared in ENEMDU_2025_12_SAV does not exist.",
    call. = FALSE
  )
}

if (!grepl("\\.sav$", microdata_path, ignore.case = TRUE)) {
  stop(
    "ENEMDU_2025_12_SAV must point to a .sav file.",
    call. = FALSE
  )
}

if (!requireNamespace("haven", quietly = TRUE)) {
  stop(
    "Package 'haven' is required to read .sav files for this local workflow.",
    call. = FALSE
  )
}

suppressPackageStartupMessages(library(enemduR))

microdata <- haven::read_sav(microdata_path)

result <- enemdu_run_poverty_reproducibility(
  data = microdata
)

if (!nzchar(output_dir)) {
  output_dir <- tempdir()
}

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

output_path <- file.path(
  output_dir,
  "poverty_reproducibility_december_2025.csv"
)

utils::write.csv(
  result,
  file = output_path,
  row.names = FALSE,
  na = ""
)

message("Poverty reproducibility result written to: ", output_path)
