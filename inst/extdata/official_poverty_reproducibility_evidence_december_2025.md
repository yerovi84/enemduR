# December 2025 Income Poverty Reproducibility Evidence

## Purpose

This document records reproducibility evidence for the December 2025 income poverty workflow implemented in `enemduR`.

The purpose is to document that the local reproducibility workflow was executed with official ENEMDU December 2025 person-level microdata and compared against published official poverty tabulations.

This document does not include microdata, does not include confidential data, and does not constitute an official validation claim by INEC.

## Scope

This evidence covers six December 2025 income-poverty benchmark comparisons:

- income poverty, national;
- income poverty, urban;
- income poverty, rural;
- extreme income poverty, national;
- extreme income poverty, urban;
- extreme income poverty, rural.

The comparison was performed using:

- the `enemdu_run_poverty_reproducibility()` workflow;
- the official December 2025 poverty and extreme poverty lines supplied explicitly;
- the package benchmark registry;
- official CSV poverty tabulations supplied separately by the user;
- local ENEMDU December 2025 microdata.

## Local Data Used

The local microdata file used for the execution was:

- `enemdu_persona_2025_12.sav`

This file was used locally in RStudio and was not committed to the repository.

The following variables were detected in the official ENEMDU December 2025 file and used by the workflow:

- `ingpc`: household per-capita income;
- `area`: urban/rural domain variable;
- `fexp`: expansion factor;
- `upm`: primary sampling unit;
- `estrato`: stratum.

The local preflight check confirmed that all required variables were present.

## Preflight Summary

The preflight check was executed with:

- `income_var = "ingpc"`;
- `area_var = "area"`;
- `weight_var = "fexp"`;
- `psu_var = "upm"`;
- `strata_var = "estrato"`.

The preflight result confirmed:

- `ingpc`: present, numeric, 133 missing values and 27,675 non-missing values;
- `area`: present, no missing values;
- `fexp`: present, no missing values;
- `upm`: present, no missing values;
- `estrato`: present, no missing values.

The preflight passed for the required reproducibility workflow inputs.

## Poverty-Line Policy

The December 2025 poverty lines were supplied explicitly to the workflow.

The values used were:

- poverty line: USD 92.40;
- extreme poverty line: USD 52.07.

These values were not added to the package operational poverty-line registry.

The workflow did not derive poverty lines from CPI.

## Official CSV Tabulations Used

The following official CSV tabulations were parsed locally:

- `1.1.1.pobre_nacional.csv`;
- `1.1.2.pobre_urbana.csv`;
- `1.1.3.pobre_rural.csv`;
- `1.2.1.extpob_nacional.csv`;
- `1.2.2.extpob_urbana.csv`;
- `1.2.3.extpob_rural.csv`.

The parsed December 2025 official values were:

| indicator_id | domain_type | domain_value | official_percent_from_csv |
|---|---|---:|---:|
| pobreza_ingresos | national | national | 21.4 |
| pobreza_ingresos | area | urban | 13.8 |
| pobreza_ingresos | area | rural | 37.6 |
| pobreza_extrema_ingresos | national | national | 8.3 |
| pobreza_extrema_ingresos | area | urban | 3.0 |
| pobreza_extrema_ingresos | area | rural | 19.7 |

## Benchmark Registry Versus Official CSV Tabulations

The benchmark registry values included in `enemduR` were compared against the parsed official CSV values.

All six benchmark rows matched the official CSV values exactly.

| indicator_id | domain_type | domain_value | package_benchmark_percent | official_percent_from_csv | status |
|---|---|---:|---:|---:|---|
| pobreza_ingresos | national | national | 21.4 | 21.4 | exact_match |
| pobreza_ingresos | area | urban | 13.8 | 13.8 | exact_match |
| pobreza_ingresos | area | rural | 37.6 | 37.6 | exact_match |
| pobreza_extrema_ingresos | national | national | 8.3 | 8.3 | exact_match |
| pobreza_extrema_ingresos | area | urban | 3.0 | 3.0 | exact_match |
| pobreza_extrema_ingresos | area | rural | 19.7 | 19.7 | exact_match |

## Reproducibility Results

The local `enemduR` workflow produced estimates that matched all six published benchmarks within reported rounding.

| indicator_id | domain_type | domain_value | estimate_percent | official_percent | difference_pp | abs_difference_pp | comparison_status |
|---|---|---:|---:|---:|---:|---:|---|
| pobreza_ingresos | national | national | 21.4 | 21.4 | -0.0290 | 0.0290 | matched_reported_rounding |
| pobreza_extrema_ingresos | national | national | 8.33 | 8.3 | 0.0291 | 0.0291 | matched_reported_rounding |
| pobreza_ingresos | area | urban | 13.8 | 13.8 | 0.0112 | 0.0112 | matched_reported_rounding |
| pobreza_ingresos | area | rural | 37.6 | 37.6 | -0.0164 | 0.0164 | matched_reported_rounding |
| pobreza_extrema_ingresos | area | urban | 3.01 | 3.0 | 0.0147 | 0.0147 | matched_reported_rounding |
| pobreza_extrema_ingresos | area | rural | 19.7 | 19.7 | 0.0263 | 0.0263 | matched_reported_rounding |

The maximum absolute difference observed was:

- 0.0291 percentage points.

## Reproducibility Status

The comparison returned:

- `matched_reported_rounding` for 6 of 6 benchmark rows;
- `benchmark_comparison_within_tolerance` for 6 of 6 benchmark rows;
- `not_officially_validated` as the official validation status.

This means that the local workflow reproduced the published December 2025 ENEMDU income-poverty benchmarks within reported rounding.

## Official Validation Disclaimer

This document records a local reproducibility check against published official benchmarks.

It is not an official validation claim by INEC.

The package should continue to report this evidence as a reproducibility check unless and until an institutional validation process explicitly grants official validation status.

## Methodological Notes

The workflow used the official ENEMDU December 2025 person-level file locally.

The workflow used `ingpc` as the income variable.

The workflow used `area` for national, urban, and rural domain comparisons.

The workflow used `fexp`, `upm`, and `estrato` for survey-design-aware estimation.

The workflow supplied poverty lines explicitly.

The workflow did not derive poverty lines from CPI.

The workflow did not modify package registries during execution.

The workflow did not commit microdata or local output files.

## Evidence Classification

Evidence level:

- local real-data execution;
- benchmark comparison against package registry;
- benchmark comparison against official CSV tabulations;
- no official institutional validation claim.

Evidence status:

- reproducibility evidence: confirmed;
- registry versus CSV consistency: confirmed;
- official validation: not claimed.

## Recommended Next Step

The next recommended step is to decide whether this evidence should remain as a static methodological record or whether the CSV-tabulation parser should be formalized into a package helper or local script.

If formalized, the implementation should preserve the same constraints:

- do not commit official microdata;
- do not commit raw ZIP tabulations unless explicitly approved;
- do not add official validation claims;
- keep the workflow reproducible and auditable;
- maintain separation between benchmark extraction, estimation, and interpretation.
