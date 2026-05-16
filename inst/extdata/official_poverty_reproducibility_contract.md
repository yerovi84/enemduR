# Official Poverty Reproducibility Contract

## Purpose

This document defines the local reproducibility scaffold for December 2025
income poverty and extreme income poverty indicators in `enemduR`.

The workflow is designed to help analysts run a documented comparison between
package estimates and published official benchmarks. It does not constitute
official validation by INEC.

## Scope

The workflow covers:

- national income poverty incidence;
- national extreme income poverty incidence;
- urban and rural income poverty incidence;
- urban and rural extreme income poverty incidence;
- comparison against published December 2025 ENEMDU poverty benchmarks.

## Out of Scope

This phase does not implement:

- CPI or IPC derivation of poverty lines;
- automatic official data download;
- official validation claims;
- NBI poverty;
- extreme NBI poverty;
- multidimensional poverty indicators;
- dashboard or presentation outputs.

## Required Data Inputs

The workflow expects a person-level ENEMDU microdata table already available in
the local R session. Official microdata must remain outside the repository and
must not be committed.

The local execution script reads the microdata path from the
`ENEMDU_2025_12_SAV` environment variable.

## Required Variables

The default workflow requires:

- `ingtot_pc`: household per-capita income already present in the data;
- `area`: urban/rural domain variable;
- `fexp`: expansion weight;
- `upm`: primary sampling unit;
- `estrato`: survey stratum.

The preflight helper reports whether these variables are present, their class,
and missingness counts. It reports problems without modifying data.

## Official Benchmark Source

The initial benchmark set uses published reference values from the INEC ENEMDU
poverty and inequality publication for December 2025.

The benchmark registry is for comparison only. It is not the operational
poverty-line registry.

## Poverty-Line Policy

The December 2025 workflow supplies poverty lines explicitly:

- poverty line: USD 92.40;
- extreme poverty line: USD 52.07.

The package must not derive these lines from CPI or IPC in this phase. The
default `poverty_line_registry.csv` remains non-operational unless audited rows
are explicitly supplied in a separate workflow.

## Domain Policy

Monthly ENEMDU representativity covers national, urban, and rural domains. The
workflow maps the requested area variable into a temporary
`.enemdu_area_domain` variable with values `urban` and `rural`.

Unmapped area values are excluded from area-domain estimates by the grouping
estimator. Analysts should review the preflight and mapping assumptions before
interpreting results.

## Survey Design Policy

The workflow delegates estimation to `enemdu_kpi_income_poverty()`, which in
turn uses `enemdu_indicator_estimate()` and the package survey design
infrastructure.

Point estimates are computed with ENEMDU design variables. Precision and
representativity metadata remain part of the analytical output.

## Comparison Status Definitions

The benchmark comparison layer classifies differences as:

- `matched_reported_rounding`: difference is within reported rounding;
- `within_tolerance`: difference exceeds reported rounding but remains within
  the requested tolerance;
- `outside_tolerance`: difference exceeds the requested tolerance;
- `missing_official_benchmark`: no official benchmark row matches the package
  estimate key;
- `missing_package_estimate`: an official benchmark row has no matching package
  estimate.

Differences are reported in package scale and percentage points.

## Official Validation Disclaimer

This workflow compares estimates against published benchmarks, but it does not
constitute official validation by INEC and must not be described as official
certification.

An official validation claim would require a reviewed reproducibility run using
the official microdata, documented filters, variables, weights, domains, poverty
lines, and comparison evidence.

## Known Methodological Risks

- Area coding may vary across files and must be verified against the official
  dictionary for the loaded microdata.
- The workflow assumes `ingtot_pc` already exists and is the intended
  household per-capita income variable.
- The current poverty flag policy leaves zero, negative, and missing income
  unclassified.
- Benchmark comparison can identify differences but does not explain them
  without a separate audit of data version, filters, weights, and domain
  definitions.
