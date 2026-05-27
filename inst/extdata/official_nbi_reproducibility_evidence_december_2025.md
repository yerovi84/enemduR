# NBI Reproducibility Evidence — ENEMDU December 2025

## Purpose

This document records local reproducibility evidence for the ENEMDU December 2025 NBI workflow implemented in `enemduR`.

The workflow reconstructs NBI from official ENEMDU person and housing/household sources using the package pipeline:

persona source + housing source
→ controlled NBI source join
→ raw NBI component derivation
→ final NBI flags
→ survey-design-aware KPI estimation
→ comparison against published benchmark values.

This evidence is intended to document local reproducibility against published benchmark values. It is not an official institutional validation by INEC.

## Scope

This evidence covers:

- ENEMDU December 2025.
- NBI poverty incidence.
- NBI extreme poverty incidence.
- National, urban, and rural domains.
- Person-level estimates using ENEMDU expansion weights and survey design variables.

This evidence does not cover:

- TPM, TPEM, intensity, or IPM.
- Income-poverty indicators.
- Labor indicators.
- Any claim of official INEC validation.
- Storage or redistribution of official microdata.

## Local data sources

The workflow was run locally using official ENEMDU December 2025 source files.

Local files used:

- `enemdu_persona_2025_12.sav`
- `enemdu_vivienda_hogar_2025_12.sav`

These files are not committed to the repository.

## Required variables

The person-level source was checked for:

- `id_hogar`
- `p01`
- `p03`
- `p04`
- `p07`
- `p10a`
- `p10b`
- `empleo`
- `area`
- `fexp`
- `upm`
- `estrato`

Result:

- missing person variables: `character(0)`

The housing/household-level source was checked for:

- `id_hogar`
- `vi04a`
- `vi05a`
- `vi07`
- `vi09`
- `vi10`
- `vi10a`

Result:

- missing housing variables: `character(0)`

## Source join diagnostics

The controlled source join was performed with:

- household key: `id_hogar`
- strict mode: `TRUE`
- overwrite: `FALSE`

Diagnostics:

| diagnostic | value |
|---|---:|
| person_rows_before | 27808 |
| person_rows_after | 27808 |
| household_rows | 8748 |
| unique_person_households | 8748 |
| unique_household_rows | 8748 |
| unmatched_person_households | 0 |

Housing variables appended:

- `vi04a`
- `vi05a`
- `vi07`
- `vi09`
- `vi10`
- `vi10a`

Interpretation:

The join preserved person row count and person row order. No household from the person source was unmatched in the housing/household source. No row multiplication was detected.

## NBI derivation profile

The workflow used:

- profile: `enemdu_2025_anual`

The profile derives:

- `comp1`: housing quality
- `comp2`: overcrowding
- `comp3`: basic services
- `comp4`: school attendance / education access
- `comp5`: economic capacity / dependency

Final NBI flags are generated separately:

- `knbi`
- `nbi`
- `xnbi`

## Key methodological adjustments validated during the local workflow

### Economic capacity component

The ENEMDU December 2025 variable `empleo` was observed with the following coding pattern:

| empleo value | interpretation |
|---:|---|
| 1 | Population with employment |
| NA | Not classified as population with employment for this variable |

For the `enemdu_2025_anual` profile, the implemented operational rule is:

- `empleo == 1` is treated as occupied.
- `empleo == NA` is treated as not occupied for the household occupied-person count.

The education variable combination:

- `p10a == 1`
- `p10b == NA`

is interpreted as:

- no schooling
- 0 years of schooling

This avoids treating valid “Ninguno” cases as non-evaluable.

### Basic services component

The basic services component was adjusted after local sensitivity checks showed that the previous water rule overclassified rural deprivation.

For `vi09`, sanitation deprivation codes are:

| code | label |
|---:|---|
| 3 | Excusado y pozo ciego |
| 4 | Letrina |
| 5 | No tiene |

For `vi10`, acceptable water source codes are:

| code | label |
|---:|---|
| 1 | Red pública |
| 3 | Otra fuente por tubería |

For `vi10a`, acceptable water reception codes are:

| code | label |
|---:|---|
| 1 | Por tubería dentro de la vivienda |
| 2 | Por tubería fuera de la vivienda pero en el lote |
| 3 | Por tubería fuera de la vivienda, lote o terreno |

The category:

| code | label |
|---:|---|
| 4 | No recibe agua por tubería sino por otros medios |

is not considered acceptable piped water reception.

## Internal NBI consistency validation

The final reconstructed NBI data passed internal consistency checks.

| metric | value |
|---|---:|
| n | 27808 |
| n_knbi_na | 0 |
| n_nbi_na | 0 |
| n_xnbi_na | 0 |
| inconsistencias_nbi | 0 |
| inconsistencias_xnbi | 0 |
| min_knbi | 0 |
| max_knbi | 4 |
| validation_status | passed |

Interpretation:

The final `knbi`, `nbi`, and `xnbi` variables are internally consistent with the implemented component rules.

## Weighted KPI estimates

Survey-design-aware estimation used:

- ids: `upm`
- strata: `estrato`
- weight: `fexp`
- survey_type: `anual`

### National estimates

| indicator_id | estimate | percent | standard_error | cv | ci_lower | ci_upper | official_validation_status |
|---|---:|---:|---:|---:|---:|---:|---|
| pobreza_nbi | 0.311 | 31.1 | 0.0296 | 0.0950 | 0.253 | 0.369 | not_officially_validated |
| pobreza_extrema_nbi | 0.0681 | 6.81 | 0.0131 | 0.192 | 0.0424 | 0.0937 | not_officially_validated |

### Area estimates

| area | indicator_id | estimate | percent | standard_error | cv | ci_lower | ci_upper | official_validation_status |
|---|---|---:|---:|---:|---:|---:|---:|---|
| Urbana | pobreza_nbi | 0.205 | 20.5 | 0.0326 | 0.159 | 0.141 | 0.269 | not_officially_validated |
| Rural | pobreza_nbi | 0.539 | 53.9 | 0.0567 | 0.105 | 0.428 | 0.650 | not_officially_validated |
| Urbana | pobreza_extrema_nbi | 0.0390 | 3.90 | 0.0126 | 0.324 | 0.0142 | 0.0637 | not_officially_validated |
| Rural | pobreza_extrema_nbi | 0.130 | 13.0 | 0.0237 | 0.182 | 0.0839 | 0.177 | not_officially_validated |

## Published benchmark comparison

The following published benchmark values were used in the local comparison for `pobreza_nbi`:

| domain | official_percent |
|---|---:|
| nacional | 30.8 |
| urbano | 20.1 |
| rural | 53.9 |

The local calculated values were:

| domain | calculated_percent | official_percent | difference_pp | abs_difference_pp |
|---|---:|---:|---:|---:|
| nacional | 31.1 | 30.8 | 0.3 | 0.3 |
| urbano | 20.5 | 20.1 | 0.4 | 0.4 |
| rural | 53.9 | 53.9 | 0.0 | 0.0 |

Values are reported at one decimal place to match the published benchmark scale.

## Interpretation

The December 2025 local NBI workflow produces estimates that are very close to the published benchmark values for NBI poverty incidence:

- National difference: approximately 0.3 percentage points.
- Urban difference: approximately 0.4 percentage points.
- Rural difference: approximately 0.0 percentage points.

This is strong evidence of local reproducibility against published benchmark values, subject to the methodological limitations documented below.

## Limitations

This evidence does not constitute official validation by INEC.

The local workflow depends on:

- the local official ENEMDU December 2025 source files;
- correct person-to-housing joining by `id_hogar`;
- the implemented `enemdu_2025_anual` NBI derivation profile;
- the package survey design handling;
- benchmark values entered externally for comparison.

The comparison is currently documented as local reproducibility against published values, not as institutional certification.

## Repository status

This evidence file documents results obtained after merging the following NBI layers into `main`:

- final NBI components to `knbi`, `nbi`, and `xnbi`;
- raw ENEMDU questionnaire variables to final NBI components;
- controlled person plus housing source join;
- economic capacity correction for ENEMDU employment coding;
- basic services correction for water and sanitation codes.

Exact commit hashes should be taken from the local repository with:

`git log --oneline --decorate -5`

before final release documentation if commit-level traceability is required.

## Recommended next steps

1. Commit this evidence file to the repository if the project decision is to keep reproducibility evidence under `inst/extdata`.
2. Optionally add a small local script under `inst/scripts` that reproduces this workflow using environment-variable paths to local official microdata.
3. Do not add official microdata to the repository.
4. Do not claim official INEC validation.
5. Before starting TPM/IPM, close the NBI phase with a continuity capsule and updated phase status.
