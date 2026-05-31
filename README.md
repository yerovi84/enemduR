# enemduR

[![R-CMD-check](https://github.com/yerovi84/enemduR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/yerovi84/enemduR/actions/workflows/R-CMD-check.yaml)

<p align="center">
  <img src="man/figures/big-y-logo.png" alt="Big Y Productions analytics brand seal" width="320" />
</p>

<p align="center">
  <strong>Reproducible analytical infrastructure for Ecuador ENEMDU microdata.</strong><br>
  Built by Alejandro Yerovi | Big Y Productions<br>
  Survey Statistics | Policy Analytics | Reporting Automation | R
</p>

<p align="center">
  <a href="https://yerovi84.github.io/enemduR/">Documentation</a> |
  <a href="https://github.com/yerovi84/enemduR">GitHub</a> |
  <a href="https://github.com/yerovi84/enemduR/releases/tag/v0.1.0-alpha">v0.1.0-alpha pre-release</a> |
  <a href="LICENSE">MIT license</a> |
  <a href="CITATION.cff">Citation</a>
</p>

`enemduR` is an R package for technical workflows that need standardized reading, validation, derivation, survey-design-aware estimation, representativity assessment, and Quarto-ready analytical outputs using Ecuador's ENEMDU microdata.

The package is designed as an analytical engine, not as a dashboard or final visual product. It is especially useful for analysts, researchers, institutional teams, and advanced users working with official microdata, technical reports, reproducible notebooks, and policy-oriented analytical systems.

## Project status

`enemduR` is currently in active development.

The tagged `v0.1.0-alpha` build is a public pre-release for review and
portfolio demonstration, not a final production release.

The current development version includes:

- reading support for `.sav`, `.dta`, and `.csv` ENEMDU files;
- `.sav` as the operational primary format for recent official ENEMDU microdata workflows;
- standardized name normalization;
- structural and content validation helpers;
- metadata registries for variables, indicators, missing codes, value ranges, domains, poverty lines, optional bonuses, and official dictionaries;
- complex survey design declaration using ENEMDU design variables;
- survey estimators for totals, means, proportions, and tabulations;
- representativity and precision-evaluation infrastructure;
- official dictionary reading and microdata validation against dictionary files;
- income, quintile, household-profile, poverty-flag, optional-bonus, and social-bonus infrastructure;
- IPM/TPM component, flag, KPI, benchmark-comparison, and local reproducibility infrastructure for the ENEMDU 2025 annual profile;
- an initial labor indicator module based on the consolidated `condact` variable;
- a formal labor indicator registry with 32 implemented labor indicators;
- official labor-market tabulation parsing;
- comparison helpers for official labor-market validation workflows;
- stable analytical outputs ready for downstream Quarto consumption;
- a basic GitHub Actions R package check workflow;
- a published pkgdown documentation site for package reference and vignettes.

The package is not intended to replace official statistical production systems or official published results. It is an analytical infrastructure project designed to improve reproducibility, transparency, and methodological consistency in downstream ENEMDU workflows.

## Design principles

`enemduR` follows a small set of non-negotiable design principles:

1. Separate analytical computation from presentation.
2. Keep Quarto, dashboards, and visual products as downstream consumers.
3. Prefer stable long-format tibbles for analytical outputs.
4. Declare the unit of analysis, universe, weight, and restrictions whenever they matter.
5. Preserve a clear distinction between analysis domains and design domains.
6. Use explicit metadata contracts instead of implicit recoding.
7. Avoid global recoding of sentinel values unless a registry explicitly authorizes it.
8. Do not compute poverty indicators without explicit and auditable poverty lines.
9. Do not present estimates without precision or representativity assessment when such evaluation is required.
10. Do not confuse weighted population counts with effective sample size.

## ENEMDU scope

The package is designed for the current ENEMDU structure with monthly, quarterly, and annual workflows.

Current representativity contracts used by the package are:

| Survey type | Design and representativity scope |
|---|---|
| Monthly ENEMDU | National and area: urban/rural |
| Quarterly ENEMDU | National, area: urban/rural, and five main cities |
| Annual ENEMDU | National, area: urban/rural, five main cities, and 24 provinces |

The five main cities are:

- Quito;
- Guayaquil;
- Cuenca;
- Machala;
- Ambato.

For recent official ENEMDU microdata workflows, the package treats `.sav` files as the operational primary input format. `.dta` and `.csv` are supported as interoperability formats.

## Core survey-design variables

The default ENEMDU survey-design workflow uses:

| Role | Variable |
|---|---|
| Primary sampling unit | `upm` |
| Strata | `estrato` |
| Expansion factor | `fexp` |

These variables are used by survey-design-aware functions such as:

- `enemdu_declare_design()`;
- `enemdu_survey_total()`;
- `enemdu_survey_mean()`;
- `enemdu_survey_proportion()`;
- `enemdu_survey_tabulate()`;
- `enemdu_kpi_employment()`.

## Installation

During active development, load the package locally from the project root:

```r
devtools::load_all(reset = TRUE)
```

After local integration, run the test suite with:

```r
devtools::test()
```

For a full package check, run:

```r
devtools::check()
```

## Basic workflow

A typical `enemduR` workflow follows this sequence:

```r
library(enemduR)

enemdu_data <- enemdu_read_data(
  path = "path/to/enemdu_persona_2026_03.sav",
  survey_type = "mensual",
  period = "2026-03"
)

structure_report <- enemdu_validate_structure(enemdu_data)

diagnosis <- enemdu_diagnose_data(enemdu_data)

labor_kpis <- enemdu_kpi_employment(
  data = enemdu_data,
  survey_type = "mensual"
)
```

The resulting analytical outputs are designed to be consumed by reporting layers, Quarto documents, dashboards, validation scripts, or further statistical workflows.

## Reading ENEMDU microdata

Use `enemdu_read_data()` to read official or interoperable ENEMDU files.

```r
monthly_data <- enemdu_read_data(
  path = "path/to/enemdu_persona_2026_03.sav",
  survey_type = "mensual",
  period = "2026-03"
)

quarterly_data <- enemdu_read_data(
  path = "path/to/enemdu_persona_2026_I_trimestre.sav",
  survey_type = "trimestral",
  period = "2026-I"
)

annual_data <- enemdu_read_data(
  path = "path/to/BDDenemdu_personas_2025_anual.sav",
  survey_type = "anual",
  period = "2025"
)
```

Supported file formats:

| Format | Role |
|---|---|
| `.sav` | Operational primary format for recent official ENEMDU microdata workflows |
| `.dta` | Interoperability format |
| `.csv` | Interoperability format |

## Declaring the survey design

`enemduR` provides a helper for declaring the ENEMDU complex survey design:

```r
design <- enemdu_declare_design(
  data = monthly_data,
  ids = "upm",
  strata = "estrato",
  weights = "fexp"
)
```

Survey estimators use the same default design variables.

Example: total estimation.

```r
total_income <- enemdu_survey_total(
  data = monthly_data,
  value = "ingrl",
  survey_type = "mensual",
  indicator_id = "income_labor_total",
  measure = "Labor income total"
)
```

Example: mean estimation by area.

```r
mean_income_area <- enemdu_survey_mean(
  data = monthly_data,
  value = "ingrl",
  group_vars = "area",
  survey_type = "mensual",
  indicator_id = "income_labor_mean_area",
  measure = "Mean labor income by area"
)
```

Example: proportion estimation for a previously built binary flag.

```r
poverty_rate <- enemdu_survey_proportion(
  data = monthly_data,
  value = "poverty_flag",
  group_vars = "area",
  survey_type = "mensual",
  indicator_id = "poverty_rate_area",
  measure = "Poverty rate by area"
)
```

Poverty flags must only be built from explicit and auditable poverty lines. The package does not treat poverty computation as a generic binary recoding task.

## Labor indicators

The current labor indicator module is based on the consolidated ENEMDU variable:

```text
condact
```

The current implementation does not reconstruct labor status from raw questionnaire variables.

The main function is:

```r
labor_kpis <- enemdu_kpi_employment(
  data = monthly_data,
  survey_type = "mensual"
)
```

The package includes a formal labor indicator registry:

```r
labor_registry <- enemdu_labor_indicator_registry()
```

The registry currently includes 32 implemented labor indicators, covering labor-population totals, employment totals, unemployment totals, labor-sector totals, and core labor-market rates.

### Labor indicators by area

```r
labor_area <- enemdu_kpi_employment(
  data = monthly_data,
  group_vars = "area",
  survey_type = "mensual",
  domain_scope = "design"
)
```

### Quarterly city-domain indicators

For official quarterly city-domain validation, use `dominio`, not `ciudad`.
The current validation examples keep `domain_scope = "observed"` for
`dominio` because `dominio` is the official city-domain comparison field; the
estimator still uses the full input microdata.

```r
labor_city_domains <- enemdu_kpi_employment(
  data = quarterly_data,
  group_vars = "dominio",
  survey_type = "trimestral",
  domain_scope = "observed"
)
```

### Annual provincial indicators

For annual provincial outputs, use `prov`.

```r
labor_provinces <- enemdu_kpi_employment(
  data = annual_data,
  group_vars = "prov",
  survey_type = "anual",
  domain_scope = "design"
)
```

## Domain scope

`enemdu_kpi_employment()` supports two domain-scope modes:

```r
domain_scope = "observed"
domain_scope = "design"
```

### `domain_scope = "observed"`

This is the default. It preserves all observed values in the grouping variables after estimation.

Use it for exploratory workflows and for backward-compatible analysis.

```r
labor_observed <- enemdu_kpi_employment(
  data = quarterly_data,
  group_vars = "dominio",
  survey_type = "trimestral",
  domain_scope = "observed"
)
```

### `domain_scope = "design"`

This mode estimates using the full input microdata and filters the output to the official design domains for the declared survey type.

It does not prefilter the microdata before estimation.

```r
labor_design <- enemdu_kpi_employment(
  data = monthly_data,
  group_vars = "area",
  survey_type = "mensual",
  domain_scope = "design"
)
```

This distinction is important because analysis domains and design domains are not the same concept.

Do not use `domain_scope = "design"` as a substitute for mapping `ciudad` to
published official city-domain tabulations. For official city-domain
validation, use `dominio` when that field is available in the analytical
workflow.

## Official labor tabulation validation workflow

`enemduR` includes helper infrastructure for reading and comparing official labor-market tabulations.

```r
official_labor <- enemdu_read_official_labor_tabulados(
  path = "path/to/202603_Tabulados_Mercado_Laboral_CSV.zip",
  survey_type = "mensual"
)

comparison <- enemdu_compare_labor_tabulados(
  estimates = labor_kpis,
  official = official_labor,
  official_period = "mar-26",
  domain_group = "Nacional",
  domain_label = "Total",
  tolerance_count = 1,
  tolerance_rate = 0.0006,
  strict = FALSE
)
```

The validation workflow is intentionally conservative. It compares only official rows that can be mapped to implemented package indicators.

Rates are stored internally as proportions in `[0, 1]`. Official tabulations may publish rates as percentages, so official values are converted to package scale before comparison.

Examples:

| Official published value | Package scale |
|---|---|
| `63,8` | `0.638` |
| `96.4%` | `0.964` |

Counts remain in count scale.

## Official validation coverage for the current labor block

The current labor validation workflow covers the following official comparison targets:

| Source | Domains covered |
|---|---|
| Monthly March 2026 | National, urban area, rural area |
| Quarterly First Quarter 2026 | National, urban area, rural area, five main cities |
| Annual 2025 | National, urban area, rural area, five main cities, 24 provinces |

For the annual provincial comparison, the following label alias is accepted only for validation alignment:

```text
Santo Domingo -> Santo Domingo de los Tsáchilas
```

This is a label-alignment rule. It is not a statistical transformation.

## Output philosophy

Analytical outputs are intended to be stable, auditable, and easy to consume.

Most analytical functions return tibbles with fields such as:

- `indicator_id`;
- `indicator_label`;
- `estimate`;
- `standard_error`;
- `cv`;
- `ci_lower`;
- `ci_upper`;
- `unweighted_n`;
- `weighted_n`;
- `quality_flag`;
- `precision_flag`;
- domain or grouping variables when applicable.

The package treats `weighted_n` as an expanded population count, not as an effective sample size.

## Metadata registries

`enemduR` uses explicit registries to reduce hidden assumptions.

Available registry helpers include:

```r
enemdu_variable_catalog()
enemdu_indicator_registry()
enemdu_labor_indicator_registry()
enemdu_validation_registry()
enemdu_missing_code_registry()
enemdu_value_range_registry()
enemdu_poverty_line_registry()
enemdu_income_component_registry()
enemdu_optional_bonus_registry()
enemdu_domain_registry()
enemdu_domain_variable_registry()
enemdu_official_dictionary_core_registry()
```

These registries support validation, documentation, and reproducibility.

## Missing values and sentinel codes

The package does not globally recode sentinel values such as:

```text
999
9999
99999
999999
```

Sentinel values are handled through explicit contracts and registries.

This prevents accidental changes to variables where the same code may have a valid meaning, an instrument-specific meaning, or no authorized recoding rule.

## Poverty and income contracts

`enemduR` includes infrastructure for income-related workflows, quintiles, poverty flags, and poverty-line registries.

However:

- poverty is not computed without explicit poverty lines;
- income per capita can be built without poverty classification;
- poverty flags must be auditable;
- poverty-line validation must be explicit;
- income and transfer scenarios must avoid double counting.

This is especially important when working with social bonuses, transfer variables, and alternative income scenarios.

## IPM/TPM reproducibility

`enemduR` includes a profile-specific IPM workflow for the ENEMDU 2025 annual profile. The workflow builds 12 deprivation components, applies the registered IPM weights, builds the weighted deprivation score, and estimates TPM, TPEM, deprivation intensity, and M0/IPM with the ENEMDU survey design.

The IPM score and poverty flags follow these cutoffs:

| Output | Rule |
|---|---|
| `ipm_score` | Weighted deprivation score across the 12 components |
| `tpm` | `ipm_score >= 4 / 12` |
| `tpem` | `ipm_score >= 6 / 12` |
| `A` | Average deprivation intensity among multidimensionally poor persons |
| `ipm` | `TPM * A` |

Component evidence is aggregated at household level and reported at person level. Missing-critical households are diagnosed before score calculation. For strict local reproducibility runs, `enemdu_run_ipm_reproducibility()` can use an explicit complete-case component policy that excludes incomplete IPM evidence before KPI estimation and benchmark comparison.

The ENEMDU December 2025 smoke test provides strong local reproducibility evidence for TPM, TPEM, and IPM benchmark comparison. No institutional validation is claimed; any official validation would require explicit authorization or confirmation by the relevant official authority. Package outputs retain:

```text
official_validation_status = "not_officially_validated"
```

Important profile-specific implementation details are:

- i03 uses `official_recode_to_zero` for unknown schooling in `profile = "enemdu_2025_anual"`, mirroring the profile-specific official syntax while preserving diagnostic counts.
- i04 uses `official_syntax_rule` when `pea` is available and `official_like_with_derived_pea` when `pea` is missing but `empleo` and `desempleo` are available. The derived PEA is internal to the i04 component and does not create a public labor-market API.
- i06 uses `official_syntax_rule` when `pet`, `pei`, and `desem` are available and `official_like_with_derived_labor_status` when labor status can be derived internally from age, `empleo`, and `desempleo`. Consistency against `condact` is reported diagnostically.

The retained December 2025 evidence is documented in `inst/extdata/official_ipm_reproducibility_evidence_december_2025.md`.

## Quarto-ready analytical consumption

`enemduR` produces stable long-format analytical tables that are designed to be
consumed by Quarto documents, validation notebooks, dashboards, and reporting
pipelines. The package remains an analytical core.

Presentation layers are downstream consumers of analytical outputs. They should
not control how indicators are computed.

The current package also reserves a small set of Quarto helper interfaces. At
this stage, these helpers are placeholders that fail with informative messages;
production Quarto documents should consume the analytical tibbles directly or
implement presentation-specific wrappers outside the computational core.

Reserved helper interfaces include:

```r
enemdu_card_kpi()
enemdu_value_box()
enemdu_badge_quality()
enemdu_note_method()
enemdu_section_header()
```

## Current limitations

The current development version does not yet provide:

- full reconstruction of labor status from raw questionnaire variables;
- a complete historical harmonization engine for all ENEMDU questionnaire changes;
- official production-grade poverty estimation workflows;
- a full custom documentation design system beyond the restrained portfolio identity;
- fully implemented Quarto helper objects;
- a complete visual dashboard layer;
- full release hardening.

These are future development areas.

## Recommended development checks

After modifying the package, run:

```r
devtools::load_all(reset = TRUE)
devtools::test()
```

For release-style hardening, run:

```r
devtools::check()
```

If documentation is modified through roxygen comments, run:

```r
devtools::document()
```

To build the local documentation site when `pkgdown` is available, run:

```r
pkgdown::build_site()
```

## Portfolio summary

`enemduR` demonstrates a reproducible analytical infrastructure for official microdata workflows. It combines package engineering, survey-design-aware estimation, metadata contracts, validation against official labor tabulations, and Quarto-ready analytical outputs.

The project is designed to support technical reporting, institutional dashboards, policy intelligence, and reproducible research while preserving methodological traceability and a strict separation between computation and presentation.
