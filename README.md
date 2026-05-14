# enemduR

`enemduR` is an R package that provides analytical infrastructure for reproducible work with Ecuador's ENEMDU microdata.

The package is designed as an analytical engine, not as a dashboard or final visual product. Its purpose is to standardize reading, validation, derivation, indicator estimation, survey-design-aware tabulation, representativity assessment, and Quarto-ready analytical outputs for technical workflows using ENEMDU microdata.

`enemduR` is especially useful for analysts, researchers, institutional teams, and advanced users working with official microdata, technical reports, Quarto websites, reproducible notebooks, and policy-oriented analytical systems.

## Project status

`enemduR` is currently in active development.

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
- an initial labor indicator module based on the consolidated `condact` variable;
- a formal labor indicator registry with 32 implemented labor indicators;
- official labor-market tabulation parsing;
- comparison helpers for official labor-market validation workflows;
- Quarto-compatible analytical output helpers.

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

```r
labor_city_domains <- enemdu_kpi_employment(
  data = quarterly_data,
  group_vars = "dominio",
  survey_type = "trimestral",
  domain_scope = "design"
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
  data = quarterly_data,
  group_vars = "dominio",
  survey_type = "trimestral",
  domain_scope = "design"
)
```

This distinction is important because analysis domains and design domains are not the same concept.

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
- `std_error`;
- `cv`;
- `conf_low`;
- `conf_high`;
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

## Quarto-ready analytical consumption

`enemduR` includes lightweight Quarto-compatible helpers, but the package remains an analytical core.

Presentation helpers are downstream consumers of analytical outputs. They should not control how indicators are computed.

Available helpers include:

```r
enemdu_card_kpi()
enemdu_value_box()
enemdu_badge_quality()
enemdu_note_method()
enemdu_section_header()
```

These helpers are intentionally lightweight at this stage.

## Current limitations

The current development version does not yet provide:

- full reconstruction of labor status from raw questionnaire variables;
- a complete historical harmonization engine for all ENEMDU questionnaire changes;
- official production-grade poverty estimation workflows;
- a full pkgdown site;
- continuous integration through GitHub Actions;
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

## Portfolio summary

`enemduR` demonstrates a reproducible analytical infrastructure for official microdata workflows. It combines package engineering, survey-design-aware estimation, metadata contracts, validation against official labor tabulations, and Quarto-ready analytical outputs.

The project is designed to support technical reporting, institutional dashboards, policy intelligence, and reproducible research while preserving methodological traceability and a strict separation between computation and presentation.
