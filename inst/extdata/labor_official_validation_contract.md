# Labor Official Validation Contract

## Purpose

This document records the official-validation contract used for the initial labor-indicator block in `enemduR`.

The goal of this contract is to make validation decisions explicit, auditable, and reproducible. It documents how `enemduR` labor indicators were compared against official INEC labor-market tabulations, which domains were validated, which tolerances were accepted, and which operational exceptions were required to match published tabular conventions without changing the analytical calculation logic.

This document is part of the package metadata layer. It is not an analytical function and it does not change any estimator.

## Scope

This validation contract applies to the initial implementation of labor indicators produced by:

`enemdu_kpi_employment()`

The implementation is based on the consolidated ENEMDU labor-status variable:

`condact`

The current implementation does not reconstruct labor status from raw questionnaire variables.

## Source Basis

The validation decisions in this document are based on:

- Official ENEMDU methodological documentation for 2021-2025.
- Official ENEMDU sample-design documentation.
- Official ENEMDU database user guide.
- Official INEC labor-market CSV tabulations used as external comparison targets.
- Internal `enemduR` package contracts and tests.

## Operational Data Sources Used in Validation

The following official microdata cuts were used in the validation workflow:

- Monthly ENEMDU microdata: March 2026.
- Quarterly ENEMDU microdata: First quarter 2026.
- Annual ENEMDU microdata: Annual 2025.

The following official labor-market tabulations were used as comparison targets:

- Monthly labor-market tabulations: March 2026.
- Quarterly labor-market tabulations: First quarter 2026.
- Annual labor-market tabulations: 2025.

## File Format Contract

Official recent ENEMDU microdata are treated operationally as SPSS `.sav` files.

The function `enemdu_read_data()` supports:

- `.sav` as the operational primary format for official recent ENEMDU microdata;
- `.dta` as an interoperability format;
- `.csv` as an interoperability format.

This file-format contract does not imply that official ENEMDU production is limited to `.sav`. It only records the package-level operational standard used in this validation phase.

## Design Variables

The package design declaration uses the following core survey-design variables:

- `upm`
- `estrato`
- `fexp`

These variables are treated as canonical for the package-level survey design workflow.

## Labor Indicator Contract

The current labor-indicator implementation uses the consolidated variable `condact`.

The following `condact` categories are used by the initial labor block:

- `0`: population younger than 15 years;
- `1`: adequate/full employment;
- `2`: time-related underemployment;
- `3`: income-related underemployment;
- `4`: other non-full employment;
- `5`: unpaid employment;
- `6`: unclassified employment;
- `7`: open unemployment;
- `8`: hidden unemployment;
- `9`: economically inactive population.

The sector-employment indicators use `secemp` when required.

The current official-validation workflow compares the labor indicators that are common between `enemduR` outputs and the published official tabulations.

## Output Scale Contract

`enemduR` stores labor rates as proportions in `[0, 1]`.

Official tabulations may publish rates as percentages. Therefore, official rates are converted to package scale before comparison.

Examples:

- `63,8` in a monthly or quarterly CSV rate is interpreted as `63.8%` and converted to `0.638`.
- `96.4%` in an annual CSV estimator is interpreted as `96.4%` and converted to `0.964`.

Counts are kept in count scale.

## Numeric Parsing Contract

The official-tabulation parser must distinguish between:

- thousand separators in counts, such as `9.309.490`;
- comma-decimal percentages, such as `63,8`;
- dot-decimal percentages, such as `96.4%`;
- non-numeric dash cells, such as `-`.

The parser must not globally treat every dash as zero. Dash handling is restricted by the rule defined in the section "Published Dash Handling".

## Period Matching Contract

Official period labels may not be fully standardized across published tabulations.

For validation, period labels are matched using a normalized period key so that labels such as the following are treated as equivalent when appropriate:

- `I - 2026`
- `I- 2026`

This normalization is used only to select comparable official rows. It does not modify the original period label stored in the returned object.

## Domain Contract

### Monthly ENEMDU

Monthly ENEMDU validation covers the following official design domains:

- National;
- urban area;
- rural area.

### Quarterly ENEMDU

Quarterly ENEMDU validation covers the following official design domains:

- National;
- urban area;
- rural area;
- five main cities.

For the five main cities published under official domain tabulations, the package validation must use the derived variable:

`dominio`

The variable `ciudad` must not be assumed to be equivalent to the published official city-domain tabulations.

### Annual ENEMDU

Annual ENEMDU validation covers the following official design domains:

- National;
- urban area;
- rural area;
- five main cities;
- 24 provinces.

For the five main cities published as official city domains, the package validation must use the derived variable:

`dominio`

For provinces, the package validation uses:

`prov`

## Domain Alias Contract

The annual official tabulation labels one province as:

`Santo Domingo`

The corresponding labelled province in the official microdata is:

`Santo Domingo de los Tsáchilas`

For validation against official tabulations, the following alias is accepted:

`Santo Domingo` -> `Santo Domingo de los Tsáchilas`

This is a label-alignment rule for validation. It is not a statistical transformation.

## Published Dash Handling

Official tabulations may publish `-` for some cells.

The package accepts `-` as a validation match only under all of the following conditions:

- the official value is non-numeric and exactly `-`;
- the package scale is `count`;
- the package estimate is not missing;
- the package estimate is equal to zero or within the accepted count tolerance from zero.

When these conditions are met, the comparison status may be adjusted to:

`match_official_dash_zero`

This rule must not be applied globally to rates, coefficients of variation, standard errors, or other non-count measures.

## Validation Tolerances

The following tolerances were used in the official-validation workflow:

- `tolerance_count = 1`
- `tolerance_rate = 0.0006`

The rate tolerance is expressed in package scale. Therefore:

`0.0006 = 0.06 percentage points`

This tolerance accounts for official tabulations published with rounded percentage values, usually at one decimal place.

The tolerance does not change the internal estimator. It is used only for comparison against published official tabulations.

## Validated Domains and Results

### Monthly March 2026

Validated domains:

- National;
- urban area;
- rural area.

Status:

- passed against official labor-market tabulations.

### Quarterly First Quarter 2026

Validated domains:

- National;
- urban area;
- rural area;
- Cuenca;
- Machala;
- Guayaquil;
- Quito;
- Ambato.

Status:

- passed against official labor-market tabulations.

Special handling:

- `Empleo no clasificado` for Cuenca and Machala was published as `-` in official count cells and matched package estimates equal to zero under the published-dash rule.

### Annual 2025

Validated domains:

- National;
- urban area;
- rural area;
- Cuenca;
- Machala;
- Guayaquil;
- Quito;
- Ambato;
- Azuay;
- Bolívar;
- Carchi;
- Cañar;
- Chimborazo;
- Cotopaxi;
- El Oro;
- Esmeraldas;
- Galápagos;
- Guayas;
- Imbabura;
- Loja;
- Los Ríos;
- Manabí;
- Morona Santiago;
- Napo;
- Orellana;
- Pastaza;
- Pichincha;
- Santa Elena;
- Santo Domingo;
- Sucumbíos;
- Tungurahua;
- Zamora Chinchipe.

Status:

- passed against official labor-market tabulations.

Special handling:

- official `Santo Domingo` was matched to microdata label `Santo Domingo de los Tsáchilas`.

## Known Scope Limits

This validation contract does not close the following items:

- reconstruction of labor status from raw questionnaire variables;
- validation of non-labor indicators;
- validation of poverty indicators;
- validation of income aggregates;
- validation of every official precision statistic published by INEC;
- validation of all historical periods;
- validation of unofficial analytical domains.

## Risk Status

Risk `R-4-001`, defined as lack of validation against official INEC published figures, is considered closed for the initial labor-indicator block and for the official domains listed in this document.

The risk remains open for future modules outside the initial labor block.

## Implementation Principle

No estimator should be changed only to match a published rounded table.

When a difference is caused by publication format, rounding, label aliases, or non-numeric display conventions, the validation layer should document and handle the comparison rule explicitly.

## Portfolio Note

This validation contract demonstrates that `enemduR` is not only a convenience wrapper. It includes a reproducible validation workflow against official statistical outputs, separates analytical computation from publication-format reconciliation, and records methodological decisions in an auditable metadata layer.
