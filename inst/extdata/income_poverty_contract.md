# Income And Poverty Contract

## Scope

This document records the design contract for a future income and poverty
module in `enemduR`. It is a planning document only. It does not implement new
analytical behavior, change existing income definitions, add official poverty
lines, or claim official validation.

The next implementation phase should strengthen existing infrastructure for:

- income construction;
- household and per-capita income;
- income quintiles;
- explicit poverty-line management;
- poverty and extreme-poverty flags;
- optional social-bonus income scenarios;
- survey-design-aware analytical outputs.

## Non-Negotiable Rules

- Do not compute poverty without explicit and auditable poverty lines.
- Do not invent ENEMDU variables.
- Do not invent official poverty methodology.
- Do not globally recode sentinel values.
- Do not alter existing income definitions unless a current registry or
  function already defines them.
- Do not sum bonus variables into income unless an explicit scenario contract
  authorizes it.
- Do not claim official validation unless it was actually run and evidence is
  retained.
- Keep computation separated from presentation.
- Prefer stable long-format tibbles for analytical outputs.

## Existing Assets

The repository already contains income and poverty infrastructure:

- `R/build_variables.R`
  - `enemdu_build_variables()` derives `hsize`, `ingr`, `ingrls`, `ingrl`,
    `ingrltot`, `ingtot`, and `ingtot_pc`.
  - `enemdu_build_quintiles()` builds weighted or unweighted income quantile
    groups, defaulting to `ingtot_pc` and `fexp`.
  - `enemdu_build_household_profile()` creates one-row-per-household profiles.
- `R/poverty_lines.R`
  - `enemdu_poverty_line_registry()` reads the poverty-line registry.
  - `enemdu_validate_poverty_lines()` checks registry structure and operational
    readiness.
  - `enemdu_get_poverty_line()` resolves one auditable poverty-line row.
- `R/poverty_flags.R`
  - `enemdu_build_poverty_flags()` builds poverty and extreme-poverty flags
    only after explicit line resolution.
  - Strict mode uses a registry; manual mode requires explicit line values and
    a source note; diagnostic mode does not compute flags.
- `R/optional_bonuses.R`
  - `enemdu_build_optional_bonuses()` validates social-bonus amount and receipt
    pairs.
  - Scenario income adds only registry-authorized bonuses not already included
    in the base income construction.
  - `enemdu_kpi_social_bonuses()` estimates validated social-bonus KPIs.
- `R/indicator_estimate.R`
  - `enemdu_kpi_income()` estimates current income KPIs when required derived
    variables are present.
- `R/survey_estimators.R` and `R/indicator_table.R`
  - survey-design-aware totals, means, proportions, and long-format indicator
    tables are already available.

## Existing Registries

The following registries already support the future module:

- `inst/extdata/variable_catalog.csv`
  - declares income source variables, derived income variables, household
    identifiers, social-bonus variables, and scenario-income variables.
- `inst/extdata/income_component_registry.csv`
  - records the current income construction stages and component-level
    operations used by existing income derivation.
- `inst/extdata/missing_code_registry.csv`
  - records missing-code rules scoped to income derivation instead of global
    recoding.
- `inst/extdata/poverty_line_registry.csv`
  - provides the required poverty-line schema, but current rows are templates
    or pending review, not operational official lines.
- `inst/extdata/optional_bonus_registry.csv`
  - defines validated social-bonus amount/receipt pairs and whether each bonus
    belongs in base income or an optional scenario.
- `inst/extdata/indicator_registry.csv`
  - includes income, poverty, income-distribution, social-bonus, and
    scenario-indicator rows.

## Current Contracts

### Income

Current income derivation builds household per-capita income but deliberately
does not derive poverty status. The current base income path is encoded in
`enemdu_build_variables()` and `income_component_registry.csv`; future work
should not alter that path without an explicit registry-backed change.

Current derived variables:

- `hsize`: household size;
- `ingr`: main labor income;
- `ingrls`: secondary labor income;
- `ingrl`: total labor income;
- `ingrltot`: individual total income before household aggregation;
- `ingtot`: household total income;
- `ingtot_pc`: household per-capita income.

### Quintiles

Quintiles are currently derived by `enemdu_build_quintiles()` from a numeric
income variable. The default is `ingtot_pc`, with `fexp` as the default weight.
Current behavior assigns quantile groups only for positive, non-missing income
values.

### Poverty Lines

The package has a poverty-line registry schema and strict validation helpers.
The package registry intentionally does not invent operational line values.
Strict poverty computation must fail unless the requested period has both
poverty and extreme-poverty rows with positive values, currency, source notes,
and non-pending source status.

### Poverty Flags

Poverty flags are computed by `enemdu_build_poverty_flags()` only after line
resolution. Existing output defaults are:

- `pobre`;
- `expobre`;
- `linea_pobreza`;
- `linea_pobreza_extrema`.

The current rule classifies only records with positive, non-missing income.
Missing or non-positive income remains unclassified.

### Current Edge-Case Contract

Current income and poverty helpers intentionally use a conservative edge-case
contract:

- `enemdu_build_variables()` sets derived household per-capita income to
  missing when the resulting value is zero or non-positive.
- `enemdu_build_quintiles()` assigns income groups only to positive,
  non-missing income values; missing, zero, and negative income remain
  unclassified.
- `enemdu_build_poverty_flags()` classifies poverty and extreme poverty only
  for positive, non-missing income values; missing, zero, and negative income
  remain unclassified under the current implementation.

This is a current package contract, not an official poverty methodology claim.
Any future change to non-positive income handling requires explicit
methodological review, tests, and documentation. Official poverty validation
has not been run for these income and poverty contracts.

### Optional Bonus Scenarios

The current optional-bonus contract validates amount variables against receipt
questions before deriving bonus outputs. Scenario income is separate from base
income and uses only bonuses authorized by `optional_bonus_registry.csv`.

Current scenario variables include:

- `bonos_scenario_add_total`;
- `ingrltot_plus_optional_bonos`;
- `ingtot_plus_optional_bonos`;
- `ingtot_pc_plus_optional_bonos`.

Future poverty scenarios must explicitly name the income variable and poverty
line source used. They must not silently replace the base poverty contract.

## Gaps Before Implementation

- No operational official poverty-line values are present in the package
  registry.
- No public, repository-level contract yet maps supported poverty periods to
  audited source documents.
- Income derivation currently exists, but the module lacks a single user-facing
  orchestration contract for income, quintiles, poverty lines, flags, and
  survey estimates.
- Scenario income exists for optional bonuses, but scenario naming,
  interpretation, and allowed outputs should be documented before broader use.
- Poverty indicators exist in the indicator registry, but official poverty
  validation has not been run and must not be claimed.
- More tests are needed for end-to-end combinations of income derivation,
  poverty-line resolution, poverty flags, quintiles, indicator estimation, and
  scenario income.
- Documentation should make clear which outputs are person-level,
  household-repeated-person, or household-profile outputs.

## Phase 7 Implementation Roadmap

1. Contract audit PR
   - Add tests that assert the current income, poverty-line, poverty-flag, and
     optional-bonus contracts without changing behavior.
   - Confirm that default package poverty lines remain non-operational until
     explicit audited lines are supplied.

2. Poverty-line source PR
   - Add or document a workflow for user-supplied poverty-line registries.
   - Do not add official line values unless the source, period, currency,
     update method, and audit trail are present.

3. Income orchestration PR
   - Add a small wrapper only if it composes existing functions without changing
     definitions.
   - It should return data plus explicit metadata describing income variables,
     missing-code handling, household identifiers, and derived outputs.

4. Poverty flag orchestration PR
   - Add a wrapper only after poverty-line input requirements are explicit.
   - It should refuse computation unless line provenance is auditable.

5. Indicator-estimation PR
   - Produce stable long-format tables for income means, poverty proportions,
     quintile distributions, and optional scenario indicators using existing
     survey-design estimators.
   - Preserve full-data estimation with output-domain filtering when design
     domains are requested.

6. Scenario-contract PR
   - Formalize optional-bonus scenario naming, metadata, and interpretation.
   - Ensure scenario outputs never overwrite base income or base poverty flags.

7. User-documentation PR
   - Add a vignette or pkgdown article using synthetic or shareable data only.
   - State limitations and avoid official validation claims.

8. Validation PR
   - Add official validation only if official sources, scripts, and exact
     comparison evidence are available.
   - Keep validation evidence auditable and separate from presentation.

## Recommended First Implementation PR

The first Phase 7 implementation PR should be a contract-test PR, not a new
feature PR. It should add focused tests around the existing functions and
registries:

- `enemdu_build_variables()`;
- `enemdu_build_quintiles()`;
- `enemdu_validate_poverty_lines()`;
- `enemdu_get_poverty_line()`;
- `enemdu_build_poverty_flags()`;
- `enemdu_build_optional_bonuses()`;
- `enemdu_indicator_table()` for existing income and poverty registry rows.

That PR should not add official poverty-line values and should not change the
current income formulas.
