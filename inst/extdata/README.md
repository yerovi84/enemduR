# extdata for enemduR

This directory stores metadata registries and auxiliary catalogs used by the package.

## Current contents

- `survey_registry.csv`: survey-level structural metadata for monthly, quarterly and annual ENEMDU workflows.
- `comparability_registry.csv`: alert registry for periods that must carry a comparability warning.
- `domain_registry.csv`: registry of ENEMDU design-domain levels by survey type.
- `domain_variable_registry.csv`: mapping between common variables and domain levels.
- `analysis_level_registry.csv`: registry of analytical levels used by indicators and outputs.
- `representativity_thresholds.csv`: thresholds used by the precision and representativity evaluation workflow.
- `variable_catalog.csv`: package-level variable catalog.
- `indicator_registry.csv`: general analytical indicator registry.
- `labor_indicator_registry.csv`: registry of labor indicators implemented by `enemdu_kpi_employment()`.
- `validation_registry.csv`: structural and substantive validation rule registry.
- `income_component_registry.csv`: registry of income-construction components.
- `missing_code_registry.csv`: registry of sentinel and no-response codes by analytical use.
- `value_range_registry.csv`: registry of valid or expected value ranges.
- `poverty_line_registry.csv`: auditable registry template for poverty and extreme-poverty lines.
- `optional_bonus_registry.csv`: registry of optional social-bonus components and scenario rules.
- `official_dictionary_core_registry.csv`: registry of official dictionary variables checked by survey type and period.

## Design rule

Files in this directory should be:

- human-readable,
- versionable,
- explicit,
- auditable,
- and editable without rewriting package logic.

## Labor indicators

`labor_indicator_registry.csv` documents the 32 labor indicators currently returned by `enemdu_kpi_employment()`.

The registry is intentionally documentary at this stage. It formalizes indicator identifiers, labels, estimator types, numerator or value flags, denominator universes, output scale and domain-scope policy.

The current labor implementation uses the consolidated ENEMDU condition-of-activity variable `condact`. It does not reconstruct labor status from raw questionnaire variables.

## Warning

The presence of a registry file here does not mean that all substantive rules are closed.

A registry may document one of three situations:

- an implemented and tested contract,
- a methodological contract pending broader validation,
- or a template requiring period-specific official information before calculation.

Rules that depend on official methodology, official dictionaries or published tabulations must remain traceable to their source or be explicitly marked as project-internal decisions.
