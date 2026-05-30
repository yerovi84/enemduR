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
- `labor_official_validation_contract.md`: official-validation contract for the initial labor-indicator block.
- `ipm_component_registry.csv`: registry of IPM dimensions, components, weights, and expected component columns.
- `ipm_derivation_contract.md`: technical contract for the implemented IPM/TPM workflow and the `enemdu_2025_anual` profile.
- `ipm_official_benchmarks.csv`: published IPM/TPM benchmark values for analytical comparison.
- `official_ipm_reproducibility_evidence_december_2025.md`: local IPM/TPM reproducibility evidence for ENEMDU December 2025 benchmark comparison.
- `package_hardening_report.md`: Phase 5 package-hardening report.
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

## IPM/TPM reproducibility

`ipm_derivation_contract.md` documents the current IPM/TPM implementation for
the ENEMDU 2025 annual profile. The workflow builds 12 components, aggregates
component evidence at household level, reports person-level flags, applies the
registered weights, and estimates TPM, TPEM, deprivation intensity, and IPM.

The December 2025 evidence file records local reproducibility evidence and
benchmark comparison results. No institutional validation is claimed; any
official validation would require explicit authorization or confirmation by the
relevant official authority. The workflow remains marked as
`not_officially_validated`.

## Official labor validation

`labor_official_validation_contract.md` documents the validation contract used to compare the initial labor-indicator block against official INEC labor-market tabulations.

The contract records:

- validated survey cuts;
- validated domains;
- accepted comparison tolerances;
- publication-format handling rules;
- official dash handling for zero counts;
- period-label normalization;
- the use of `dominio` for official city-domain validation;
- the `Santo Domingo` label alias used for annual province validation.

## Warning

The presence of a registry or contract file here does not mean that all substantive rules are closed.

A file may document one of three situations:

- an implemented and tested contract,
- a methodological contract pending broader validation,
- or a template requiring period-specific official information before calculation.

Rules that depend on official methodology, official dictionaries or published tabulations must remain traceable to their source or be explicitly marked as project-internal decisions.
