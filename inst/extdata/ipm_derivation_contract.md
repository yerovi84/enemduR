# IPM Derivation Architecture Contract

## Purpose

This file documents the current `enemduR` implementation contract for Ecuador
multidimensional poverty indicators in the `enemdu_2025_anual` profile.

The module name is IPM. TPM, TPEM, deprivation intensity, and M0/IPM are outputs
of the IPM module and must not be treated as separate methodologies.

The implementation provides local reproducibility evidence for ENEMDU December
2025 benchmark comparison. No institutional validation is claimed; any official
validation would require explicit authorization or confirmation by the relevant
official authority. Package outputs continue to report
`official_validation_status = "not_officially_validated"`.

## Scope

The current IPM implementation covers:

- the 4 Ecuador IPM dimensions;
- the 12 registered deprivation components;
- official indicator weights declared in `ipm_component_registry.csv`;
- component derivation for the `enemdu_2025_anual` profile;
- household-level aggregation of component evidence and person-level reporting;
- missing-critical diagnostics before scoring;
- `ipm_score`, `tpm`, `tpem`, deprivation intensity `A`, and M0/IPM;
- design-aware KPI estimation;
- published benchmark comparison for ENEMDU December 2025;
- complete-case reproducibility policy for non-evaluable component evidence.

The supporting machine-readable files are:

- `ipm_component_registry.csv`;
- `ipm_derivation_registry.csv`;
- `ipm_official_benchmarks.csv`.

## Out Of Scope

This contract does not change NBI methodology. It does not change income poverty
methodology. It does not modify survey estimators, benchmarks, weights,
thresholds, or official source files. It does not store or redistribute
microdata.

This contract does not claim institutional validation. Local benchmark
comparison evidence is useful for reproducibility review, but it is not an
institutional certification, INEC endorsement, or official production-system
status.

## Official IPM Structure

Ecuador's multidimensional poverty structure uses 4 dimensions and 12
deprivation indicators.

| Dimension | Dimension weight | Indicator | Indicator weight |
|---|---:|---|---:|
| Education | 0.25 | Inasistencia a educacion basica y bachillerato | 0.0833333333333333 |
| Education | 0.25 | No acceso a educacion superior por razones economicas | 0.0833333333333333 |
| Education | 0.25 | Logro educativo incompleto | 0.0833333333333333 |
| Work and social security | 0.25 | Empleo infantil y adolescente | 0.0833333333333333 |
| Work and social security | 0.25 | Desempleo o empleo inadecuado | 0.0833333333333333 |
| Work and social security | 0.25 | No contribucion al sistema de pensiones | 0.0833333333333333 |
| Health water and food | 0.25 | Pobreza extrema por ingresos | 0.125 |
| Health water and food | 0.25 | Sin servicio de agua por red publica | 0.125 |
| Habitat housing and healthy environment | 0.25 | Hacinamiento | 0.0625 |
| Habitat housing and healthy environment | 0.25 | Deficit habitacional | 0.0625 |
| Habitat housing and healthy environment | 0.25 | Sin saneamiento de excretas | 0.0625 |
| Habitat housing and healthy environment | 0.25 | Sin servicio de recoleccion de basura | 0.0625 |

The indicator weights sum to `1`.

## Identification And Analysis Levels

IPM component derivation starts from person-level or household-level evidence.
Components are aggregated at household level. The household component value is
then repeated to all persons in that household, because official reporting is
person-level.

The component builder preserves row count and row order. It does not drop rows.
Exclusions for reproducibility are handled later by
`enemdu_run_ipm_reproducibility()` when an explicit missing-component policy is
selected.

## Score And Cutoffs

`enemdu_build_ipm_flags()` builds the weighted deprivation score and final flags
from the 12 component columns.

- `ipm_score` is the weighted deprivation score.
- `tpm` is `1` when `ipm_score >= 4 / 12`.
- `tpem` is `1` when `ipm_score >= 6 / 12`.
- `A` is the average deprivation intensity among multidimensionally poor
  persons.
- M0/IPM is `TPM * A`.

The TPM cutoff is therefore `4 / 12`, equivalent to
`0.3333333333333333` or one third of total deprivation weight. The TPEM cutoff
is `6 / 12`, equivalent to `0.5` or one half of total deprivation weight.

## Missing-Critical Policy

The component builder records missing-critical evidence without dropping rows.
Diagnostics include component-level missing-critical person rows and households.

For reproducibility runs, `enemdu_run_ipm_reproducibility()` supports an
explicit complete-case policy. In complete-case mode, rows with incomplete IPM
component or score evidence are excluded before KPI estimation and benchmark
comparison. The result reports:

- total, complete, and excluded rows;
- total, complete, and excluded weighted population;
- domain-level complete and incomplete diagnostics when `by` is supplied;
- `official_validation_status = "not_officially_validated"`.

Rows are not dropped unless the reproducibility workflow is called with an
explicit missing-component policy that permits exclusion.

## Profile-Specific Component Policies

The `enemdu_2025_anual` profile implements official-syntax or official-like
rules where the required ENEMDU variables are available.

### i03 incomplete educational attainment

For `ipm_i03_logro_educativo_incompleto`, the official profile uses
`schooling_unknown_policy = "official_recode_to_zero"`.

This mirrors the official syntax behavior where unassigned schooling is
recoded to zero after the schooling-years construction. Diagnostics preserve
traceability through counts for unmatched observed schooling, missing schooling
inputs, and conversion to zero. This policy is profile-specific and does not
imply institutional validation.

### i04 child and adolescent employment

For `ipm_i04_empleo_infantil_adolescente`, the rule status can be:

- `official_syntax_rule` when `pea` is available and used as the official PEA
  gate for hours;
- `official_like_with_derived_pea` when `pea` is missing but `empleo` and
  `desempleo` are available;
- `official_like_without_pea_gate` when `pea` is not available and cannot be
  derived, but the child/adolescent labor source variables are sufficient for
  an official-like decision;
- `proxy_fallback_not_official_syntax` only when the official-like source set is
  not available.

The derived PEA used by i04 is internal to the IPM component. It is not a public
labor-market API and does not reconstruct the full ENEMDU labor module.

### i06 pension contribution

For `ipm_i06_no_contribucion_pensiones`, the rule status can be:

- `official_syntax_rule` when `pet`, `pei`, and `desem` are available;
- `official_like_with_derived_labor_status` when those compact official status
  variables are missing but labor status can be derived internally for this
  component.

In the official-like path:

- PET is derived from age as ages 15 to 98;
- PEA is derived from `empleo` or `desempleo` membership flags;
- PEI is derived as PET minus PEA;
- `desem` is derived from `desempleo`;
- `empleo` and `desempleo` are treated as `1`/`NA` membership flags for this
  profile;
- consistency against `condact` is reported diagnostically through
  `labor_status_validation` and mismatch counts.

The i06 derivation remains branch-specific. Benefits such as retirement pension
or bonuses are required only in the branches where they are needed to decide
the component. Observed contribution evidence for occupied persons is not
overwritten by structurally irrelevant benefit missingness.

## December 2025 Local Reproducibility Evidence

The post-PR #40 local ENEMDU December 2025 smoke test produced strong local
reproducibility evidence for TPM, TPEM, and IPM benchmark comparison.

Residual non-evaluable component evidence was limited to
`ipm_i07_pobreza_extrema_ingresos_flag`, with 133 person rows and 62
households. Components i04 and i06 had zero residual missing values in that
run.

All national, urban, and rural TPM/TPEM/IPM comparisons were within tolerance
or matched published rounding. The retained evidence is documented in
`official_ipm_reproducibility_evidence_december_2025.md`.

## Non-Official Validation Statement

`enemduR` provides local reproducibility evidence and benchmark comparison
infrastructure against published benchmarks. This documentation file does not implement IPM; implementation lives in the package R code and registries. The
workflow does not claim official institutional validation by INEC or any other
official authority. Any official validation would require explicit
authorization or confirmation by the relevant official authority.

Use the terms "local reproducibility evidence", "benchmark comparison",
"official-like implementation", and "not officially validated" when describing
this workflow.
