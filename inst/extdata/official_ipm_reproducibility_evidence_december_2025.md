# IPM Reproducibility Evidence - ENEMDU December 2025

## Purpose

This document records local reproducibility evidence for the ENEMDU December
2025 IPM/TPM workflow implemented in `enemduR`.

The workflow builds the 12 IPM components for the `enemdu_2025_anual` profile,
constructs the weighted deprivation score and TPM/TPEM/IPM flags, estimates the
published indicators with the ENEMDU survey design, and compares the local
estimates against published benchmark values.

This evidence is intended to document local reproducibility against published
benchmark values. No institutional validation is claimed; any official
validation would require explicit authorization or confirmation by the relevant
official authority.

## Scope

This evidence covers:

- ENEMDU December 2025.
- The ENEMDU 2025 annual IPM profile.
- Multidimensional poverty rate (`TPM`).
- Extreme multidimensional poverty rate (`TPEM`).
- Adjusted multidimensional poverty index (`IPM` or M0).
- National, urban, and rural domains.
- Person-level estimates using ENEMDU expansion weights and survey design
  variables.

This evidence does not cover:

- storage or redistribution of official microdata;
- institutional validation by INEC or any other official authority;
- certification or endorsement as an official production system;
- changes to NBI, income-poverty, labor-helper, survey-estimator, or benchmark
  methodology.

## Methodological Summary

The IPM workflow uses 12 deprivation components and the official weights
declared in `ipm_component_registry.csv`.

Components are identified from person-level or household-level evidence,
aggregated at household level, and repeated to all persons in the household.

The score and cutoffs are:

- `ipm_score`: weighted deprivation score across the 12 components;
- `TPM`: `ipm_score >= 4 / 12`;
- `TPEM`: `ipm_score >= 6 / 12`;
- `A`: average deprivation intensity among multidimensionally poor persons;
- `IPM`: `TPM * A`.

Missing-critical households are diagnosed before score calculation. The
complete-case reproducibility policy excludes rows with incomplete component
evidence before KPI estimation and benchmark comparison.

## Profile-Specific Decisions

### i03 incomplete educational attainment

For `profile = "enemdu_2025_anual"`,
`ipm_i03_logro_educativo_incompleto_flag` follows the profile-specific
official schooling recode policy:

- `schooling_unknown_policy = "official_recode_to_zero"`.

This mirrors the official syntax behavior where unassigned schooling is recoded
to zero after constructing schooling years. Diagnostics preserve traceability
for unmatched observed schooling, missing inputs, and conversions to zero.

### i04 child and adolescent employment

`ipm_i04_empleo_infantil_adolescente_flag` uses:

- `official_syntax_rule` if `pea` is available;
- `official_like_with_derived_pea` if `pea` is missing but `empleo` and
  `desempleo` are available;
- `official_like_without_pea_gate` only when PEA is unavailable and cannot be
  derived but child/adolescent labor evidence is otherwise sufficient;
- `proxy_fallback_not_official_syntax` only when the official-like source set
  is unavailable.

The derived PEA is internal to the i04 IPM component. It does not create a
public labor-market API and does not reconstruct the full ENEMDU labor module.

### i06 pension contribution

`ipm_i06_no_contribucion_pensiones_flag` uses:

- `official_syntax_rule` if `pet`, `pei`, and `desem` are available;
- `official_like_with_derived_labor_status` if PET, PEA, PEI, and `desem` can
  be derived internally for the component.

In the official-like path:

- PET is derived from age;
- PEA is derived from `empleo` and `desempleo` membership flags;
- PEI is derived as PET minus PEA;
- `desem` is derived from `desempleo`;
- `empleo` and `desempleo` are treated as `1`/`NA` membership flags in this
  profile;
- consistency against `condact` is reported diagnostically.

The December 2025 run reported
`labor_status_validation = "consistent_with_condact"` for i06.

## Component Missingness

Post-PR #40 component missing counts:

| Component | NA rows |
|---|---:|
| `ipm_i01_inasistencia_basica_bach_flag` | 0 |
| `ipm_i02_no_acceso_superior_economico_flag` | 0 |
| `ipm_i03_logro_educativo_incompleto_flag` | 0 |
| `ipm_i04_empleo_infantil_adolescente_flag` | 0 |
| `ipm_i05_desempleo_empleo_inadecuado_flag` | 0 |
| `ipm_i06_no_contribucion_pensiones_flag` | 0 |
| `ipm_i07_pobreza_extrema_ingresos_flag` | 133 |
| `ipm_i08_sin_agua_red_publica_flag` | 0 |
| `ipm_i09_hacinamiento_flag` | 0 |
| `ipm_i10_deficit_habitacional_flag` | 0 |
| `ipm_i11_sin_saneamiento_excretas_flag` | 0 |
| `ipm_i12_sin_recoleccion_basura_flag` | 0 |

Critical missing diagnostics:

| Diagnostic | Value |
|---|---:|
| critical missing households | 62 |
| critical missing person rows | 133 |

The residual non-evaluable evidence came from
`ipm_i07_pobreza_extrema_ingresos_flag`, where the selected extreme-income
poverty flag was unavailable for those rows.

## Complete-Case Diagnostics

Complete-case reproducibility diagnostics:

| Diagnostic | Value |
|---|---:|
| rows_total | 27808 |
| rows_complete | 27675 |
| rows_excluded | 133 |
| weighted_excluded | 80364.43 |
| share_weighted_excluded | 0.004233338 |

The complete-case exclusion is explicit and auditable. Component values are not
imputed, and missing component evidence is not converted to deprivation or
non-deprivation.

## Benchmark Comparison

Final local benchmark comparison against published values:

| Indicator | Domain | Local estimate | Published benchmark | Difference |
|---|---|---:|---:|---:|
| TPM | Nacional | 41.5 | 41.7 | -0.224 pp |
| TPM | Urbano | 29.7 | 29.9 | -0.158 pp |
| TPM | Rural | 66.6 | 66.9 | -0.261 pp |
| TPEM | Nacional | 18.1 | 18.1 | -0.0124 pp |
| TPEM | Urbano | 10.3 | 10.3 | 0.00944 pp |
| TPEM | Rural | 34.8 | 34.8 | -0.0315 pp |
| IPM | Nacional | 20.5 | 20.5 | -0.0459 |
| IPM | Urbano | 13.1 | 13.2 | -0.0746 |
| IPM | Rural | 36.2 | 36.3 | -0.129 |

All comparisons were within tolerance or matched reported rounding.

## Validation Status

All IPM reproducibility outputs retain:

```text
official_validation_status = "not_officially_validated"
```

This evidence should be described as local reproducibility evidence and
benchmark comparison. It should not be described as an INEC endorsement,
institutional certification, or guarantee. Any official validation would
require explicit authorization or confirmation by the relevant official
authority.
