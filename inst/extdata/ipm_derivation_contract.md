# IPM Derivation Architecture Contract

## Purpose

This file establishes a reproducible internal implementation contract for the
future Ecuador multidimensional poverty module in `enemduR`.

The module name is IPM. TPM is one output of the IPM module and must not be
treated as the whole module.

This file does not implement IPM. It documents the official structure that
future work must preserve before adding calculation functions.

## Scope

This contract defines the initial declarative architecture for:

- the 4 official Ecuador IPM dimensions;
- the 12 official deprivation indicators;
- the analytical weights attached to each indicator;
- the expected applicable populations and identification levels;
- the expected final analytical variables;
- the current non-operational derivation status;
- the validation plan for later real-data reproducibility work.

The supporting machine-readable files are:

- `ipm_component_registry.csv`;
- `ipm_derivation_registry.csv`.

## Out Of Scope

This file does not implement IPM.

This file does not create:

- `enemdu_build_ipm_components()`;
- `enemdu_build_ipm_flags()`;
- `enemdu_kpi_ipm()`;
- `enemdu_official_ipm_benchmarks()`;
- `enemdu_compare_official_ipm()`;
- `enemdu_run_ipm_reproducibility()`.

This file does not change NBI methodology. It does not change income poverty
methodology. It does not reconstruct `condact` from raw questionnaire
variables. It does not compute poverty indicators without explicit auditable
poverty lines. It does not add official benchmarks or claim official
institutional validation.

## Official IPM Structure

Ecuador's official multidimensional poverty structure uses 4 dimensions and 12
indicators.

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

## Identification And Analysis Levels

Most IPM deprivations are identified at household level or by person-level
conditions that are then assigned to the household. Official reporting is
person-level: people inherit the household IPM status for estimation.

The registries therefore separate:

- `identification_level`: where the deprivation is identified;
- `analysis_level`: where the final indicator is estimated.

Future implementation must distinguish a non-applicable population from a
missing value. A person outside an indicator universe should not be silently
treated as deprived.

## Weighting Structure

Each dimension has weight `0.25`.

The first 6 indicators have weight `0.0833333333333333`.

The next 2 indicators have weight `0.125`.

The final 4 indicators have weight `0.0625`.

The indicator weights sum to `1`.

## Cutoff Structure

The TPM cutoff is `0.3333333333333333`.

A household is multidimensionally poor when its weighted deprivation score is
greater than or equal to `0.3333333333333333`.

The TPEM cutoff is `0.5`.

A household is extremely multidimensionally poor when its weighted deprivation
score is greater than or equal to `0.5`.

## Expected Output Variables

Future implementation should produce:

- `ipm_score`: weighted deprivation score for each household repeated to the
  analysis unit;
- `tpm`: binary multidimensional poverty flag based on the TPM cutoff;
- `tpem`: binary extreme multidimensional poverty flag based on the TPEM
  cutoff;
- `A`: deprivation intensity among the multidimensionally poor;
- `ipm`: multidimensional poverty index calculated as `TPM * A`.

## Difference Between NBI And IPM

NBI uses 5 unsatisfied-basic-needs components and identifies poverty when a
household has at least one NBI deprivation. The current NBI workflow derives
`comp1` through `comp5`, then builds `knbi`, `nbi`, and `xnbi`.

IPM uses 12 weighted deprivation indicators across 4 dimensions. TPM depends on
a weighted score cutoff rather than a simple count of basic needs. IPM also
includes extreme income poverty as one deprivation input. Therefore IPM must
not reuse NBI water sanitation housing or education rules without explicit
methodological verification.

## Implementation Roadmap

Future work should proceed in small auditable phases:

1. Validate the IPM registries against official methodological documentation.
2. Confirm source variables and coding rules for the `enemdu_2025_anual`
   profile.
3. Implement `enemdu_build_ipm_components()` as a component builder only after
   the rules are auditable.
4. Implement `enemdu_build_ipm_flags()` to compute `ipm_score`, `tpm`, `tpem`,
   `A`, and `ipm`.
5. Implement `enemdu_kpi_ipm()` using the package survey-design estimators.
6. Add `enemdu_official_ipm_benchmarks()` only when published benchmark values
   are documented.
7. Add `enemdu_compare_official_ipm()` with explicit tolerances and comparison
   evidence.
8. Add `enemdu_run_ipm_reproducibility()` only after all source and benchmark
   contracts are closed.

## Methodological Risks

Main risks for later implementation are:

- confusing TPM with the full IPM module;
- treating non-applicable populations as missing or as deprived;
- reusing NBI water and sanitation rules without IPM-specific verification;
- implementing labor indicators without respecting consolidated ENEMDU labor
  derivations;
- reconstructing `condact` from raw questionnaire variables;
- computing the extreme-income-poverty component without explicit poverty
  lines;
- claiming official validation without retained comparison evidence.

## Validation Plan

Later validation must document:

- the official methodological source used for each indicator;
- the ENEMDU period and survey type;
- source variables and derived inputs;
- applicable populations and non-applicability treatment;
- survey design variables and expansion weight;
- national and domain estimates;
- published official benchmark values;
- package estimates;
- absolute and relative differences;
- tolerance rules and pass or fail status.

Real-data reproducibility must be validated later against published benchmarks.

## Non-Official Validation Statement

This file does not claim official institutional validation.

This file establishes a reproducible internal implementation contract.

The registries created with this contract are non-operational until future code
implements and validates the derivation rules.

Official validation must not be claimed until real-data reproducibility is
tested against published official benchmarks and the evidence is retained.
