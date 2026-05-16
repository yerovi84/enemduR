# NBI Contract

## Purpose

This document defines the initial `enemduR` contract for poverty by unsatisfied
basic needs (NBI). The module is designed for reproducible analytical workflows
using final NBI component variables that already exist in the microdata.

## Scope

This phase builds and validates NBI outputs from final component variables:

- `comp1`
- `comp2`
- `comp3`
- `comp4`
- `comp5`

The package creates:

- `knbi`: count of observed NBI deprivations;
- `nbi`: NBI poverty flag;
- `xnbi`: extreme NBI poverty flag.

## Out of Scope

This phase does not reconstruct NBI from raw questionnaire variables or `pXX`
questions. It does not implement TPM, TPEM, intensity, or IPM. It does not add
official NBI benchmark values and does not claim official validation.

Reconstruction from raw ENEMDU variables requires the complete official syntax
and an auditable recoding contract before implementation.

## Expected Components

The expected component variables are final binary fields. A value of `1`
represents an observed NBI deprivation. A value of `0` represents no observed
deprivation for that component.

## `knbi` Rule

`knbi` is the count of observed NBI deprivations across the final components.
If any component is missing for a row, `knbi` is set to `NA` because the row is
not evaluable under this contract.

`knbi = 0` is a valid value and must not be converted to `NA`.

## `nbi` Rule

`nbi = 1` when `knbi >= 1`.

`nbi = 0` when `knbi == 0`.

`nbi = NA` when `knbi` is missing.

## `xnbi` Rule

`xnbi = 1` when `knbi >= 2`.

`xnbi = 0` when `knbi < 2`.

`xnbi = NA` when `knbi` is missing.

## Missing Data Policy

Rows with any missing component are treated as not evaluable. The package
preserves this uncertainty by setting `knbi`, `nbi`, and `xnbi` to missing for
those rows.

## Reproducibility and Official Validation

The local reproducibility workflow can estimate NBI indicators and compare them
against benchmark rows when official benchmarks are supplied. This comparison is
analytical readiness work. It is not institutional validation by INEC.

Official validation requires published benchmark values, exact source
documentation, documented survey design and domain treatment, and reviewed
comparison results.

## Future Relation to TPM and IPM

NBI is separate from Ecuador's multidimensional poverty methodology. TPM, TPEM,
intensity, and IPM remain out of scope for this NBI phase and should be handled
in a later 7-TPM phase with a separate contract.

## Benchmark Requirement for Phase 7-NBI-C

Before claiming reproducibility against official NBI outputs, Phase 7-NBI-C
must add audited benchmark rows to `official_nbi_benchmarks.csv` or supply an
external benchmark registry with equivalent metadata. No benchmark values are
invented in this phase.
