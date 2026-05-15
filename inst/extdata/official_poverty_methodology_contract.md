# Official Poverty Methodology Contract

## Scope

This document records the methodology contracts required before `enemduR`
implements broader poverty wrappers or official-replication workflows. It is a
documentation and planning artifact only.

The contract covers:

- income poverty;
- extreme income poverty;
- NBI poverty;
- extreme NBI poverty;
- multidimensional poverty rate (`TPM`);
- extreme multidimensional poverty rate (`TPEM`);
- multidimensional poverty index (`IPM`);
- poverty-line handling through published official lines or audited
  IPC-derived lines;
- the current conservative package policy for non-positive income values;
- future implementation and validation sequencing.

This document does not implement poverty indicators, add official poverty-line
registry values, change package functions, or claim official validation.

## Source Status

The methodological facts in this document are intake facts for future package
work. They define what future implementation must preserve, document, test, and
validate before official poverty outputs are claimed.

The package must not invent ENEMDU variables, official poverty lines, official
methodological rules, or official validation results. Official validation must
not be claimed until package outputs are compared against official published
outputs and the comparison evidence is retained.

## Indicator Families

Future poverty work must keep these indicator families distinct:

- income poverty;
- extreme income poverty;
- NBI poverty;
- extreme NBI poverty;
- multidimensional poverty rate (`TPM`);
- extreme multidimensional poverty rate (`TPEM`);
- multidimensional poverty index (`IPM`).

Each output must declare its poverty family, unit of identification, unit of
analysis or reporting, weight, domain, poverty-line or deprivation source
status, and validation status.

## Income Poverty Contract

Income poverty compares household per-capita income against an official poverty
line.

Extreme income poverty compares household per-capita income against an official
extreme poverty line.

The poverty and extreme-poverty lines originate from the ECV 5th round
consumption poverty lines. The base values are:

| Line type | 2006 base value |
|---|---:|
| Poverty line | USD 56.64 |
| Extreme poverty line | USD 31.92 |

The lines are updated using CPI/IPC.

For December 2025, the official published monthly per-capita lines are:

| Line type | December 2025 official published value |
|---|---:|
| Poverty line | USD 92.40 |
| Extreme poverty line | USD 52.07 |

These values document methodology requirements for future implementation. They
must not be read as a package poverty-line registry or as evidence that package
poverty estimates have been officially validated.

## Poverty-Line Contract

Future package work may support two poverty-line input strategies.

1. Published official lines

   Published official poverty and extreme-poverty lines may be supplied by the
   user or by an audited package registry in a future phase. The source, period,
   line type, currency, value, and source status must be explicit and auditable.

2. IPC-derived audited lines

   IPC-derived lines may be supported only after the CPI/IPC source, base-year
   handling, previous-month reference rule, rounding rule, and validation
   against published official lines are documented and tested. The package must
   not silently derive lines from an undocumented CPI series.

The package must not invent official poverty-line values. If poverty lines are
missing, pending review, undocumented, or not auditable, poverty computation
must fail or return a documented non-computable result according to the future
function contract.

## Current Non-Positive Income Policy

Current package behavior treats zero or non-positive derived household
per-capita income as unclassified for income quintiles and poverty flags.

This is a conservative package contract, not a claim about all possible
official methodologies. Future official-replication behavior may be added only
as an explicit mode after methodological review, tests, documentation, and
validation against official published outputs.

Future wrappers must not silently change the current non-positive income
policy.

## NBI Poverty Contract

NBI poverty is based on household-level deprivations. A person is poor by NBI
when they belong to a household with at least one unsatisfied basic need.

The standard NBI dimensions or components are:

1. housing quality / physical housing conditions;
2. overcrowding;
3. basic services;
4. access to education / school attendance;
5. household economic capacity / economic dependency.

NBI is identified at household level. When reported at person level, household
deprivation status is inherited by household members.

Future implementation must document whether NBI inputs are built by the package
or supplied by the user, and must validate the consistency of household-level
flags before estimating NBI indicators.

## Extreme NBI Poverty Contract

Extreme NBI poverty is represented in the current analytical context by `xnbi`.
The variable `knbi` is the count of NBI deprivations.

The current consistency contract is:

- `nbi = 1` when `knbi >= 1`;
- `nbi = 0` when `knbi == 0`;
- `xnbi = 1` when `knbi >= 2`;
- `xnbi = 0` when `knbi < 2`.

Future code must validate consistency between `knbi`, `nbi`, and `xnbi` before
estimating NBI or extreme NBI indicators. Inconsistent records must be reported
through an auditable diagnostic contract before any official validation claim is
made.

## Multidimensional Poverty Contract

Ecuador's multidimensional poverty methodology follows the Alkire-Foster
approach. It uses 4 dimensions and 12 indicators.

A person is multidimensionally poor when the household has weighted
deprivations greater than or equal to one third of indicators or weights:

- `TPM`: `K >= 33.3%`.

A person is extremely multidimensionally poor when the household has weighted
deprivations greater than or equal to one half:

- `TPEM`: `K >= 50%`.

The multidimensional poverty index is computed as:

- `IPM = TPM x intensity`.

One multidimensional deprivation indicator is extreme income poverty. Therefore,
future implementation must compute or receive an explicit, auditable extreme
income poverty flag before computing `TPM`, `TPEM`, intensity, or `IPM`.

## Unit-of-Identification and Reporting Contract

Future poverty outputs must declare both how poverty is identified and how it is
reported.

| Poverty family | Unit of identification | Reporting contract |
|---|---|---|
| Income poverty | Household per-capita income repeated to persons | Persons |
| NBI poverty | Household | Persons or households depending on estimator; official ENEMDU poverty reporting commonly uses persons |
| Extreme NBI poverty | Household deprivation count and flags | Persons or households depending on estimator; official ENEMDU poverty reporting commonly uses persons |
| Multidimensional poverty | Household | Persons |

Aggregated outputs must declare:

- unit of identification;
- unit of analysis or reporting;
- survey weight;
- domain;
- source status;
- validation status.

## Implementation Sequence

Future implementation must proceed in small, auditable phases:

1. Preserve existing income derivation contracts.
2. Document or supply audited poverty lines.
3. Build or validate income poverty flags.
4. Build or validate NBI deprivation counts and flags.
5. Build or validate extreme NBI flags from `knbi`.
6. Build or validate multidimensional deprivation indicators.
7. Compute the weighted deprivation score.
8. Compute `TPM`, `TPEM`, intensity, and `IPM`.
9. Estimate indicators with survey-design-aware estimators.
10. Compare against official tabulated results before claiming official
    validation.

## Validation Requirements

Future official validation must be evidence based. At minimum, validation must
document:

- the source of each poverty line or deprivation input;
- the period and survey type;
- whether lines are published official values or IPC-derived audited values;
- the unit of identification and reporting;
- the survey design and weight;
- the estimation domain;
- the official tabulated result used for comparison;
- the package result;
- the absolute and relative difference;
- whether the comparison is within an explicit tolerance.

No official validation claim is allowed until the validation script, source
inputs, comparison outputs, and tolerance decision are retained.

## Out of Scope for This Phase

This phase does not include:

- functional implementation;
- official-line data registry values;
- automatic CPI/IPC ingestion;
- official validation claims;
- changes to R functions;
- changes to the package public API;
- changes to poverty, income, NBI, or multidimensional indicator definitions;
- changes to tests unless a future documentation-consistency pattern requires
  them.
