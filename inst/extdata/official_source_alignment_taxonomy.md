# Official-source alignment taxonomy

## Purpose

This taxonomy defines cautious, evidence-based labels for describing how
`enemduR` workflows relate to public official sources, methodological
documentation, syntax files, dictionaries, published tabulations, and local
benchmark comparisons.

The goal is to make public and technical documentation more precise without
overstating the institutional status of the package.

## Why this taxonomy exists

`enemduR` is an analytical infrastructure package for reproducible ENEMDU
microdata workflows. Some implemented workflows are traceable to public INEC
materials, public syntax, official dictionaries, and published benchmark
tabulations. Other workflows are package-level scaffolds, templates, or local
reproducibility aids that require user-supplied official public inputs.

This taxonomy helps distinguish:

- public official source traceability;
- implementation alignment with public syntax;
- implementation alignment with public methodology;
- comparison against published official benchmarks;
- documented local reproducibility evidence;
- and formal institutional validation.

## What this taxonomy does not claim

These labels describe documentation traceability, implementation alignment,
and reproducibility evidence. They do not certify the package as an official
statistical production system and do not imply official institutional
validation.

The package, its code, and its outputs should not be described as officially
validated, certified, endorsed, homologated, or approved by INEC unless the
relevant official authority explicitly provides that status and the evidence is
documented.

## Recommended status labels

### `official_source_documented`

Use when an indicator, variable, input, benchmark, or methodological concept is
traceable to a public official source such as INEC methodological sheets,
public syntax files, dictionaries, published tables, or official public
documentation.

### `official_syntax_aligned`

Use when the implementation follows public INEC syntax closely enough to
document alignment. This does not mean that INEC reviewed or approved the R
implementation.

### `official_methodology_aligned`

Use when the implementation follows the public methodological definition
documented by INEC or another official public source. This does not imply
official validation.

### `official_benchmark_compared`

Use when package outputs are compared against published official tabulations
or benchmarks.

### `official_benchmark_reproduced`

Use only when package outputs reproduce published official benchmarks within
an explicit tolerance or matched rounding policy documented in the project.

### `official_like_with_documented_deviation`

Use when the package implements a traceable official-like path but includes a
documented deviation, proxy, derived path, or implementation limitation.

### `local_reproducibility_evidence`

Use when the package has local evidence from user-supplied official public
inputs, scripts, tests, or benchmark comparisons.

### `not_officially_validated`

Use when there is no formal institutional confirmation, certification,
endorsement, or homologation from INEC or the relevant official authority.

## Interpretation rules

- Use the strongest label only where the repository documents the supporting
  evidence.
- Do not infer evidence from naming, intent, or similarity alone.
- Treat benchmark comparison as analytical evidence, not institutional
  approval.
- Treat reproduced benchmarks as reproduced only within the documented
  tolerance, rounding policy, inputs, and workflow scope.
- Keep local reproducibility evidence separate from official institutional
  validation.
- Preserve `not_officially_validated` unless formal institutional confirmation
  has been explicitly obtained and documented.
- When evidence is incomplete or profile-specific, state the scope of the
  evidence instead of generalizing to all periods, domains, or indicators.

## Indicator-family examples

### Labor indicators

Labor indicators may be described as official-source-documented or
official-syntax-aligned when supported by public syntax and tabulation
comparison. The current labor module remains based on the consolidated
`condact` variable and does not fully reconstruct labor status from raw
questionnaire variables.

Use benchmark-comparison language only where the relevant public tabulations,
mapped indicators, domains, tolerances, and rounding policies are documented.

### Poverty and extreme poverty

Poverty and extreme-poverty workflows may be described as
official-source-documented and benchmark-compared when explicit auditable
poverty lines and public benchmark comparisons are used.

Do not compute or describe poverty indicators without explicit poverty-line
contracts and source notes.

### NBI

NBI workflows may be described as official-methodology-aligned or
benchmark-compared only where the implemented profile and comparison evidence
are documented.

If component evidence, source joins, profile restrictions, or benchmark rows
are incomplete, state the limitation instead of assigning a stronger status.

### IPM/TPM

IPM/TPM workflows may be described as official-methodology-aligned and
benchmark-reproduced only where component logic, weights, complete-case policy,
and benchmark comparisons are documented.

Preserve `not_officially_validated` unless formal institutional validation is
explicitly documented.

## Governance rules

- Do not invent evidence for a specific indicator family.
- If evidence is not present in the repository, state that the status is
  available only where documented.
- Do not change analytical logic, statistical methods, benchmark values, or
  indicator definitions to fit a label.
- Do not add official PDFs, ZIP files, or restricted microdata to the package.
- Keep official-source alignment documentation separate from institutional
  validation claims.
- Use machine-readable status fields only when the project needs that level of
  governance and the supporting evidence is explicit.

## Recommended public wording

- "official-source-documented indicators"
- "official-syntax-aligned implementation"
- "official-methodology-aligned indicators"
- "benchmark comparison against published official tabulations"
- "locally reproduced against official public benchmarks"
- "not officially validated"

## Prohibited wording

- "officially homologated indicators"
- "INEC-validated package"
- "officially certified outputs"
- "official production system"
- "official INEC implementation"
- "institutionally endorsed"
