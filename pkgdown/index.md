![Big Y Productions analytics brand seal](man/figures/big-y-logo.png)

# enemduR

**Reproducible analytical infrastructure for Ecuador ENEMDU microdata.**

Built by Alejandro Yerovi | Big Y Productions

Survey Statistics | Policy Analytics | Reporting Automation | R

[Reference](reference/index.html) | [Articles](articles/index.html) | [News](news/index.html) | [GitHub](https://github.com/yerovi84/enemduR) | [v0.1.0-alpha](https://github.com/yerovi84/enemduR/releases/tag/v0.1.0-alpha)

`enemduR` is an R package for reproducible analytical work with Ecuador's ENEMDU microdata. It provides infrastructure for reading, validation, derivation, survey-design-aware estimation, representativity assessment, benchmark comparison workflows, IPM/TPM local reproducibility evidence, and Quarto-ready analytical outputs.

The package is an analytical engine. It is not a dashboard, not a replacement for official statistical production systems, and not an official endorsement layer. Its role is to make technical ENEMDU workflows more reproducible, auditable, and easier to review.

## Portfolio Signal

This public package demonstrates a professional analytical stack built around official microdata workflows: R package engineering, complex survey statistics, metadata contracts, validation discipline, and reporting automation.

## Core Capabilities

| Capability | Package focus |
|---|---|
| Survey statistics | Complex-design estimators, domain handling, and precision-aware analytical outputs. |
| Policy analytics | Metadata contracts and documented workflows for reproducible ENEMDU analysis. |
| Reporting automation | Stable long-format tibbles designed for downstream Quarto and reporting systems. |
| R package engineering | Tests, CI, pkgdown, generated reference pages, governance files, and release metadata. |

## Current Scope

- Reading support for `.sav`, `.dta`, and `.csv` ENEMDU files.
- Complex survey design declaration using ENEMDU design variables.
- Survey estimators for totals, means, proportions, and tabulations.
- Representativity and precision-evaluation infrastructure.
- Labor indicators based on the consolidated `condact` variable.
- Metadata registries for variables, indicators, missing codes, domains, poverty lines, optional bonuses, and official dictionaries.
- Income, quintile, poverty-flag, household-profile, optional-bonus, and social-bonus infrastructure under explicit contract rules.
- IPM/TPM component, flag, KPI, and benchmark-comparison infrastructure for the ENEMDU 2025 annual profile, with outputs marked as not officially validated.

## Methodological Guardrails

- Poverty indicators are not computed without explicit and auditable poverty lines.
- Sentinel values are not globally recoded unless a registry authorizes the rule.
- Weighted population counts are kept distinct from effective sample size.
- Presentation layers consume analytical outputs; they do not govern computation.
- Benchmark comparisons are local reproducibility evidence; no institutional validation is claimed.
- Any official validation would require explicit authorization or confirmation by the relevant official authority.

## Start Here

- Browse the [function reference](reference/index.html).
- Read the [labor indicators vignette](articles/labor-indicators.html).
- Review the [release notes](news/index.html).
- Inspect the source repository on [GitHub](https://github.com/yerovi84/enemduR).
