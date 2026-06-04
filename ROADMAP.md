# enemduR Roadmap

`enemduR` is an R package for reproducible analytical infrastructure using
Ecuador ENEMDU microdata. The package is an analytical engine for reading,
validation, derivation, survey-design-aware estimation, representativity
assessment, official-reference comparison workflows, and downstream Quarto-ready
analytical outputs.

## Current State

`enemduR` 0.1.1 is a documentation-only patch release following the first
stable portfolio release. It strengthens official-source alignment
documentation while keeping the package suitable for public portfolio review
as an analytical infrastructure project. The repository includes the core
package structure, user documentation, generated manual pages, portfolio
vignettes, GitHub Actions for R package checks, a published pkgdown
documentation site, and lightweight agent governance.

The v0.1.1 codebase includes implemented infrastructure for labor, income,
poverty, NBI, and IPM/TPM workflows. The package remains an analytical engine,
not a dashboard, institutional production environment, official validation
product, or official endorsement layer.

## Implemented Analytical Infrastructure

- Reading support for `.sav`, `.dta`, and `.csv` ENEMDU files.
- Metadata registries for variables, domains, indicators, missing codes,
  value ranges, poverty lines, optional bonuses, and official dictionaries.
- Survey-design-aware estimators for totals, means, proportions, and
  tabulations.
- Representativity and precision-evaluation infrastructure.
- Labor indicator infrastructure based on the consolidated `condact` variable.
- Official labor-tabulation parsing and comparison helpers.
- Income, household-profile, quintile, poverty-flag, optional-bonus, and
  social-bonus infrastructure under explicit contract rules.
- Poverty KPI and benchmark-comparison helpers driven by auditable poverty
  lines.
- NBI component, flag, KPI, benchmark-comparison, source-join, and local
  reproducibility infrastructure for the ENEMDU 2025 annual profile.
- IPM/TPM component, flag, KPI, benchmark-comparison, and local
  reproducibility infrastructure for the ENEMDU 2025 annual profile.
- Quarto-ready analytical outputs while keeping presentation separate from
  computation.
- Package hardening through README, NEWS, roxygen/manual pages, vignette,
  GitHub Actions, pkgdown publication, and agent guidance.

## Reproducibility Evidence

The repository retains local reproducibility evidence and comparison workflows
for December 2025 poverty, NBI, and IPM/TPM benchmarks. These comparisons are
evidence for local reproducibility under explicit inputs, contracts, and
benchmark rows; they are not official institutional validation.

Any official validation claim would require explicit authorization or
confirmation by the relevant official authority. Until then, outputs that
carry validation metadata should continue to preserve the
`not_officially_validated` status when that status applies.

## v0.1.1 Documentation-Only Patch

The v0.1.1 patch strengthens official-source alignment documentation. It
clarifies how public official sources, implementation alignment, official
benchmark comparison, local reproducibility evidence, and official
institutional validation should be interpreted.

This patch does not change calculations, registries, benchmarks, dependencies,
APIs, indicator definitions, statistical methods, or survey-design logic.

Future work may formalize more granular metadata status fields only when the
project needs machine-readable governance and the supporting evidence is
explicit. Any such governance work should preserve the rule that benchmark
comparison and local reproducibility evidence do not imply official
institutional validation.

## Post-v0.1.1 Hardening And Portfolio Maintenance

Future work after v0.1.1 should keep the public surface polished without
changing statistical methodology, indicator definitions, survey-design logic,
benchmark values, or package APIs unless a specific reviewed task requires it.

Priorities include:

- Keep validation checks clean and report their exact results.
- Keep pkgdown, README, NEWS, and roadmap language aligned with the actual
  release and development state.
- Improve examples that demonstrate complete analytical workflows with
  synthetic or otherwise shareable data.
- Refine pkgdown reference organization only where it improves navigation.
- Prepare broader production hardening and CRAN-readiness reviews as future
  work, without implying that v0.1.1 is an official production release.
- Preserve the analytical-engine identity and avoid turning documentation into
  a dashboard or presentation-first product.

## Next Milestones

- Keep future work scoped through issue templates, pull request templates, and
  small Codex-ready task cards.
- Strengthen tests around metadata contracts, official tabulation parsers,
  reproducibility inputs, and representativity edge cases.
- Review benchmark-comparison outputs whenever official microdata and
  user-supplied inputs are available locally.
- Formalize any official validation pathway only if authorization or
  confirmation from the relevant authority is available.
- Improve release-readiness checks and review discipline before any broader
  production or CRAN release decision.

## Future Analytical Modules

Future modules should be added only when variables, methodological rules,
registries, and validation strategies are explicit and auditable. Possible
expansion areas include:

- historical harmonization across ENEMDU questionnaire changes;
- raw labor-status reconstruction only if a future methodology task explicitly
  authorizes variables, rules, and validation evidence;
- broader income and household analytical summaries;
- additional poverty-line scenario workflows beyond the current explicit-line
  infrastructure;
- expanded social bonus and optional bonus workflows;
- survey-domain representativity reports;
- official dictionary and metadata quality diagnostics;
- downstream Quarto/reporting helpers that consume stable analytical outputs
  without controlling computation.
- dashboard products as downstream consumers, not as replacements for the
  analytical engine.

## Governance And Quality Improvements

- Keep `R/*.R` source files ASCII-portable for R CMD check.
- Allow UTF-8 text in documentation outside `R/` when it improves readability.
- Avoid new package dependencies unless they are reviewed and approved.
- Require explicit validation evidence before claiming official agreement.
- Prefer small pull requests with clear files touched, validation commands, and
  methodological risk notes.

## Out Of Scope For The Current Stage

- Dashboards, final visual products, and presentation-first redesigns.
- Reconstructing labor condition from raw questionnaire variables.
- Computing poverty indicators without explicit auditable poverty lines.
- Global recoding of sentinel values without registry authorization.
- New official validation claims that have not been run and documented.
- A CRAN release commitment; CRAN submission can be considered later if package
  scope, checks, documentation, and maintenance expectations are ready.
