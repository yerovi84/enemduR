# enemduR Roadmap

`enemduR` is an R package for reproducible analytical infrastructure using
Ecuador ENEMDU microdata. The package is an analytical engine for reading,
validation, derivation, survey-design-aware estimation, representativity
assessment, official validation workflows, and downstream Quarto-ready
analytical outputs.

## Current Status

`enemduR` is in active development. The repository has the core package
structure, user documentation, generated manual pages, a labor vignette,
GitHub Actions for R package checks, a published pkgdown documentation site,
and lightweight agent governance.

## Completed Foundations

- Reading support for `.sav`, `.dta`, and `.csv` ENEMDU files.
- Metadata registries for variables, domains, indicators, missing codes,
  value ranges, poverty lines, optional bonuses, and official dictionaries.
- Survey-design-aware estimators for totals, means, proportions, and
  tabulations.
- Representativity and precision-evaluation infrastructure.
- Labor indicator infrastructure based on the consolidated `condact` variable.
- Official labor-tabulation parsing and comparison helpers.
- Quarto-ready analytical outputs while keeping presentation separate from
  computation.
- Package hardening through README, NEWS, roxygen/manual pages, vignette,
  GitHub Actions, pkgdown publication, and agent guidance.

## Next Milestones

- Keep future work scoped through issue templates, pull request templates, and
  small Codex-ready task cards.
- Expand examples that demonstrate complete analytical workflows with synthetic
  or otherwise shareable data.
- Continue Phase 7 income and poverty work from explicit contracts; the income
  poverty KPI wrapper is implemented for auditable user-supplied poverty lines,
  official poverty benchmark comparison readiness is implemented for published
  December 2025 reference values, and a local reproducibility workflow scaffold
  is available for December 2025 income-poverty indicators.
- Run the December 2025 reproducibility workflow with official microdata and
  review benchmark differences before any official poverty validation claim.
- Continue Phase 7 NBI work from final NBI components only; the initial module
  builds `knbi`, `nbi`, and `xnbi`, estimates NBI KPIs, and prepares benchmark
  and reproducibility scaffolding without reconstructing raw questionnaire
  variables.
- Add and review raw ENEMDU-to-NBI component derivation for the
  `enemdu_2025_anual` profile, then compare results against audited official
  NBI benchmarks before any validation claim.
- Strengthen tests around metadata contracts, official tabulation parsers, and
  representativity edge cases.
- Refine pkgdown reference organization without turning documentation into a
  dashboard or visual product.
- Improve release-readiness checks and review discipline before any public
  release decision.

## Future Analytical Modules

Possible future modules should be added only when variables, methodological
rules, registries, and validation strategies are explicit and auditable.
Candidate areas include:

- income and household analytical summaries;
- poverty-line-driven poverty indicators;
- NBI benchmark intake and official comparison workflows once audited official
  benchmark rows are available;
- TPM, TPEM, intensity, and IPM under a separate multidimensional poverty
  contract;
- social bonus and optional bonus workflows;
- survey-domain representativity reports;
- official dictionary and metadata quality diagnostics.

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
