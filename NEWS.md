# enemduR 0.1.0.9000

## Current development version

- Added portfolio-oriented vignettes for onboarding, survey design,
  representativity, and poverty/NBI/IPM reproducibility workflows.
- Polished public portfolio documentation and pkgdown homepage styling,
  including the roadmap, pre-release wording, and dark logo presentation.
- Documented the final ENEMDU December 2025 IPM/TPM local reproducibility
  evidence, including the complete-case policy, benchmark comparison results,
  and the not-officially-validated status of the workflow.
- Updated the IPM derivation contract for the implemented `enemdu_2025_anual`
  profile, including the i03 official schooling recode policy, i04 derived-PEA
  official-like path, and i06 traceable derived labor-status path.
- Added a controlled NBI source-join helper for appending household/housing
  variables to person-level ENEMDU files without changing person row order.
- Added NBI raw-component derivation for the `enemdu_2025_anual` profile,
  deriving `comp1` through `comp5` from documented ENEMDU questionnaire
  variables before downstream NBI flag construction.
- Added an initial NBI module that builds `knbi`, `nbi`, and `xnbi` from final
  NBI components, estimates NBI poverty KPIs, and prepares benchmark comparison
  and reproducibility scaffolding without reconstructing raw questionnaire
  variables.
- Added a December 2025 income-poverty reproducibility workflow scaffold with
  input preflight checks, benchmark comparison output, and a local script that
  reads official microdata only from user-supplied environment variables.
- Added official poverty benchmark comparison helpers for published ENEMDU
  income-poverty benchmarks.
- Added `enemdu_kpi_income_poverty()`, a high-level wrapper for design-based
  poverty and extreme-poverty incidence estimates from explicit auditable
  poverty lines.
- Hardened the README for portfolio use and aligned it with the current
  analytical-engine scope, domain-scope contract, Quarto consumption model, and
  package limitations.
- Added a basic GitHub Actions workflow for R package checks.
- Added pkgdown configuration and GitHub Pages documentation publication support.
- Aligned roxygen documentation with the implemented labor module and refreshed
  exported-function manual coverage.
- Converted the labor-indicator user guide into an R Markdown vignette source.
- Removed avoidable package-check issues from stale metadata tests, unqualified
  `stats::ave()` calls, and non-ASCII characters in R source code.
- Fixed parsing of official labor tabulation percentages with dot-decimal notation, such as `96.4%`, used in annual official CSV tabulations.
- Added normalized period matching for official labor tabulation comparisons, so labels such as `I - 2026` and `I- 2026` are treated as equivalent.
- Added support for reading `.sav`, `.dta` and `.csv` files through `enemdu_read_data()`.
- Documented `.sav` as the operational primary format for recent official ENEMDU microdata.
- Kept `.dta` and `.csv` support as interoperability formats.
- Added tests for SAV, DTA, comma-separated CSV, semicolon-separated CSV and unsupported file formats.
- Added official labor indicator registry support.
- Added helper infrastructure for reading and comparing official labor tabulations.
- Added explicit labor domain-scope control through `domain_scope = c("observed", "design")`.
- Added initial labor KPI support based on the consolidated `condact` variable.
- Added representativity and precision-evaluation infrastructure for survey estimates.
- Added metadata registries for domains, indicators, validation rules, missing codes, value ranges, poverty lines, optional bonuses and official dictionaries.
- Added support for official dictionary reading and microdata validation against dictionaries.
- Added basic real-data smoke-test infrastructure.
- Added support for income, quintiles, poverty flags, household profiles, optional bonuses and social bonus indicators.
- Added survey estimator helpers for totals, means, proportions and tabulations.
- Declared placeholder Quarto helper interfaces while keeping analytical
  outputs as the primary Quarto-ready consumption layer.

## Initial development version

- Created the base package scaffold for `enemduR`.
- Defined the initial public API.
- Added initial support for reading `.dta` files.
- Added basic name standardization and structural validation.
- Added seed metadata registries for survey type and comparability alerts.
- Declared the analytical and Quarto helper layers for later implementation.
