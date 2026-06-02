# enemduR 0.1.0

## First stable portfolio release

`enemduR` 0.1.0 is the first stable portfolio release of the package. It
positions the project as a reproducible analytical engine for Ecuador ENEMDU
microdata workflows, not as a dashboard, CRAN release, official statistical
production system, or official institutional validation product.

- Includes reading support for `.sav`, `.dta`, and `.csv` files through
  `enemdu_read_data()`, with `.sav` documented as the operational primary
  format for recent ENEMDU workflows and `.dta` and `.csv` retained for
  interoperability.
- Provides metadata registries for variables, indicators, domains, validation
  rules, missing codes, value ranges, poverty lines, optional bonuses, income
  components, and official dictionaries.
- Includes survey-design-aware estimators for totals, means, proportions, and
  tabulations, plus representativity and precision-evaluation infrastructure
  aligned with ENEMDU design-domain contracts.
- Provides labor indicators based on the consolidated `condact` variable,
  including the labor indicator registry, domain-scope controls, and helpers
  for reading and comparing official labor tabulations.
- Includes income, quintile, household-profile, poverty, and extreme-poverty
  infrastructure driven by explicit, auditable poverty-line contracts.
- Adds NBI component, flag, KPI, benchmark-comparison, source-join, and local
  reproducibility infrastructure for the implemented ENEMDU 2025 annual
  profile.
- Adds IPM/TPM component, flag, KPI, benchmark-comparison, and local
  reproducibility infrastructure for the implemented ENEMDU 2025 annual
  profile.
- Publishes pkgdown documentation and portfolio vignettes covering getting
  started, survey design and representativity, labor indicators, and poverty,
  NBI, and IPM/TPM reproducibility workflows.
- Retains local reproducibility evidence and benchmark-comparison workflows as
  evidence for reproducible local analysis under explicit inputs and contracts.
  These comparisons are not official institutional validation.
- Preserves methodological guardrails: no reconstruction of `condact` from raw
  questionnaire variables, no poverty classification without explicit poverty
  lines, no global sentinel-value recoding, no design-domain prefiltering before
  estimation, and no confusion between weighted population counts and effective
  sample size.

## Initial development version

- Created the base package scaffold for `enemduR`.
- Defined the initial public API.
- Added initial support for reading `.dta` files.
- Added basic name standardization and structural validation.
- Added seed metadata registries for survey type and comparability alerts.
- Declared the analytical and Quarto helper layers for later implementation.
