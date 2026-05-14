# enemduR 0.0.1.9000

## Current development version

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
- Added output helpers for Quarto-compatible analytical consumption.

## Initial development version

- Created the base package scaffold for `enemduR`.
- Defined the initial public API.
- Added initial support for reading `.dta` files.
- Added basic name standardization and structural validation.
- Added seed metadata registries for survey type and comparability alerts.
- Declared the analytical and Quarto helper layers for later implementation.
