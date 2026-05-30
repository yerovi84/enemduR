# IPM Component Derivation Audit

## Purpose

This document is a historical audit that guided the implementation of
`enemdu_build_ipm_components()`.

The current implemented contract for the IPM/TPM workflow is documented in
`ipm_derivation_contract.md`. The retained ENEMDU December 2025 local
reproducibility evidence is documented in
`official_ipm_reproducibility_evidence_december_2025.md`.

The audit maps the 12 official Ecuador IPM deprivation indicators declared in
`ipm_component_registry.csv` to available repository assets, likely ENEMDU
source variables, reusable helpers, methodological risks, and implementation
questions. Sections below that use future-tense implementation language should
be read as historical planning context, not as the current package state.

This file does not change any public API and does not claim institutional
validation.

## Current State Of The IPM Module

The IPM module currently has implemented derivation, scoring, KPI, benchmark
comparison, and reproducibility layers:

- `inst/extdata/ipm_component_registry.csv` declares the official IPM structure:
  4 dimensions, 12 indicators, analytical weights, applicable populations, and
  expected component names.
- `inst/extdata/ipm_derivation_registry.csv` records source-variable and caveat
  metadata for the `enemdu_2025_anual` profile.
- `inst/extdata/ipm_derivation_contract.md` documents the implemented IPM/TPM
  architecture and profile-specific policies.
- `R/ipm_components.R` implements the 12 component builder for the current
  profile.
- `R/ipm_flags.R` implements `enemdu_build_ipm_flags()`, which computes
  row-level `ipm_score`, `tpm`, and `tpem` from already-built binary component
  columns using the registered weights.
- `R/ipm_reproducibility.R` implements complete-case reproducibility
  diagnostics and benchmark comparison.

The ENEMDU December 2025 local reproducibility run remains marked as
`not_officially_validated`.

## Methodological Boundaries

The future component builder must respect these boundaries:

- IPM and NBI are related but not identical. NBI component rules must not be
  copied into IPM without indicator-specific verification.
- Labor indicators must use the consolidated and validated labor status
  infrastructure when possible. The package must not reconstruct `condact` or
  CIET labor categories from raw questionnaire variables in the IPM component
  phase.
- Extreme income poverty must use an auditable poverty flag or explicit
  poverty-line inputs. The package must not compute income poverty without a
  documented line source.
- Household-level deprivations repeated on person records must preserve the
  person-level analysis frame required by IPM scoring.
- Non-applicable populations must be handled explicitly by each component.
  They must not be silently treated as deprived, and missing values must not be
  silently treated as non-deprived.
- This audit does not establish institutional validation. Current
  reproducibility evidence is retained as local benchmark comparison evidence.
  Any official validation would require explicit authorization or confirmation
  by the relevant official authority, and package outputs remain marked as not
  officially validated.

## Repository Assets Already Available

### IPM Assets

- `ipm_component_registry.csv`: authoritative source for component IDs,
  expected component names, dimensions, weights, and applicable-population
  labels.
- `ipm_derivation_registry.csv`: source-variable and caveat registry for the
  `enemdu_2025_anual` profile.
- `ipm_derivation_contract.md`: implemented IPM/TPM methodological boundary
  document.
- `enemdu_build_ipm_components()`: profile-specific component builder.
- `enemdu_build_ipm_flags()`: downstream row-level scoring and TPM/TPEM flag
  builder that is called after component flags exist.
- `enemdu_run_ipm_reproducibility()`: complete-case reproducibility and
  benchmark comparison workflow.

### Registry And Validation Helpers

- `.enemdu_read_csv_registry()` in `R/utils-metadata.R` is the existing pattern
  for package registry access.
- `.enemdu_abort_missing_vars()` and `.enemdu_abort_invalid_registry()` provide
  existing error style for missing variables and invalid metadata.
- `variable_catalog.csv` and `official_dictionary_core_registry.csv` cover core
  design, domain, identifier, income, and bonus variables, but they do not yet
  cover the full education, housing, labor-detail, pension, or garbage-collection
  variable set required by IPM component derivation.

### NBI Assets

- `enemdu_build_nbi_components()` derives NBI components from ENEMDU variables
  for profile `enemdu_2025_anual`.
- Useful NBI helper mechanics include numeric coercion, household maximum and
  sum operations, household first-value repetition, household-size construction,
  overcrowding mechanics, and conservative years-of-schooling approximation.
- `enemdu_join_nbi_sources()` safely joins person rows to household/housing
  source rows by `id_hogar`, preserves person row order, checks duplicate
  household rows, and supports a custom `housing_vars` vector.
- NBI evidence for December 2025 documents local source availability and
  observed coding for variables such as `p03`, `p07`, `p10a`, `p10b`, `empleo`,
  `vi04a`, `vi05a`, `vi07`, `vi09`, `vi10`, and `vi10a`.

NBI logic is useful as implementation scaffolding, not as a methodological
shortcut. The NBI schooling rule is for children aged 6 to 12; IPM education
indicators have different universes. The NBI services rule combines sanitation,
water source, and water delivery; IPM separates water and sanitation and may use
different definitions.

### Income Poverty Assets

- `enemdu_build_variables()` can derive `ingtot_pc` from income components and
  household size.
- `enemdu_build_poverty_flags()` can derive `pobre` and `expobre` when explicit
  poverty and extreme-poverty lines are supplied or when an appropriate registry
  row exists.
- The package poverty-line registry is currently a template; future IPM
  component derivation should not assume a usable period-specific line is
  present unless it is supplied or registered.
- Poverty reproducibility evidence exists for December 2025, but it is local
  reproducibility evidence and no institutional validation is claimed.

### Labor Assets

- `enemdu_build_labor_flags()` builds labor flags from the consolidated ENEMDU
  `condact` variable and optional `secemp`. It does not reconstruct labor status
  from raw questionnaire items.
- The labor registry documents implemented outputs such as `labor_pet`,
  `labor_pea`, `labor_pei`, `labor_empleo`, `labor_empleo_adecuado`,
  `labor_subempleo`, `labor_otro_empleo_no_pleno`, `labor_empleo_no_remunerado`,
  `labor_empleo_no_clasificado`, and `labor_desempleo`.
- The labor validation contract records the `condact` categories 0 to 9 and
  states that the current labor block has been validated against official labor
  tabulations for selected 2026 monthly/quarterly and 2025 annual domains.
- The IPM labor indicators should depend on this consolidated labor layer where
  possible and should not recreate labor classifications from questionnaire
  variables.

## Indicator-By-Indicator Audit Table

| indicator_id | Indicator label | Applicable population | Likely required variables | Source level | Existing reuse | Missing variables or uncertainties | Risk | Implementation recommendation |
|---|---|---|---|---|---|---|---|---|
| `ipm_i01_inasistencia_basica_bach` | Inasistencia a educacion basica y bachillerato | School-age basic and bachillerato population | `id_hogar`; `p03`; `p07`; `p10a`; `p10b`; possibly `p09` | person, then household-from-person condition | Conditional reuse of NBI numeric coercion and household aggregation; do not reuse NBI school-age rule directly | Exact basic/bachillerato age range, enrollment/attendance coding, grade-cycle equivalence, and role of `p09` must be confirmed | Medium | Implement after confirming universe and attendance codes. Build person-level applicability, then repeat household deprivation to person records. |
| `ipm_i02_no_acceso_superior_economico` | No acceso a educacion superior por razones economicas | Higher-education age population | `id_hogar`; `p03`; `p07`; `p10a`; `p10b`; reason-for-nonattendance variable, likely `p09` or another education reason variable | person, then household-from-person condition | Conditional reuse of education coercion and household aggregation only | Reason variable and economic-reason codes are not registered; higher-education universe must be audited | High | Do not implement until official dictionary and coding for economic non-access are confirmed. Add an explicit variable argument if the official name varies by period. |
| `ipm_i03_logro_educativo_incompleto` | Logro educativo incompleto | Adult or reference education population | `id_hogar`; `p03`; `p07`; `p10a`; `p10b`; possibly `area` | person, then household-from-person condition | Conditional reuse of `.enemdu_nbi_years_schooling()` mechanics after review | Official schooling-years equivalence, age threshold, completed-cycle threshold, and treatment of `p10a == 1` with missing `p10b` must be verified | Medium | Start from a reviewed schooling-years helper shared with NBI only if the rule is parameterized and documented for IPM. |
| `ipm_i04_empleo_infantil_adolescente` | Empleo infantil y adolescente | Children and adolescents | `id_hogar`; `p03`; `condact`; possibly child-labor detail variables | person, then household-from-person condition using derived labor | Conditional reuse of `enemdu_build_labor_flags()` for `labor_menor_15` and employment categories | IPM child/adolescent employment definition may require age bands, hours, school status, or activity details beyond `condact` | High | Use consolidated `condact` and labor flags where sufficient. Do not reconstruct labor status. Confirm whether extra child/adolescent work variables are needed. |
| `ipm_i05_desempleo_empleo_inadecuado` | Desempleo o empleo inadecuado | Labor force or working-age population | `id_hogar`; `p03`; `condact`; possibly `secemp` | person, then household-from-person condition using derived labor | Conditional reuse of `enemdu_build_labor_flags()` outputs: `labor_desempleo`, `labor_subempleo`, `labor_otro_empleo_no_pleno`, `labor_empleo_no_remunerado`, `labor_empleo_no_clasificado`, `labor_empleo_adecuado`, `labor_pet`, `labor_pea` | Exact IPM mapping of "empleo inadecuado" to consolidated labor categories must be confirmed; non-applicable inactive persons need explicit handling | Medium | Implement only as a mapping over validated labor flags, with a separately documented IPM category contract. |
| `ipm_i06_no_contribucion_pensiones` | No contribucion al sistema de pensiones | Employed or contribution-eligible population | `id_hogar`; `p03`; `condact`; pension contribution variable pending review | person, then household-from-person condition using derived labor plus pension source | Labor flags can identify employment and age universe; no current pension helper exists | Pension contribution variable name, valid codes, eligible population, exceptions, and informal-worker treatment are unresolved | High | Add no operational rule until the pension contribution variable and eligibility logic are confirmed from official dictionaries/methodology. |
| `ipm_i07_pobreza_extrema_ingresos` | Pobreza extrema por ingresos | Persons in households with valid income | Preferred: precomputed `expobre`; alternative: `ingtot_pc` plus explicit `extreme_poverty_line` and line source | household_repeated_person / derived income | Safe reuse of existing poverty flag output if `expobre` is already present; conditional reuse of `enemdu_build_poverty_flags()` with explicit lines | Registry lines may be absent; line period and source must match data period; non-positive income policy must remain explicit | Medium | Prefer requiring `expobre` by default. Offer an explicit build mode only when poverty lines and source metadata are supplied. |
| `ipm_i08_sin_agua_red_publica` | Sin servicio de agua por red publica | Households | `id_hogar`; `vi10`; possibly `vi10a` | household, repeated to person | Conditional reuse of NBI source join and numeric coercion; do not reuse NBI basic-services rule | IPM water deprivation may be based on public network only, while NBI accepts selected non-public piped sources; exact `vi10` codes must be verified | Low | Implement as a separate IPM water rule after verifying public-network coding. Keep it independent from NBI `comp3`. |
| `ipm_i09_hacinamiento` | Hacinamiento | Households | `id_hogar`; `p01`; `hsize`; `vi07` | joined person-household / household, repeated to person | Conditional reuse of NBI household-size and overcrowding mechanics | IPM threshold and bedroom definition may match NBI but must be confirmed; zero-bedroom handling must be documented | Low | Reuse NBI mechanics if the IPM threshold is confirmed. Keep the IPM component name and diagnostics separate from NBI `comp2`. |
| `ipm_i10_deficit_habitacional` | Deficit habitacional | Households | `id_hogar`; likely `vi03a`; `vi03b`; `vi04a`; `vi04b`; `vi05a`; `vi05b`; possibly other housing quality variables | household, repeated to person | Conditional reuse of NBI join and material-code coercion only | Housing deficit may require roof, floor, wall, condition, qualitative and/or quantitative criteria beyond current NBI variables | High | Design a new housing-deficit rule after auditing official codes. Do not equate this indicator with NBI housing quality. |
| `ipm_i11_sin_saneamiento_excretas` | Sin saneamiento de excretas | Households | `id_hogar`; `area`; `vi09`; possibly related sanitation variables | joined person-household / household, repeated to person | Conditional reuse of NBI join and numeric coercion; do not reuse NBI sanitation rule directly | IPM rule is expected to be area-specific; urban and rural thresholds must be verified separately | Medium | Require `area` explicitly. Implement separate urban/rural code sets only after methodological confirmation. |
| `ipm_i12_sin_recoleccion_basura` | Sin servicio de recoleccion de basura | Households | `id_hogar`; likely `vi13` or another garbage-service variable | household, repeated to person | Reuse join mechanics only | Garbage variable is not in current NBI default join or core dictionary registry; valid service categories must be confirmed | High | Add the garbage variable to an IPM source join contract after dictionary review. Keep rule non-operational until codes are confirmed. |

## Education Indicator Notes

The education block should treat `p03`, `p07`, `p09`, `p10a`, and `p10b` as
candidate variables, but the repository does not yet have an IPM-specific
education coding contract.

Current NBI code uses `p03`, `p07`, `p10a`, and `p10b` for two purposes:

- school attendance deprivation for ages 6 to 12;
- conservative schooling-years approximation for the household head in the NBI
  economic-capacity component.

Those mechanics are not enough for IPM. IPM has separate indicators for basic
and bachillerato attendance, economic non-access to higher education, and
incomplete educational achievement. Future implementation should:

- confirm the exact age ranges and education-cycle equivalences;
- confirm whether `p09` is the correct reason-for-nonattendance variable for
  higher-education economic access;
- explicitly mark non-applicable ages as not deprived for scoring only after
  applicability is determined;
- keep missing source values as non-evaluable unless a documented official rule
  says otherwise.

## Labor And Social Security Notes

The repository has a strong labor base through `enemdu_build_labor_flags()`.
That function exposes `labor_pet`, `labor_pea`, `labor_pei`, `labor_empleo`,
`labor_empleo_adecuado`, `labor_subempleo`, `labor_otro_empleo_no_pleno`,
`labor_empleo_no_remunerado`, `labor_empleo_no_clasificado`, and
`labor_desempleo` from consolidated `condact`.

This is the correct layer for the future IPM labor indicators to depend on.
The future implementation should not use raw labor questionnaire variables to
recreate `condact`, `condactn`, `empleo`, `desem`, `pei`, or `pet`.

Current repository status by candidate variable:

- `condact` is supported by the labor layer and validated contract.
- `secemp` is supported for sector flags but may not be required by IPM.
- `labor_pet`, `labor_pea`, and `labor_pei` are derived outputs, not raw
  official source columns.
- NBI uses `empleo` for household economic capacity, but that variable is not a
  sufficient source for IPM labor indicators.
- Pension contribution variables are not yet identified in the package
  registries inspected for this audit.

## Extreme Income Poverty Notes

The IPM income component should be designed around an auditable `expobre`
source.

Preferred future behavior:

1. If `expobre` already exists, validate it as a binary flag and use it.
2. If `expobre` is absent and the user supplies `ingtot_pc`, an explicit
   `extreme_poverty_line`, period, and line source, call or mirror the existing
   poverty-flag contract to derive `expobre`.
3. If neither condition is met, abort with an informative error.

The future IPM component builder should not silently look up a poverty line from
the template registry unless a period-specific row is present and validated.

## Housing, Water, Sanitation, And Garbage Notes

The current NBI housing/service block covers:

- `vi04a` and `vi05a` for housing quality;
- `vi07` for sleeping rooms;
- `vi09`, `vi10`, and `vi10a` for basic services;
- `p01` and `hsize` for household membership and overcrowding.

The future IPM component builder likely needs a broader housing-source contract:

- `vi03a`
- `vi03b`
- `vi04a`
- `vi04b`
- `vi05a`
- `vi05b`
- `vi07`
- `vi09`
- `vi10`
- `vi13`

The exact role of each variable must be confirmed from official dictionaries and
IPM methodology before operational rules are written. `area` should be required
for sanitation if urban and rural excreta-disposal thresholds differ.

## Source Join Requirements

`enemdu_join_nbi_sources()` is mechanically suitable for controlled
person-to-household joins because it:

- joins by `id_hogar`;
- checks for duplicate household rows in strict mode;
- checks missing household source variables;
- preserves person row count and row order;
- accepts a custom `housing_vars` vector.

However, its default `housing_vars` are NBI-specific:

`vi04a`, `vi05a`, `vi07`, `vi09`, `vi10`, `vi10a`

That default is not enough for IPM if the future implementation needs roof,
wall, floor, housing-deficit, or garbage variables such as `vi03a`, `vi03b`,
`vi04b`, `vi05b`, and `vi13`.

Future IPM implementation has two reasonable options:

1. Reuse `enemdu_join_nbi_sources()` with an explicit IPM `housing_vars` vector.
2. Add a small IPM-specific wrapper such as `enemdu_join_ipm_sources()` only if
   the package needs a stable public source-join contract for IPM.

The second option is clearer for users, but it should remain a thin wrapper
around the existing controlled join mechanics.

## Reuse Map

### Safe Reuse

- `ipm_component_registry.csv` for expected component names, indicator order,
  dimensions, weights, and output alignment.
- `enemdu_build_ipm_flags()` after all 12 component flags are built.
- `.enemdu_read_csv_registry()` and existing registry/error helper style.
- Existing poverty flag `expobre` when it is already present and validated as
  binary.
- Controlled join mechanics from `enemdu_join_nbi_sources()` when called with
  the full IPM household variable set.

### Conditional Reuse

- NBI numeric coercion, household aggregation, household-size, and overcrowding
  mechanics, only after IPM-specific rules are confirmed.
- `.enemdu_nbi_years_schooling()` mechanics, only after reviewing IPM education
  equivalences and thresholds.
- `enemdu_build_poverty_flags()`, only with explicit extreme poverty lines and
  source metadata or a validated period-specific registry row.
- `enemdu_build_labor_flags()`, only as the source for labor categories derived
  from consolidated `condact`; the IPM mapping still needs its own contract.
- NBI housing and service variables, only as source variables and coercion
  mechanics, not as final IPM rules.

### Do Not Reuse

- NBI `comp3` basic-services rule as the IPM water or sanitation rule.
- NBI `comp4` school attendance rule as the complete IPM education block.
- NBI `comp5` economic-capacity rule for any IPM labor or income indicator.
- Raw labor questionnaire reconstruction of `condact`.
- Poverty-line assumptions without an auditable line source.
- Any local reproducibility evidence as an institutional certification claim.

## Implementation Risk Classification

### Low Risk

- `ipm_i08_sin_agua_red_publica`, if the public-network code is confirmed.
- `ipm_i09_hacinamiento`, if the IPM threshold matches the existing
  overcrowding mechanics.

### Medium Risk

- `ipm_i01_inasistencia_basica_bach`, because the age and education-cycle
  universe must differ from NBI schooling.
- `ipm_i03_logro_educativo_incompleto`, because schooling-years equivalence and
  thresholds need methodological review.
- `ipm_i05_desempleo_empleo_inadecuado`, because the labor flags exist but the
  IPM mapping of inadequate employment must be confirmed.
- `ipm_i07_pobreza_extrema_ingresos`, because infrastructure exists but line
  source and period handling must be explicit.
- `ipm_i11_sin_saneamiento_excretas`, because the rule may be area-specific.

### High Risk

- `ipm_i02_no_acceso_superior_economico`, because the economic reason variable
  and codes are not confirmed.
- `ipm_i04_empleo_infantil_adolescente`, because child/adolescent labor may
  require rules beyond consolidated `condact`.
- `ipm_i06_no_contribucion_pensiones`, because pension contribution variables
  and eligibility logic are unresolved.
- `ipm_i10_deficit_habitacional`, because housing deficit may require variables
  and definitions beyond NBI housing quality.
- `ipm_i12_sin_recoleccion_basura`, because the garbage collection source
  variable and codes are still pending dictionary confirmation.

## Proposed Implementation Order

1. Add an IPM source-variable preflight for the full expected person and
   household source set, using the registry pattern already used by NBI.
2. Implement the source-join plan for household variables, either by calling
   `enemdu_join_nbi_sources()` with explicit IPM variables or by adding a thin
   IPM wrapper.
3. Implement low-risk household components first: water and overcrowding, after
   verifying their IPM-specific codes.
4. Implement extreme income poverty by accepting a precomputed `expobre` first,
   then optionally supporting explicit poverty-line derivation.
5. Implement education indicators after confirming age ranges, `p09` or reason
   variables, and schooling equivalence.
6. Implement sanitation after confirming area-specific thresholds and requiring
   `area`.
7. Implement labor inadequate-employment mapping using `enemdu_build_labor_flags()`.
8. Implement child/adolescent employment only after confirming whether
   consolidated labor flags are sufficient.
9. Implement housing deficit after the housing-variable dictionary review is
   complete.
10. Implement pension contribution and garbage collection last unless official
    variable names and codes are resolved earlier.
11. Run `enemdu_build_ipm_flags()` only after all 12 component flags are built
    and validated.

## Future Function Contract For `enemdu_build_ipm_components()`

A future public function should be small, auditable, and registry-backed. A
candidate contract is:

```r
enemdu_build_ipm_components <- function(
  data,
  profile = "enemdu_2025_anual",
  household_data = NULL,
  household_id = "id_hogar",
  extreme_poverty_var = "expobre",
  income_var = "ingtot_pc",
  extreme_poverty_line = NULL,
  poverty_line_source = NULL,
  build_labor_flags = TRUE,
  overwrite = FALSE,
  strict = TRUE
)
```

Contract principles:

- Return the input data with the 12 registered component variables added.
- Do not compute `ipm_score`, `tpm`, or `tpem`; that remains the role of
  `enemdu_build_ipm_flags()`.
- Do not compute KPI-level `A` or aggregate `ipm`.
- Validate that all output names match `ipm_component_registry.csv`.
- Preserve person row count and row order after household joins.
- Use household-level deprivations repeated on person rows for scoring.
- Treat non-applicable populations explicitly and consistently.
- Abort on missing required variables in strict mode.
- Preserve missing/non-evaluable component values rather than recoding them to
  zero.
- Attach diagnostics similar in spirit to existing NBI/IPM diagnostics, including
  source variables used, joined household variables, component NA counts, and
  non-applicable counts.

## Future Test Plan

Future tests should use synthetic data only and should cover:

- The function creates exactly the 12 registered component columns and no
  scoring or KPI columns.
- Default output names match `ipm_component_registry.csv`.
- Missing required source variables abort in strict mode.
- `overwrite = FALSE` aborts if any component already exists.
- `overwrite = TRUE` replaces existing component columns.
- Household joins preserve person row count and row order.
- Duplicate household rows abort in strict mode.
- Unmatched household IDs abort or produce documented diagnostics according to
  the source-join contract.
- Non-applicable populations are handled separately from missing source data.
- Missing source values do not become non-deprivation silently.
- Education tests cover age boundaries and reason-for-nonattendance boundaries
  once official codes are confirmed.
- Labor tests confirm that the implementation uses consolidated `condact` or
  derived labor flags and does not require raw CIET reconstruction.
- Poverty tests cover precomputed `expobre`, explicit poverty-line derivation,
  and absence of auditable lines.
- Housing and service tests cover water, overcrowding, housing deficit,
  sanitation by `area`, and garbage collection using official codes once
  confirmed.
- Integration tests confirm that the 12 component outputs can be passed to
  `enemdu_build_ipm_flags()` and produce expected synthetic `ipm_score`, `tpm`,
  and `tpem`.

## Validation Plan

The implementation phase should validate in layers:

1. Registry validation: confirm 12 components, unique IDs, expected names,
   weights, profiles, required variables, and non-official status.
2. Source validation: confirm required variables exist in person and household
   sources for the target period.
3. Component validation: validate binary or missing outputs and document
   non-applicable counts.
4. Internal IPM validation: pass components to `enemdu_build_ipm_flags()` and
   check score consistency.
5. Survey-design validation: in a later KPI phase, estimate TPM and TPEM with
   package survey design infrastructure.
6. Reproducibility validation: compare calculated outputs against published
   benchmarks only after benchmark sources are registered and documented.

## Open Questions

- Is `p09` the correct variable for reason for not attending education, and
  what exact codes represent economic reasons for higher-education non-access?
- What are the official age ranges for basic education, bachillerato, higher
  education access, child/adolescent employment, and educational achievement?
- Does IPM incomplete educational achievement use individual adult attainment,
  household-head attainment, or another household-level rule?
- Which consolidated labor categories define "empleo inadecuado" for IPM?
- Does child/adolescent employment require hours worked or other detail beyond
  `condact`?
- Which variable records pension contribution, and what population is eligible
  for the deprivation?
- Should the extreme income poverty component require precomputed `expobre` by
  default, or should the public function support explicit poverty-line
  derivation in the first component phase?
- Does the IPM water rule use only public-network access from `vi10`, or does it
  also depend on water delivery variables such as `vi10a`?
- What are the official variables and codes for housing deficit: `vi03a`,
  `vi03b`, `vi04a`, `vi04b`, `vi05a`, `vi05b`, or additional fields?
- Are sanitation thresholds explicitly different by `area`, and what are the
  urban and rural code sets?
- Is `vi13` the correct garbage collection variable for the target annual
  profile, and what codes represent no collection service?
- Should the package add a public `enemdu_join_ipm_sources()` wrapper or keep
  the source join internal to `enemdu_build_ipm_components()`?

## Non-Official Validation Statement

This audit is retained as historical implementation context. It does not claim
endorsement by INEC or any other public institution.

The implemented IPM workflow provides local reproducibility evidence and
benchmark comparison outputs. No institutional validation is claimed; any
official validation would require explicit authorization or confirmation by the
relevant official authority. The workflow remains marked as not officially
validated.
