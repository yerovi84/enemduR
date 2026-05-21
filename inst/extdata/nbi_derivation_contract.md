# NBI Raw Component Derivation Contract

## Purpose

This document defines the initial `enemduR` contract for deriving final NBI
components from ENEMDU questionnaire variables. The output of this layer is
`comp1` through `comp5`, which can then be passed to
`enemdu_build_nbi_flags()`.

## Scope

The initial operational profile is `enemdu_2025_anual`. It derives:

- `comp1`: housing quality;
- `comp2`: overcrowding;
- `comp3`: basic services;
- `comp4`: access to education;
- `comp5`: household economic capacity / economic dependency.

The source variables used by this contract are:

- household and person identifiers: `id_hogar`, `p01`;
- demographic and education variables: `p03`, `p04`, `p07`, `p10a`, `p10b`;
- housing variables: `vi04a`, `vi05a`, `vi07`, `vi09`, `vi10`, `vi10a`;
- employment status variable: `empleo`;
- optional household size variable: `hsize`.

## Out of Scope

This phase does not implement TPM, TPEM, intensity, or IPM. It does not add
official NBI benchmark values and does not claim official validation. It does
not commit official microdata, local outputs, or raw official tabulation files.

## Raw Derivation Versus Final Components

The previous NBI layer starts from final components `comp1` through `comp5`.
This layer sits immediately before it. It derives those final components from
documented ENEMDU questionnaire variables, then leaves `knbi`, `nbi`, and
`xnbi` to `enemdu_build_nbi_flags()`.

## Component Rules

`comp1` identifies housing-quality deprivation from floor and wall material
codes. The initial profile uses parameterized deprivation-code sets documented
in the implementation. These codes must be reviewed against official labels
before official validation is claimed.

`comp2` identifies overcrowding when the household has no sleeping rooms or
when household members per sleeping room is greater than three. Division by zero
is avoided by classifying zero sleeping rooms directly as deprivation.

`comp3` identifies basic-services deprivation when sanitation or water-access
rules indicate a deprivation. Sanitation and water-code mappings are
profile-based and must be reviewed against official labels before validation
claims.

`comp4` identifies education-access deprivation when at least one child aged 6
to 12 in the household does not attend school. The household-level result is
assigned to all household members.

`comp5` identifies economic-capacity deprivation when the household head has
approximately two or fewer years of schooling and the dependency ratio is
greater than three. When the number of occupied persons is zero, the dependency
ratio is treated as infinite. The schooling approximation from `p10a` and
`p10b` is conservative and must be reviewed before official validation claims.

## Missing Data Policy

Component rules preserve non-evaluable cases as missing where the available
information is insufficient and no deprivation can be established from observed
inputs. Downstream `enemdu_build_nbi_flags()` keeps rows with missing components
unclassified for `knbi`, `nbi`, and `xnbi`.

## Official Validation

This contract documents an auditable implementation path. It is not official
INEC validation. Official validation requires published benchmark values,
documented source files, exact survey-design treatment, and reviewed comparison
results.

## Relationship With `enemdu_build_nbi_flags()`

The intended workflow is:

```r
data |>
  enemdu_build_nbi_components(profile = "enemdu_2025_anual") |>
  enemdu_build_nbi_flags()
```

The first step derives `comp1` through `comp5`; the second step computes
`knbi`, `nbi`, and `xnbi`.
