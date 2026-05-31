# AGENTS.md

Guidance for Codex or other code agents working in `enemduR`.

## Project Purpose

`enemduR` is an R package for reproducible analytical infrastructure using
Ecuador ENEMDU microdata. It is an analytical engine for reading, validation,
derivation, survey-design-aware estimation, representativity assessment,
official validation workflows, and Quarto-ready analytical outputs.

## Working Rules

- Keep changes small, auditable, and scoped to the requested task.
- Prefer existing package patterns over new abstractions.
- Separate analytical computation from presentation.
- Do not change methodology, indicators, survey-design logic, domain rules,
  labor definitions, poverty logic, or registries unless explicitly requested.
- Do not add package dependencies without user approval.
- Do not claim official validation unless the official comparison was actually
  run and the evidence is reported.

## Portability And Encoding

- Files under `R/*.R` must be ASCII-portable for R CMD check.
- Non-ASCII user-facing strings in `R/*.R` must use Unicode escapes such as
  `\u00f3`, `\u00e1`, and `\u00f1`.
- Documentation outside `R/` may use UTF-8 literal text.

## Validation Commands

Run the smallest useful validation for the change. For package-level changes,
prefer:

```sh
Rscript -e "devtools::load_all(reset = TRUE)"
Rscript -e "devtools::test()"
```

When feasible, also run:

```sh
Rscript -e "devtools::check()"
```

If a source tarball check is needed, build it first and pass the generated
tarball path explicitly instead of relying on a hard-coded package version.

If a command cannot run because of local tooling or network constraints, report
the exact blocker.

## Forbidden Without Explicit Approval

- Reconstructing `condact` from raw questionnaire variables.
- Computing poverty indicators without explicit auditable poverty lines.
- Globally recoding sentinel values such as `999`, `9999`, `99999`, or `999999`.
- Prefiltering microdata for design domains when the package contract requires
  full-data estimation followed by output filtering.
- Confusing weighted population counts with effective sample size.
- Pushing, merging, or publishing releases unless requested.

## Reporting Format

Final reports should include:

- files changed;
- validation commands run and exact results;
- warnings, notes, errors, or skipped checks;
- methodological assumptions or limitations;
- whether a branch, commit, push, or pull request was created.
