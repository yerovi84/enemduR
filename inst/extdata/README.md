# extdata for enemduR

This directory stores metadata registries and auxiliary catalogs used by the package.

## Phase 1 contents

- `survey_registry.csv`: survey-level structural metadata for monthly, quarterly and annual ENEMDU workflows.
- `comparability_registry.csv`: alert registry for periods that must carry a comparability warning.

## Design rule

Files in this directory should be:

- human-readable,
- versionable,
- explicit,
- and editable without rewriting package logic.

## Warning

The presence of a registry file here does not mean that all substantive rules are closed.
Detailed indicator contracts and variable mappings belong to later phases.
