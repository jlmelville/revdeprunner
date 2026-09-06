# Plan a reverse-dependency check

Discover reverse dependencies and estimate the preparation work without
downloading, building, or checking packages. Direct reverse dependencies
are always selected. Recursive-strong-only targets are opt-in and can be
limited with a reproducible sample.

## Usage

``` r
revdep_plan(
  package,
  recursive = FALSE,
  max_recursive = NULL,
  sample_seed = NULL,
  cache = NULL,
  repos = NULL
)
```

## Arguments

- package:

  Path to the development package checkout.

- recursive:

  Include recursive strong reverse dependencies as candidates.

- max_recursive:

  Optional maximum number of recursive-only targets to select. All
  direct targets remain selected.

- sample_seed:

  Optional non-negative integer used to choose a different reproducible
  recursive sample. It can be supplied only with `max_recursive`.

- cache:

  Cache directories to inspect for compatible binaries. `NULL` uses the
  ordinary `crancache` directory and the current runner-managed binary
  cache when they exist;
  [`character()`](https://rdrr.io/r/base/character.html) disables cache
  inspection. An explicit value replaces default discovery.

- repos:

  Named source repository base URLs. `NULL` combines the configured
  repositories with the standard Bioconductor repositories. An explicit
  value is used exactly. When a named `CRAN` repository is present,
  reverse targets come from CRAN while dependencies can come from every
  configured repository.

## Value

A `revdep_plan` list with `summary`, `targets`, `requirements`,
`unavailable`, and `repository_alternates` tables. Per-target
requirement counts overlap; use `summary` for totals over the unique
selected requirement set. Declared system requirements are metadata
clues, not a platform-readiness check. Repository-unavailable `Suggests`
remain in `unavailable`, but do not enter preparation requirements.

## Examples

``` r
if (FALSE) { # \dontrun{
plan <- revdep_plan("/path/to/package")
plan$summary

sampled <- revdep_plan(
  "/path/to/package",
  recursive = TRUE,
  max_recursive = 20
)
sampled$targets
} # }
```
